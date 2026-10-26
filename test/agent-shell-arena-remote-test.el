;;; agent-shell-arena-remote-test.el --- Tests for remote pstack arenas -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the wire records, the laptop's phases, prerequisite
;; negotiation, the transport's queue and backoff, the runner's lock, and
;; re-attaching from a record on disk.  Git work runs against scratch
;; repositories; host calls go through a stubbed process layer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-arena-remote)

(defvar agent-shell-inhibit-system-sleep)

(defmacro +arena-remote-test--with-scratch (&rest body)
  "Run BODY with `scratch', a private git config and a fresh arena root."
  (declare (indent 0))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "arena-remote-test-" t))))
          (config (expand-file-name "gitconfig" scratch))
          (process-environment
           (append (list (concat "GIT_CONFIG_GLOBAL=" config)
                         "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" scratch))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (+agent-shell-pstack-arena-profiles nil)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-arena-remote--arenas (make-hash-table :test #'equal))
          (+agent-shell-arena-remote--lines (make-hash-table :test #'equal))
          (+agent-shell-pstack-panel-change-hook nil))
     (with-temp-file config
       (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
               "[init]\n\tdefaultBranch = main\n"
               "[commit]\n\tgpgSign = false\n"))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_ remote)
                  (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
                    (cancel-timer timer)))
                +agent-shell-arena-remote--arenas)
       (delete-directory scratch t))))

(defun +arena-remote-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return trimmed output, failing on error."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +arena-remote-test--commit (repo file text)
  "Write TEXT to FILE in REPO, commit it, and return the commit."
  (with-temp-file (expand-file-name file repo) (insert text))
  (+arena-remote-test--git repo "add" "-A")
  (+arena-remote-test--git repo "commit" "-q" "-m" file)
  (+arena-remote-test--git repo "rev-parse" "HEAD"))

(defun +arena-remote-test--wait (start)
  "Call START with a callback and return the arguments it receives."
  (let ((outcome nil) (deadline (+ (float-time) 30)))
    (funcall start (lambda (&rest arguments) (setq outcome (or arguments '(nil)))))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (or outcome (error "Timed out waiting for a callback"))))

(defun +arena-remote-test--manifest (repo)
  "Snapshot REPO's tree and return a laptop arena over it."
  (let ((base (car (+arena-remote-test--wait
                    (lambda (done)
                      (+agent-shell-arena-git-capture-base repo done))))))
    (+agent-shell-arena-git-create base "panel1")))

(defun +arena-remote-test--job ()
  "Return a job with every field set."
  (+agent-shell-arena-remote--make-job
   :id "abc-12345678" :panel "abc" :brief "Do \"it\"\nnow"
   :rubric '("one" "two" "three") :timeout 60 :judge-timeout 30
   :candidates (list (+agent-shell-arena-remote--make-candidate
                      :role "opus" :agent 'claude-code :focus nil)
                     (+agent-shell-arena-remote--make-candidate
                      :role "codex" :agent 'codex :focus "Short"))
   :judge 'codex :prefix "sub/" :base (make-string 40 ?a)
   :prerequisite (make-string 40 ?b)
   :sparse (+agent-shell-arena-remote--make-sparse
            :cone t :patterns '("/*" "!/*/" "/src/"))
   :profile '(:shared ("vendor") :files (("cfg" . "tmp=@TMP@\n"))
                      :write ("/tmp/x" (regexp . "^/private/tmp/diff\\."))
                      :env ("A=1"))
   :repository "~/src/big-repo" :code (make-string 64 ?c) :deadline 2400))

(defun +arena-remote-test--record (host)
  "Return a laptop record on HOST."
  (+agent-shell-arena-remote--make
   :id "abc-12345678" :panel "abc" :session "s1" :host host :phase 'remote
   :submitted t :seen 1000.5 :restarts 1 :emacs "/usr/bin/emacs"))

;;;; Records

(ert-deftest +agent-shell-arena-remote-test-round-trips ()
  "Every record kind reads back equal to what was written."
  (let ((host (+agent-shell-arena-remote--make-host :kind 'coder
                                                     :name "example-ws"))
        (member (+agent-shell-arena-remote--make-member
                 :role "opus" :agent 'claude-code :kind 'candidate
                 :state 'done :capture 'captured :text "Why" :error nil
                 :commit (make-string 40 ?d) :stat " a | 1 +" :failure nil)))
    (dolist (case
             `((job . ,(+arena-remote-test--job))
               (status . ,(+agent-shell-arena-remote--make-status
                           :id "abc-12345678" :phase 'candidates :pid 42
                           :heartbeat 1.5 :deadline nil :arena "abc-87654321"
                           :members
                           (list (+agent-shell-arena-remote--make-member
                                  :role "opus" :agent 'claude-code
                                  :kind 'candidate :state 'running))
                           :results nil :error nil))
               (report . ,(+agent-shell-arena-remote--make-report
                           :id "abc-12345678" :arena "abc-87654321"
                           :base (make-string 40 ?a) :rejection nil
                           :members (list member)))
               (record . ,(+arena-remote-test--record host))
               (ready . ,(+agent-shell-arena-remote--make-ready
                          :emacs (+agent-shell-arena-remote--make-tool
                                  :path "/nix/bin/emacs" :version "31.1")))))
      (let* ((kind (car case))
             (json (+agent-shell-arena-remote-to-json kind (cdr case)))
             (back (+agent-shell-arena-remote-from-json kind json)))
        (should (equal (+agent-shell-arena-remote-to-json kind back) json))
        (should (eq (not (eq kind 'ready))
                    (and (string-match-p (format "\"kind\":\"%s\"" kind)
                                         json)
                         t)))))
    (let ((back (+agent-shell-arena-remote-from-json
                 'job (+agent-shell-arena-remote-to-json
                       'job (+arena-remote-test--job)))))
      (should (equal "Do \"it\"\nnow" (+agent-shell-arena-remote-job-brief back)))
      (should (eq 'codex (+agent-shell-arena-remote-job-judge back)))
      (should (equal '(:shared ("vendor") :files (("cfg" . "tmp=@TMP@\n"))
                               :write ("/tmp/x" (regexp . "^/private/tmp/diff\\."))
                               :env ("A=1"))
                     (+agent-shell-arena-remote-job-profile back))))))

(ert-deftest +agent-shell-arena-remote-test-validation ()
  "Malformed records are refused at the boundary, naming the field."
  (let* ((good (+agent-shell-arena-remote--encode
                'status (+agent-shell-arena-remote--make-status
                         :id "abc-1" :phase 'queued :heartbeat 1 :results nil)))
         (refuse (lambda (change pattern)
                   (let ((err (should-error
                               (+agent-shell-arena-remote-from-json
                                'status (json-serialize (funcall change
                                                                 (copy-alist good))))
                               :type '+agent-shell-arena-remote-wire)))
                     (should (string-match-p pattern (cadr err)))))))
    (should (+agent-shell-arena-remote-from-json 'status (json-serialize good)))
    (funcall refuse (lambda (json) (setf (alist-get 'id json) "../../etc") json)
             "status\\.id must be a name")
    (funcall refuse (lambda (json) (setf (alist-get 'phase json) "dancing") json)
             "status\\.phase must be one of queued")
    (funcall refuse (lambda (json) (setf (alist-get 'heartbeat json) "soon") json)
             "status\\.heartbeat must be a number")
    (funcall refuse (lambda (json) (setf (alist-get 'version json) 99) json)
             "version 99, but this Emacs reads up to 1")
    (funcall refuse (lambda (json) (setf (alist-get 'kind json) "report") json)
             "status\\.kind must be \"status\"")
    (funcall refuse (lambda (json) (assq-delete-all 'heartbeat json))
             "status\\.heartbeat must be present")
    (funcall refuse (lambda (json)
                      (setf (alist-get 'members json)
                            (vector '((role . "opus") (agent . "Bad Agent")
                                      (kind . "candidate") (state . "done"))))
                      json)
             "status\\.members\\[0\\]\\.agent must be an agent")
    (should-error (+agent-shell-arena-remote-from-json 'status "{not json")
                  :type '+agent-shell-arena-remote-wire)))

(ert-deftest +agent-shell-arena-remote-test-profiles-cross-as-data ()
  "Profile functions run on the laptop against placeholders, and the host
turns placeholders back into functions of its own sandbox."
  (let* ((plain (+agent-shell-arena-remote-plain-profile
                 (list :host '(coder . "x") :host-directory "~/r"
                       :copy '("local.env")
                       :files `(("cfg" . ,(lambda (sandbox)
                                            (format "cache=%s/c\n"
                                                    (+agent-shell-sandbox-tmp
                                                     sandbox)))))
                       :write (list (lambda (sandbox)
                                      (list (+agent-shell-sandbox-config sandbox)
                                            (cons 'regexp
                                                  (concat "^"
                                                          (+agent-shell-sandbox-regexp-quote
                                                           (+agent-shell-sandbox-tmp
                                                            sandbox))
                                                          "/x")))))
                       :env (list (lambda (sandbox)
                                    (list (concat "T="
                                                  (+agent-shell-sandbox-root
                                                   sandbox))))))))
         (host (+agent-shell-arena-remote-host-profile plain))
         (sandbox (+agent-shell-sandbox--make :root "/w/opus" :tmp "/tmp/pa-1.2"
                                              :config "/c/pa-config-9")))
    (should (equal plain '(:copy ("local.env") :files (("cfg" . "cache=@TMP@/c\n"))
                                 :write ("@CONFIG@" (regexp . "^@TMP@/x"))
                                 :env ("T=@ROOT@"))))
    (should (equal "cache=/tmp/pa-1.2/c\n"
                   (funcall (cdr (assoc "cfg" (plist-get host :files))) sandbox)))
    (should (equal '("/c/pa-config-9" (regexp . "^/tmp/pa-1\\.2/x"))
                   (mapcar (lambda (item) (funcall item sandbox))
                           (plist-get host :write))))
    (should (equal '("T=/w/opus") (funcall (plist-get host :env) sandbox)))
    (should (+agent-shell-arena-git--check-profile "/r/" host))))

(defconst +arena-remote-test--ready
  "{
  \"version\": 1,
  \"written_at\": \"2026-10-26T10:29:45.684Z\",
  \"emacs\": {\"path\": \"/nix/store/0wx9-emacs-nox-31.1/bin/emacs\", \"version\": \"31.1\"},
  \"adapters_bin\": \"/home/user/.local/share/pstack-arena/adapters/node_modules/.bin\",
  \"claude_agent_acp\": {\"path\": \"/a/claude-agent-acp/dist/index.js\", \"version\": \"0.95.0\"},
  \"codex_acp\": {\"path\": \"/a/codex-acp/dist/index.js\", \"version\": \"2.2.2\"},
  \"node\": {\"path\": \"/usr/bin/node\", \"version\": \"v24.22.1\"},
  \"bwrap\": {\"path\": \"/usr/bin/bwrap\", \"version\": \"bubblewrap 0.6.1\"},
  \"unshare\": {\"path\": \"/usr/bin/unshare\", \"version\": \"unshare 2.37.2\"},
  \"slirp4netns\": {\"path\": \"/usr/bin/slirp4netns\", \"version\": \"slirp4netns version 1.0.1\"}
}"
  "The remote host's readiness file, as its setup wrote it.")

(ert-deftest +agent-shell-arena-remote-test-ready ()
  "The host's readiness file is read strictly and names what ran."
  (let ((ready (+agent-shell-arena-remote-from-json
                'ready +arena-remote-test--ready)))
    (should (equal "/nix/store/0wx9-emacs-nox-31.1/bin/emacs"
                   (+agent-shell-arena-remote-tool-path
                    (+agent-shell-arena-remote-ready-emacs ready))))
    (should (equal "Emacs 31.1, claude-agent-acp 0.95.0, codex-acp 2.2.2"
                   (+agent-shell-arena-remote--ready-text ready)))
    (should (equal "/home/user/.local/share/pstack-arena/adapters/node_modules/.bin"
                   (+agent-shell-arena-remote-ready-adapters-bin ready)))
    (should (string-match-p
             "its Emacs is 29.4, older than 30"
             (+agent-shell-arena-remote--ready-error
              (list :toplevel "/r"
                    :ready (+agent-shell-arena-remote--make-ready
                            :emacs (+agent-shell-arena-remote--make-tool
                                    :path "/e" :version "29.4")))
              "/r")))
    (should-not (+agent-shell-arena-remote--ready-error
                 (list :toplevel "/r" :ready ready) "/r")))
  (dolist (case '(("\"version\": 1" "\"version\": 2" "version 2, but this Emacs reads up to 1")
                  ("{\"path\": \"/usr/bin/node\", \"version\": \"v24.22.1\"}"
                   "{\"path\": \"/usr/bin/node\"}" "ready\\.node\\.version must be present")
                  ("{\"path\": \"/nix/store/0wx9-emacs-nox-31.1/bin/emacs\", \"version\": \"31.1\"}"
                   "\"/nix/bin/emacs\"" "ready\\.emacs must be an object")))
    (let ((err (should-error
                (+agent-shell-arena-remote-from-json
                 'ready (string-replace (nth 0 case) (nth 1 case)
                                        +arena-remote-test--ready))
                :type '+agent-shell-arena-remote-wire)))
      (should (string-match-p (nth 2 case) (cadr err))))))

(ert-deftest +agent-shell-arena-remote-test-code-files ()
  "The runner's code holds the libraries, the packages and the launcher."
  (let ((names (mapcar #'car (+agent-shell-arena-remote--code-files))))
    (dolist (name '("+agent-shell-arena-remote.el" "+agent-shell-sandbox.el"
                    "+agent-shell-sandbox-launch.sh" "agent-shell.el" "acp.el"
                    "shell-maker.el"))
      (should (member name names)))
    (should-not (seq-some (lambda (name) (string-match-p "autoloads" name))
                          names))))

;;;; Phases

(ert-deftest +agent-shell-arena-remote-test-transitions ()
  "The laptop's phases move only as the table allows, and each move is saved."
  (+arena-remote-test--with-scratch
    (let* ((directory (file-name-as-directory (expand-file-name "a/" scratch)))
           (remote (+agent-shell-arena-remote--make
                    :id "abc-1" :panel "abc" :phase 'preparing
                    :host (+agent-shell-arena-remote--make-host
                           :kind 'local :name scratch)
                    :directory directory))
           (saved (lambda ()
                    (+agent-shell-arena-remote-phase
                     (+agent-shell-arena-remote--read-json
                      (expand-file-name "remote.json" directory) 'record)))))
      (make-directory directory)
      (dolist (phase '(submitting remote unreachable remote fetching remote
                                  fetching finished))
        (+agent-shell-arena-remote--move remote phase)
        (should (eq phase (funcall saved))))
      (should-error (+agent-shell-arena-remote--move remote 'remote))
      (setf (+agent-shell-arena-remote-phase remote) 'preparing)
      (should-error (+agent-shell-arena-remote--move remote 'fetching))
      (should-error (+agent-shell-arena-remote--move remote 'unreachable)))))

;;;; Base transfer

(ert-deftest +agent-shell-arena-remote-test-prerequisites ()
  "The bundle builds on the host's tip when this clone has it, else on this
clone's upstream, else on nothing."
  (+arena-remote-test--with-scratch
    (let* ((origin (expand-file-name "origin.git" scratch))
           (lead (expand-file-name "lead/" scratch))
           (other (expand-file-name "other/" scratch))
           (ladder (lambda (manifest tip)
                     (car (+arena-remote-test--wait
                           (lambda (done)
                             (+agent-shell-arena-remote-prerequisites
                              manifest tip done)))))))
      (+arena-remote-test--git scratch "init" "-q" "--bare" origin)
      (+arena-remote-test--git scratch "clone" "-q" origin lead)
      (let ((first (+arena-remote-test--commit lead "a" "a\n")))
        (+arena-remote-test--git lead "push" "-q" "origin" "main")
        (+arena-remote-test--git lead "remote" "set-head" "origin" "main")
        (+arena-remote-test--git scratch "clone" "-q" origin other)
        (let ((second (+arena-remote-test--commit other "b" "b\n")))
          (+arena-remote-test--git other "push" "-q" "origin" "main")
          (+arena-remote-test--commit lead "c" "c\n")
          (let ((manifest (+arena-remote-test--manifest lead)))
            ;; The host's newer tip is unknown here: build on the upstream.
            (should (equal (list first) (funcall ladder manifest second)))
            (+arena-remote-test--git lead "fetch" "-q")
            (+arena-remote-test--git lead "rebase" "-q" "origin/main")
            (let ((manifest (+arena-remote-test--manifest lead)))
              ;; Both rungs agree once this clone is up to date.
              (should (equal (list second) (funcall ladder manifest second)))
              ;; A stale host's tip comes first, then the newer upstream.
              (should (equal (list first second)
                             (funcall ladder manifest first))))
            (should (equal (list nil)
                           (let ((alone (expand-file-name "alone/" scratch)))
                             (+arena-remote-test--git scratch "init" "-q" alone)
                             (+arena-remote-test--commit alone "x" "x\n")
                             (funcall ladder (+arena-remote-test--manifest alone)
                                      second))))))))))

(ert-deftest +agent-shell-arena-remote-test-bundle ()
  "A bundle holds the base ref and requires its prerequisite; a base that
changes Git LFS files after the prerequisite is refused."
  (+arena-remote-test--with-scratch
    (let* ((repo (expand-file-name "repo/" scratch))
           (bundle (expand-file-name "base.bundle" scratch)))
      (+arena-remote-test--git scratch "init" "-q" repo)
      (with-temp-file (expand-file-name ".gitattributes" repo)
        (insert "*.bin filter=lfs diff=lfs merge=lfs -text\n"))
      (let* ((first (+arena-remote-test--commit repo "a.txt" "a\n"))
             (manifest (progn (+arena-remote-test--commit repo "b.txt" "b\n")
                              (+arena-remote-test--manifest repo)))
             (ref (+agent-shell-arena-remote--base-ref manifest)))
        (+arena-remote-test--git repo "update-ref" ref
                                 (+agent-shell-arena-git-manifest-base manifest))
        (should (equal (list bundle nil)
                       (+arena-remote-test--wait
                        (lambda (done)
                          (+agent-shell-arena-remote-bundle
                           manifest first bundle done)))))
        (should (equal (format "%s %s" (+agent-shell-arena-git-manifest-base
                                        manifest)
                               ref)
                       (+arena-remote-test--git repo "bundle" "list-heads"
                                                bundle)))
        (with-temp-file (expand-file-name "big.bin" repo) (insert "pointer\n"))
        (let* ((manifest (+arena-remote-test--manifest repo))
               (outcome (+arena-remote-test--wait
                         (lambda (done)
                           (+agent-shell-arena-remote-bundle
                            manifest first bundle done)))))
          (should-not (car outcome))
          (should (string-match-p "Git LFS files .*: big\\.bin" (cadr outcome))))))))

;;;; Transport

(defmacro +arena-remote-test--with-spawns (&rest body)
  "Run BODY with the process layer stubbed.
BODY sees `spawned', each (DIRECTORY COMMAND DONE), newest first."
  (declare (indent 0))
  `(let* ((spawned nil)
          (+agent-shell-arena-remote-spawn-function
           (lambda (directory command _input _output _timeout done)
             (push (list directory command done) spawned))))
     ,@body))

(ert-deftest +agent-shell-arena-remote-test-one-call-per-host ()
  "Calls to one host wait their turn; calls to another host do not."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let ((one (+agent-shell-arena-remote--make-host :kind 'ssh :name "one"))
            (two (+agent-shell-arena-remote--make-host :kind 'coder :name "two"))
            (answers nil))
        (dotimes (index 2)
          (+agent-shell-arena-remote--call
           one "echo" (list (number-to-string index))
           :callback (lambda (output error) (push (list output error) answers))))
        (+agent-shell-arena-remote--call
         two "echo" nil :callback (lambda (output error)
                                    (push (list output error) answers)))
        (should (equal 2 (length spawned)))
        (should (equal '("ssh" "coder.two")
                       (mapcar (lambda (call)
                                 (let ((command (nth 1 call)))
                                   (if (member "coder.two" command) "coder.two"
                                     (car command))))
                               (list (nth 1 spawned) (nth 0 spawned)))))
        (funcall (nth 2 (nth 1 spawned)) 0 "first" "")
        (should (equal '(("first" nil)) answers))
        (should (equal 3 (length spawned)))
        (should (string-suffix-p " sh 1" (car (last (nth 1 (car spawned))))))
        (funcall (nth 2 (car spawned)) 'timeout "" "")
        (should (string-match-p "SSH host one did not answer within 120 seconds"
                                (cadr (car answers))))
        (funcall (nth 2 (nth 1 spawned)) 255 "" "Connection refused")
        (should (equal '(nil "Coder workspace two: Connection refused")
                       (car answers)))))))

(ert-deftest +agent-shell-arena-remote-test-spawn-timeout ()
  "A command that outlives its timeout is killed and reported as such."
  (let ((outcome (+arena-remote-test--wait
                  (lambda (done)
                    (+agent-shell-arena-remote--spawn
                     temporary-file-directory '("sleep" "10") nil nil 0.3
                     done)))))
    (should (eq 'timeout (car outcome)))))

(ert-deftest +agent-shell-arena-remote-test-ssh-quoting ()
  "The command an SSH host runs gets every argument intact."
  (let* ((home (file-truename (make-temp-file "arena-remote-home-" t)))
         (args '("two words" "it's" "$HOME" "line\nbreak" ""))
         (command (+agent-shell-arena-remote-remote-command
                   "for arg; do printf '<%s>\\n' \"$arg\"; done; pwd" args)))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory (file-name-as-directory home)))
            (should (eql 0 (call-process "/bin/sh" nil t nil "-c" command))))
          (should (equal (concat "<two words>\n<it's>\n<$HOME>\n<line\nbreak>\n<>\n"
                                 (file-truename
                                  (expand-file-name
                                   +agent-shell-arena-remote-host-root home))
                                 "\n")
                         (buffer-string))))
      (delete-directory home t))))

(ert-deftest +agent-shell-arena-remote-test-backoff ()
  "Polls that keep failing wait twice as long each time, up to a ceiling,
and the lead hears once the host has been silent too long."
  (let ((+agent-shell-arena-remote-poll-interval 30)
        (+agent-shell-arena-remote-max-interval 600))
    (should (equal '(30 60 120 240 480 600 600)
                   (mapcar #'+agent-shell-arena-remote--delay
                           '(0 1 2 3 4 5 9)))))
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let* ((+agent-shell-arena-remote-poll-interval 0.01)
             (+agent-shell-arena-remote-max-interval 0.04)
             (+agent-shell-arena-remote-unreachable-after 3600)
             (remote (+arena-remote-test--record
                      (+agent-shell-arena-remote--make-host
                       :kind 'ssh :name "flaky")))
             (delays nil))
        (setf (+agent-shell-arena-remote-seen remote) (float-time))
        (puthash (+agent-shell-arena-remote-id remote) remote
                 +agent-shell-arena-remote--arenas)
        (advice-add 'run-at-time :before
                    (lambda (time &rest _) (push time delays))
                    '((name . arena-remote-test)))
        (unwind-protect
            (dotimes (_ 4)
              (+agent-shell-arena-remote--poll
               (+agent-shell-arena-remote-id remote))
              (funcall (nth 2 (car spawned)) 255 "" "No route to host"))
          (advice-remove 'run-at-time 'arena-remote-test))
        (should (equal '(0.02 0.04 0.04 0.04)
                       (reverse (seq-filter #'numberp delays))))
        (should (equal "SSH host flaky: No route to host"
                       (+agent-shell-arena-remote-error remote)))
        (setf (+agent-shell-arena-remote-seen remote) (- (float-time) 7200))
        (cl-letf (((symbol-function '+agent-shell-pstack-panel--deliver-to)
                   #'ignore))
          (+agent-shell-arena-remote--poll (+agent-shell-arena-remote-id remote))
          (funcall (nth 2 (car spawned)) 255 "" "No route to host"))
        (should (eq 'unreachable (+agent-shell-arena-remote-phase remote)))))))

(ert-deftest +agent-shell-arena-remote-test-poll-answers ()
  "A poll answer gives liveness, the cancel marker and the status."
  (should (eq 'none (+agent-shell-arena-remote-parse-poll
                     "pstack-arena-poll none\n")))
  (let ((poll (+agent-shell-arena-remote-parse-poll
               (concat "noise\npstack-arena-poll alive=1 cancel=0\n"
                       (+agent-shell-arena-remote-to-json
                        'status (+agent-shell-arena-remote--make-status
                                 :id "abc-1" :phase 'judging :heartbeat 5
                                 :results nil))))))
    (should (plist-get poll :alive))
    (should-not (plist-get poll :cancel))
    (should (eq 'judging (+agent-shell-arena-remote-status-phase
                          (plist-get poll :status)))))
  (should-not (plist-get (+agent-shell-arena-remote-parse-poll
                          "pstack-arena-poll alive=0 cancel=1\n")
                         :status))
  (should-error (+agent-shell-arena-remote-parse-poll "bash: no such file")
                :type '+agent-shell-arena-remote-wire))

;;;; Waking a host

(defun +arena-remote-test--workspaces (name status &optional agent)
  "Return coder list JSON with workspace NAME at STATUS, its agent AGENT."
  (json-serialize
   (vector `((name . ,name)
             (latest_build
              (status . ,status)
              (resources . [((agents . [((name . "main")
                                         (status . ,(or agent "connected")))]))]))))))

(defun +arena-remote-test--probe (boot written)
  "Return a probe answer from a host booted at BOOT, its readiness file
written at WRITTEN, an ISO 8601 string, or missing when WRITTEN is nil."
  (concat "pstack-arena-probe 1\ntoplevel /home/user/work/monorepo\n"
          "tip " (make-string 40 ?e) "\ncode 0\n"
          "ready /home/user/.local/share/pstack-arena/ready.json\n"
          (format "boot %s\n" boot)
          (if written
              (format "{\"version\":1,\"written_at\":%S,\
\"emacs\":{\"path\":\"/nix/bin/emacs\",\"version\":\"31.1\"}}\n" written)
            "")))

(defmacro +arena-remote-test--with-submission (&rest body)
  "Run BODY submitting a job to the Coder workspace box.
BODY sees `remote', `panel', `offered', set once the host was ready, and
`answer', which answers the newest call, waiting for it to be one whose
command holds a string matching a regexp."
  (declare (indent 0))
  `(+arena-remote-test--with-scratch
     (+arena-remote-test--with-spawns
       (let* ((+agent-shell-arena-remote-wake-interval 0.01)
              (remote (+arena-remote-test--record
                       (+agent-shell-arena-remote--make-host
                        :kind 'coder :name "box")))
              (panel (+agent-shell-pstack-panel--make
                      :id "abc" :shape 'arena :phase 'submitting
                      :remote remote))
              (offered nil))
         (setf (+agent-shell-arena-remote-phase remote) 'submitting
               (+agent-shell-arena-remote-directory remote) scratch)
         (puthash "abc" panel +agent-shell-pstack-panel--panels)
         (puthash (+agent-shell-arena-remote-id remote) remote
                  +agent-shell-arena-remote--arenas)
         (cl-letf (((symbol-function '+agent-shell-arena-remote-prerequisites)
                    (lambda (_manifest tip _callback) (setq offered tip))))
           (cl-flet ((answer (regexp status stdout)
                       (let ((deadline (+ (float-time) 10)) call)
                         (while (and (not (setq call
                                                (seq-find
                                                 (lambda (call)
                                                   (seq-some
                                                    (lambda (arg)
                                                      (string-match-p regexp arg))
                                                    (nth 1 call)))
                                                 (list (car spawned)))))
                                     (< (float-time) deadline))
                           (accept-process-output nil 0.01))
                         (should call)
                         (setq spawned (remq call spawned))
                         (funcall (nth 2 call) status stdout ""))))
             (+agent-shell-arena-remote--submit
              remote (+arena-remote-test--job))
             ,@body))))))

(ert-deftest +agent-shell-arena-remote-test-wakes-workspace ()
  "Submitting to a stopped Coder workspace starts it, then waits until it
runs, its agent answers and its setup has written readiness since boot."
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "stopped"))
    (answer "\\`start\\'" 0 "")
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "starting"))
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces
                            "box" "running" "connecting"))
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 255 "")
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    ;; Written at 19:33:35 UTC, before the boot at 19:40:00 UTC.
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             1793043600 "2026-10-26T19:33:35.288Z"))
    (should-not offered)
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             1793043600 "2026-10-26T19:41:44.010Z"))
    (should (equal (make-string 40 ?e) offered))
    (should-not (+agent-shell-pstack-panel-rejection panel))))

(ert-deftest +agent-shell-arena-remote-test-running-workspace ()
  "A running workspace that answers is probed once, and one whose setup
never finished is refused at once."
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             (- (float-time) 7200) "2026-10-26T19:41:44.010Z"))
    (should offered))
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             (- (float-time) 7200) nil))
    (should-not offered)
    (should (string-match-p "no readiness file"
                            (+agent-shell-pstack-panel-rejection panel)))))

(ert-deftest +agent-shell-arena-remote-test-wake-gives-up ()
  "A workspace that never gets ready is refused after the wake timeout,
saying what it waited for."
  (let ((+agent-shell-arena-remote-wake-timeout 0.2))
    (+arena-remote-test--with-submission
      (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "stopped"))
      (answer "\\`start\\'" 0 "")
      (let ((deadline (+ (float-time) 10)))
        (while (and (not (+agent-shell-pstack-panel-rejection panel))
                    (< (float-time) deadline))
          (when-let* ((call (car spawned)))
            (setq spawned (cdr spawned))
            (funcall (nth 2 call) 0
                     (+arena-remote-test--workspaces "box" "starting") ""))
          (accept-process-output nil 0.01)))
      (should-not offered)
      (should (string-match-p
               "Coder workspace box: it was stopped.*started it.*not ready \
within 0\\.2 seconds.*starting"
               (+agent-shell-pstack-panel-rejection panel)))))
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "stopped"))
    (answer "\\`start\\'" 1 "")
    (should (string-match-p "coder start"
                            (+agent-shell-pstack-panel-rejection panel)))))

;;;; Runner

(ert-deftest +agent-shell-arena-remote-test-lock ()
  "One job holds a host repository's lock; a dead holder's lock is taken over."
  (let* ((root (make-temp-file "arena-remote-lock-" t))
         (lock (+agent-shell-arena-remote--lock-file root root))
         (dead (let ((process (start-process "dead" nil "true")))
                 (while (process-live-p process) (accept-process-output nil 0.01))
                 (process-id process))))
    (unwind-protect
        (progn
          (should (+agent-shell-arena-remote-acquire lock "one"))
          (with-temp-file (expand-file-name "owner" lock)
            (insert (format "%d \"other\"\n" (emacs-pid))))
          (cl-letf (((symbol-function 'emacs-pid) (lambda () 1)))
            (should-not (+agent-shell-arena-remote-acquire lock "two")))
          (with-temp-file (expand-file-name "owner" lock)
            (insert (format "%d \"other\"\n" dead)))
          (should (+agent-shell-arena-remote-acquire lock "two"))
          (should (equal (emacs-pid) (+agent-shell-arena-remote--lock-owner lock)))
          (+agent-shell-arena-remote--release lock)
          (should-not (file-exists-p lock)))
      (delete-directory root t))))

(ert-deftest +agent-shell-arena-remote-test-runner-sleep ()
  "A runner's agent shells never ask the host to stay awake.
A host's D-Bus refuses a runner, and every busy turn would log it."
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "arena-remote-runner-" t))))
         (directory (expand-file-name "jobs/abc-12345678/" root))
         (seen (expand-file-name "seen" root)))
    (unwind-protect
        (let ((agent-shell-inhibit-system-sleep t)
              (+agent-shell-pstack-panel-change-hook nil)
              (+agent-shell-arena-remote--runner nil))
          (make-directory directory t)
          (+agent-shell-arena-remote--write-json
           (expand-file-name "job.json" directory) 'job
           (+arena-remote-test--job))
          (+agent-shell-arena-remote--write-json
           (expand-file-name "status.json" directory) 'status
           (+agent-shell-arena-remote--make-status
            :id "abc-12345678" :phase 'finished :heartbeat 1 :results t))
          (with-temp-file (expand-file-name "runner-init.el" root)
            (prin1 `(with-temp-file ,seen
                      (prin1 agent-shell-inhibit-system-sleep (current-buffer)))
                   (current-buffer)))
          (should (eql 0 (+agent-shell-arena-remote-runner-main directory)))
          (should (equal "nil" (with-temp-buffer
                                 (insert-file-contents seen)
                                 (buffer-string)))))
      (advice-remove 'yes-or-no-p #'+agent-shell-arena-remote--refuse)
      (advice-remove 'y-or-n-p #'+agent-shell-arena-remote--refuse)
      (delete-directory root t))))

(ert-deftest +agent-shell-arena-remote-test-refusal-reason ()
  "Why a host refused a candidate reaches its status, the laptop's panel
and, after the runner restarts, the follow-up."
  (+arena-remote-test--with-scratch
    (let* ((reason "Claude Code's access token expires at 2026-10-27 \
00:03:00 AEDT, before the sandbox's deadline; refresh this host's login first")
           (repo (expand-file-name "repo/" scratch))
           (directory (expand-file-name "jobs/abc-12345678/" scratch))
           (members
            (lambda ()
              (list (+agent-shell-pstack-panel--make-member
                     :role "opus" :agent 'claude-code :kind 'candidate
                     :state 'pending)
                    (+agent-shell-pstack-panel--make-member
                     :role "judge" :agent 'codex :kind 'judge :state 'pending))))
           (finished nil))
      (make-directory directory t)
      (+arena-remote-test--git scratch "init" "-q" repo)
      (+arena-remote-test--commit repo "a" "a\n")
      (let* ((manifest (+arena-remote-test--manifest repo))
             (panel (+agent-shell-pstack-panel--make
                     :id "abc" :shape 'arena :phase 'candidates
                     :members (funcall members) :arena manifest))
             (runner (+agent-shell-arena-remote--make-runner
                      :job (+arena-remote-test--job) :directory directory
                      :root scratch :panel panel)))
        (should (car (+arena-remote-test--wait
                      (lambda (done)
                        (+agent-shell-arena-git-provision
                         manifest "opus" done)))))
        (setf (+agent-shell-pstack-panel-member-state
               (car (+agent-shell-pstack-panel-members panel)))
              'error
              (+agent-shell-pstack-panel-member-error
               (car (+agent-shell-pstack-panel-members panel)))
              reason
              (+agent-shell-pstack-panel-member-capture
               (car (+agent-shell-pstack-panel-members panel)))
              'stopping)
        (+agent-shell-arena-remote--runner-status runner)
        (let* ((status (+agent-shell-arena-remote--read-json
                        (expand-file-name "status.json" directory) 'status))
               (laptop (+agent-shell-pstack-panel--make
                        :id "abc" :shape 'arena :phase 'remote
                        :members (funcall members)))
               (remote (+arena-remote-test--record
                        (+agent-shell-arena-remote--make-host
                         :kind 'ssh :name "box"))))
          (should (equal reason (+agent-shell-arena-remote-member-error
                                 (car (+agent-shell-arena-remote-status-members
                                       status)))))
          (setf (+agent-shell-arena-remote-directory remote) scratch)
          (puthash "abc" laptop +agent-shell-pstack-panel--panels)
          (+agent-shell-arena-remote--mirror remote status)
          (should (equal reason (+agent-shell-pstack-panel-member-error
                                 (car (+agent-shell-pstack-panel-members
                                       laptop)))))
          (let ((recovered (+agent-shell-pstack-panel--make
                            :id "abc" :shape 'arena :phase 'preparing
                            :members (funcall members)
                            :sink (lambda (panel) (setq finished panel)))))
            (+agent-shell-arena-remote--recover recovered status)
            (+arena-remote-test--wait
             (lambda (done)
               (let ((timer nil))
                 (setq timer
                       (run-at-time 0 0.02
                                    (lambda ()
                                      (when finished
                                        (cancel-timer timer)
                                        (funcall done))))))))
            (should (string-search
                     reason (+agent-shell-pstack-panel-followup finished)))))))))

;;;; Host logins

(defconst +arena-remote-test--fake-claude
  "#!/bin/sh
echo run >> \"$FAKE_LOG\"
case \"$FAKE_CLAUDE\" in
  fail) echo 'Invalid API key - Please run /login'; exit 1 ;;
  noop) echo OK; exit 0 ;;
esac
sleep \"${FAKE_DELAY:-0}\"
expires=$(( ($(date +%s) + 28800) * 1000 ))
printf '{\"claudeAiOauth\":{\"accessToken\":\"new\",\"refreshToken\":\"r2\",\
\"expiresAt\":%s}}' \"$expires\" > \"$FAKE_CLAUDE_FILE.tmp\"
mv \"$FAKE_CLAUDE_FILE.tmp\" \"$FAKE_CLAUDE_FILE\"
echo OK
"
  "A claude that refreshes its fixture login for 8 hours, or fails, or
does nothing, as FAKE_CLAUDE says, logging each run to FAKE_LOG.")

(defconst +arena-remote-test--fake-codex
  "#!/bin/sh
echo run >> \"$FAKE_LOG\"
[ \"$FAKE_CODEX\" = noop ] && { echo OK; exit 0; }
payload=$(printf '{\"exp\":%d}' $(( $(date +%s) + 800000 )) | base64 | tr -d '=\\n')
printf '{\"tokens\":{\"access_token\":\"h.%s.s\"}}' \"$payload\" \
> \"$CODEX_FIXTURE_HOME/auth.json\"
echo OK
"
  "A codex that refreshes its fixture login, unless FAKE_CODEX is noop.")

(defun +arena-remote-test--jwt (expiry)
  "Return a JSON web token whose exp claim is EXPIRY."
  (concat "h." (string-trim-right
                (base64-encode-string (format "{\"exp\":%d}" expiry) t) "=+")
          ".s"))

(defmacro +arena-remote-test--with-logins (claude codex &rest body)
  "Run BODY with fixture host logins and fake claude and codex on PATH.
The Claude Code token expires CLAUDE seconds from now and Codex's CODEX.
BODY sees `root', a host state folder, `runs', which returns how many
times the fakes ran, and `runner', which makes a runner whose job uses
both agents."
  (declare (indent 2))
  `(let* ((root (file-name-as-directory
                 (file-truename (make-temp-file "arena-remote-login-" t))))
          (bin (expand-file-name "bin/" root))
          (log (expand-file-name "runs" root))
          (+agent-shell-sandbox-claude-credentials
           (expand-file-name "credentials.json" root))
          (+agent-shell-sandbox-codex-home (expand-file-name "codex/" root))
          (+agent-shell-arena-remote-tick 0.1)
          (+agent-shell-arena-remote-refresh-timeout 20)
          (exec-path (cons bin exec-path))
          (process-environment
           (append (list (concat "PATH=" bin path-separator (getenv "PATH"))
                         (concat "FAKE_LOG=" log)
                         (concat "FAKE_CLAUDE_FILE="
                                 +agent-shell-sandbox-claude-credentials)
                         (concat "CODEX_FIXTURE_HOME="
                                 +agent-shell-sandbox-codex-home))
                   process-environment))
          (runs (lambda ()
                  (if (file-exists-p log)
                      (with-temp-buffer
                        (insert-file-contents log)
                        (count-lines (point-min) (point-max)))
                    0)))
          (runner (lambda ()
                    (let ((directory (make-temp-file
                                      (expand-file-name "job-" root) t)))
                      (+agent-shell-arena-remote--make-runner
                       :job (+arena-remote-test--job) :root root
                       :directory (file-name-as-directory directory)
                       :deadline (+ (float-time) 3600))))))
     (ignore runs runner)
     (make-directory bin t)
     (make-directory +agent-shell-sandbox-codex-home t)
     (pcase-dolist (`(,name . ,script)
                    `(("claude" . ,+arena-remote-test--fake-claude)
                      ("codex" . ,+arena-remote-test--fake-codex)))
       (with-temp-file (expand-file-name name bin) (insert script))
       (set-file-modes (expand-file-name name bin) #o755))
     (with-temp-file +agent-shell-sandbox-claude-credentials
       (insert (format "{\"claudeAiOauth\":{\"accessToken\":\"old\",\
\"refreshToken\":\"r1\",\"expiresAt\":%d}}"
                       (* 1000 (round (+ (float-time) ,claude))))))
     (with-temp-file (expand-file-name "auth.json"
                                       +agent-shell-sandbox-codex-home)
       (insert (format "{\"tokens\":{\"access_token\":\"%s\"}}"
                       (+arena-remote-test--jwt
                        (round (+ (float-time) ,codex))))))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun +arena-remote-test--login (runner agent)
  "Return the login RUNNER finds for AGENT."
  (car (+arena-remote-test--wait
        (lambda (done) (+agent-shell-arena-remote--login runner agent done)))))

(ert-deftest +agent-shell-arena-remote-test-login-lasts ()
  "A login that outlasts the deadline and margin is used as it is."
  (+arena-remote-test--with-logins (* 10 3600) (* 200 3600)
    (let ((login (+arena-remote-test--login (funcall runner) 'claude-code)))
      (should (equal 0 (funcall runs)))
      (should-not (+agent-shell-arena-remote-login-refreshed login))
      (should-not (+agent-shell-arena-remote-login-error login))
      (should (< (abs (- (* 10 3600)
                         (+agent-shell-arena-remote-login-remaining login)))
                 5)))))

(ert-deftest +agent-shell-arena-remote-test-login-refreshes ()
  "An expired login is refreshed once, outside the sandbox, then used."
  (+arena-remote-test--with-logins -60 (* 200 3600)
    (let ((runner (funcall runner)))
      (let ((login (+arena-remote-test--login runner 'claude-code)))
        (should (equal 1 (funcall runs)))
        (should (+agent-shell-arena-remote-login-refreshed login))
        (should-not (+agent-shell-arena-remote-login-error login))
        (should (< (abs (- (* 8 3600)
                           (+agent-shell-arena-remote-login-remaining login)))
                   5)))
      (should-not (file-exists-p (+agent-shell-arena-remote--login-lock
                                  runner 'claude-code))))))

(ert-deftest +agent-shell-arena-remote-test-login-refresh-fails ()
  "A refresh that fails refuses the agent with why and what to do."
  (+arena-remote-test--with-logins -60 (* 200 3600)
    (let* ((process-environment (cons "FAKE_CLAUDE=fail" process-environment))
           (login (+arena-remote-test--login (funcall runner) 'claude-code))
           (why (+agent-shell-arena-remote-login-error login)))
      (should (equal 1 (funcall runs)))
      (should-not (+agent-shell-arena-remote-login-refreshed login))
      (should (string-search "claude exited with 1: Invalid API key" why))
      (should (string-search "Log Claude Code in again on the host" why)))))

(ert-deftest +agent-shell-arena-remote-test-login-not-extended ()
  "A refresh that leaves the token short refuses; for Codex, it says no
refresh could be triggered."
  (+arena-remote-test--with-logins (* 200 3600) 60
    (let* ((process-environment (cons "FAKE_CODEX=noop" process-environment))
           (login (+arena-remote-test--login (funcall runner) 'codex))
           (why (+agent-shell-arena-remote-login-error login)))
      (should (equal 1 (funcall runs)))
      (should (string-search "a refresh did not extend it" why))
      (should (string-search "could not be triggered" why)))))

(ert-deftest +agent-shell-arena-remote-test-login-refreshes-once ()
  "Two runners finding one expired login refresh it once between them."
  (+arena-remote-test--with-logins -60 (* 200 3600)
    (let* ((process-environment (cons "FAKE_DELAY=1" process-environment))
           (logins nil)
           (deadline (+ (float-time) 30)))
      (dolist (runner (list (funcall runner) (funcall runner)))
        (+agent-shell-arena-remote--login
         runner 'claude-code (lambda (login) (push login logins))))
      (while (and (< (length logins) 2) (< (float-time) deadline))
        (accept-process-output nil 0.05))
      (should (equal 1 (funcall runs)))
      (should (equal 2 (length logins)))
      (should-not (seq-some #'+agent-shell-arena-remote-login-error logins))
      (should (equal 1 (seq-count #'+agent-shell-arena-remote-login-refreshed
                                  logins))))))

(ert-deftest +agent-shell-arena-remote-test-login-refusal-reaches-status ()
  "A Linux runner checks each agent's login before provisioning, refuses
the members of one it cannot use, and records every login in its status."
  (+arena-remote-test--with-logins -60 (* 200 3600)
    (let* ((process-environment (cons "FAKE_CLAUDE=fail" process-environment))
           (+agent-shell-sandbox-backend 'bwrap)
           (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
           (+agent-shell-pstack-panel-change-hook nil)
           (+agent-shell-pstack-arena-deadline nil)
           (runner (funcall runner))
           (provisioned nil))
      (cl-letf (((symbol-function '+agent-shell-pstack-panel--provision)
                 (lambda (panel _base) (setq provisioned panel)))
                ((symbol-function '+agent-shell-arena-git-base-at) #'ignore))
        (+agent-shell-arena-remote--runner-start runner)
        (let ((deadline (+ (float-time) 30)))
          (while (and (not provisioned) (< (float-time) deadline))
            (accept-process-output nil 0.05))))
      (should provisioned)
      (+agent-shell-arena-remote--runner-status runner)
      (let* ((status (+agent-shell-arena-remote--read-json
                      (+agent-shell-arena-remote--runner-file
                       runner "status.json")
                      'status))
             (members (+agent-shell-arena-remote-status-members status))
             (logins (+agent-shell-arena-remote-status-logins status))
             (remote (+arena-remote-test--record
                      (+agent-shell-arena-remote--make-host
                       :kind 'ssh :name "box"))))
        (should (equal '(claude-code codex)
                       (mapcar #'+agent-shell-arena-remote-login-agent logins)))
        (should (string-search
                 "Log Claude Code in again"
                 (+agent-shell-arena-remote-member-error (car members))))
        (should (eq 'error (+agent-shell-arena-remote-member-state
                            (car members))))
        (should (equal '(pending pending)
                       (mapcar #'+agent-shell-arena-remote-member-state
                               (cdr members))))
        (setf (+agent-shell-arena-remote-directory remote) root)
        (+agent-shell-arena-remote--mirror remote status)
        (should (string-match-p
                 "logins: claude-code refused; codex, 2[0-9][0-9]\\.[0-9] h left"
                 (+agent-shell-arena-remote-describe remote)))))))

;;;; Re-attaching

(ert-deftest +agent-shell-arena-remote-test-reattach ()
  "A record on disk rebuilds its panel, finds its lead by session, and
resumes polling; a delivered arena stays put."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let* ((host (+agent-shell-arena-remote--make-host :kind 'ssh :name "box"))
             (repo (expand-file-name "repo/" scratch))
             (lead (generate-new-buffer " *lead*"))
             (sent nil))
        (+arena-remote-test--git scratch "init" "-q" repo)
        (+arena-remote-test--commit repo "a" "a\n")
        (unwind-protect
            (cl-letf (((symbol-function '+agent-shell-arena-remote--lead-for)
                       (lambda (session) (and (equal session "s1") lead)))
                      ((symbol-function '+agent-shell-drive-watch) #'ignore)
                      ((symbol-function '+agent-shell-drive-send)
                       (lambda (shell text) (push (cons shell text) sent) t)))
              (let* ((manifest (+arena-remote-test--manifest repo))
                     (directory (+agent-shell-arena-git-manifest-directory
                                 manifest))
                     (job (+arena-remote-test--job))
                     (remote (+arena-remote-test--record host)))
                (setf (+agent-shell-arena-remote-id remote)
                      (+agent-shell-arena-git-manifest-id manifest)
                      (+agent-shell-arena-remote-directory remote) directory)
                (+agent-shell-arena-remote--write-json
                 (expand-file-name "job.json" directory) 'job job)
                (+agent-shell-arena-remote--save remote)
                (+agent-shell-arena-remote--write-json
                 (expand-file-name "status.json" directory) 'status
                 (+agent-shell-arena-remote--make-status
                  :id (+agent-shell-arena-remote-id remote) :phase 'candidates
                  :heartbeat 1
                  :members (list (+agent-shell-arena-remote--make-member
                                  :role "opus" :agent 'claude-code
                                  :kind 'candidate :state 'running))
                  :results nil))
                (+agent-shell-arena-remote-reattach-all)
                (let ((panel (+agent-shell-pstack-panel--find "abc")))
                  (should panel)
                  (should (eq 'remote (+agent-shell-pstack-panel-phase panel)))
                  (should (eq lead (+agent-shell-pstack-panel-lead panel)))
                  (should (equal '("opus" "codex" "judge")
                                 (mapcar #'+agent-shell-pstack-panel-member-role
                                         (+agent-shell-pstack-panel-members
                                          panel))))
                  (should (equal '(running pending pending)
                                 (mapcar #'+agent-shell-pstack-panel-member-state
                                         (+agent-shell-pstack-panel-members
                                          panel))))
                  (+arena-remote-test--wait
                   (lambda (done) (run-at-time 0.05 nil done)))
                  (should (equal 1 (length spawned)))
                  (should (member "box" (nth 1 (car spawned))))
                  (should (string-search (+agent-shell-arena-remote-id remote)
                                         (car (last (nth 1 (car spawned))))))
                  ;; Restoring twice changes nothing.
                  (+agent-shell-arena-remote-reattach-all)
                  (should (eq panel (+agent-shell-pstack-panel--find "abc"))))
                (clrhash +agent-shell-arena-remote--arenas)
                (clrhash +agent-shell-pstack-panel--panels)
                (setf (+agent-shell-arena-remote-phase remote) 'finished
                      (+agent-shell-arena-remote-delivered remote) t)
                (+agent-shell-arena-remote--save remote)
                (+agent-shell-arena-remote-reattach-all)
                (should-not (+agent-shell-pstack-panel--find "abc"))))
          (kill-buffer lead))))))

(ert-deftest +agent-shell-arena-remote-test-unreachable-reattach ()
  "An unreachable arena tells its lead how to re-attach, and re-attaching
resumes polling with a fresh follow-up to come."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let* ((repo (expand-file-name "repo/" scratch))
             (lead (generate-new-buffer " *lead*"))
             (sent nil))
        (+arena-remote-test--git scratch "init" "-q" repo)
        (+arena-remote-test--commit repo "a" "a\n")
        (unwind-protect
            (cl-letf (((symbol-function '+agent-shell-drive-send)
                       (lambda (shell text) (push (cons shell text) sent) t))
                      ((symbol-function '+agent-shell-drive-watch) #'ignore))
              (let* ((manifest (+arena-remote-test--manifest repo))
                     (remote (+arena-remote-test--record
                              (+agent-shell-arena-remote--make-host
                               :kind 'ssh :name "box")))
                     (job (+arena-remote-test--job))
                     (panel (+agent-shell-arena-remote--rebuild
                             remote job manifest)))
                (setf (+agent-shell-arena-remote-id remote)
                      (+agent-shell-arena-git-manifest-id manifest)
                      (+agent-shell-arena-remote-directory remote)
                      (+agent-shell-arena-git-manifest-directory manifest)
                      (+agent-shell-pstack-panel-lead panel) lead)
                (puthash "abc" panel +agent-shell-pstack-panel--panels)
                (puthash (+agent-shell-arena-remote-id remote) remote
                         +agent-shell-arena-remote--arenas)
                (+agent-shell-arena-remote--unreachable remote)
                (should (eq 'unreachable (+agent-shell-pstack-panel-phase panel)))
                (should (string-match-p
                         (format "unreachable: no heartbeat since .*\
(\\+agent-shell-pstack-arena-reattach \"%s\")"
                                 (+agent-shell-arena-remote-id remote))
                         (cdar sent)))
                (should (eq lead (caar sent)))
                (+agent-shell-pstack-arena-reattach
                 (+agent-shell-arena-remote-id remote))
                (should (eq 'remote (+agent-shell-arena-remote-phase remote)))
                (should-not (+agent-shell-pstack-panel-followup panel))
                (+arena-remote-test--wait
                 (lambda (done) (run-at-time 0.05 nil done)))
                (should (equal 1 (length spawned)))))
          (kill-buffer lead))))))

(provide 'agent-shell-arena-remote-test)
;;; agent-shell-arena-remote-test.el ends here
