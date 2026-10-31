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
   :shape 'arena
   :members (list (+agent-shell-arena-remote--make-request-member
                   :role "opus" :agent 'claude-code :kind 'candidate)
                  (+agent-shell-arena-remote--make-request-member
                   :role "codex" :agent 'codex :kind 'candidate :focus "Short")
                  (+agent-shell-arena-remote--make-request-member
                   :role "judge" :agent 'codex :kind 'judge))
   :prefix "sub/" :base (make-string 40 ?a)
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
      (should (equal '((judge . codex))
                     (mapcan (lambda (member)
                               (when (eq 'judge
                                         (+agent-shell-arena-remote-request-member-kind
                                          member))
                                 (list (cons 'judge
                                             (+agent-shell-arena-remote-request-member-agent
                                              member)))))
                             (+agent-shell-arena-remote-job-members back))))
      (should (equal '(:shared ("vendor") :files (("cfg" . "tmp=@TMP@\n"))
                               :write ("/tmp/x" (regexp . "^/private/tmp/diff\\."))
                               :env ("A=1"))
                     (+agent-shell-arena-remote-job-profile back))))))

(ert-deftest +agent-shell-arena-remote-test-failure-kind ()
  "Carry a member's provider failure kind, reading an unknown one leniently."
  (let* ((status (+agent-shell-arena-remote--make-status
                  :id "abc-12345678" :phase 'delegating :heartbeat 1.5
                  :members (list (+agent-shell-arena-remote--make-member
                                  :role "builder" :agent 'codex
                                  :kind 'delegate :state 'error
                                  :error "Codex was overloaded: Busy"
                                  :failure-kind 'overloaded))))
         (json (+agent-shell-arena-remote-to-json 'status status))
         (member (car (+agent-shell-arena-remote-status-members
                       (+agent-shell-arena-remote-from-json 'status json)))))
    (should (string-search "\"failure_kind\":\"overloaded\"" json))
    (should (eq 'overloaded
                (+agent-shell-arena-remote-member-failure-kind member)))
    (should (equal '(:kind overloaded)
                   (+agent-shell-arena-remote--failure member)))
    (should-not (+agent-shell-arena-remote--failure
                 (+agent-shell-arena-remote--make-member :role "a")))
    (let ((member (car (+agent-shell-arena-remote-status-members
                        (+agent-shell-arena-remote-from-json
                         'status (string-replace "\"overloaded\"" "\"novel\""
                                                 json))))))
      (should (eq 'provider_error
                  (+agent-shell-arena-remote-member-failure-kind member))))
    (should-error (+agent-shell-arena-remote-from-json
                   'status (string-replace "\"overloaded\"" "7" json))
                  :type '+agent-shell-arena-remote-wire)))

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
             "version 99, but this Emacs reads up to 2")
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

(defconst +arena-remote-test--capabilities
  "\"capabilities\": {\"linux_namespaces\": 1, \"bwrap\": 1, \"slirp4netns\": 1, \
\"systemd_user\": 1, \"git\": 1, \"node_adapters\": 1, \"delegate\": 1, \
\"egress_tshark\": 1}"
  "A readiness file's capabilities, every one proved.")

(defconst +arena-remote-test--ready
  (concat "{
  \"version\": 1,
  \"written_at\": \"2026-10-26T10:29:45.684Z\",
  \"emacs\": {\"path\": \"/nix/store/0wx9-emacs-nox-31.1/bin/emacs\", \"version\": \"31.1\"},
  \"adapters_bin\": \"/home/user/.local/share/pstack-arena/adapters/node_modules/.bin\",
  \"claude_agent_acp\": {\"path\": \"/a/claude-agent-acp/dist/index.js\", \"version\": \"0.95.0\"},
  \"codex_acp\": {\"path\": \"/a/codex-acp/dist/index.js\", \"version\": \"2.2.2\"},
  \"node\": {\"path\": \"/usr/bin/node\", \"version\": \"v24.22.1\"},
  \"bwrap\": {\"path\": \"/usr/bin/bwrap\", \"version\": \"bubblewrap 0.6.1\"},
  \"unshare\": {\"path\": \"/usr/bin/unshare\", \"version\": \"unshare 2.37.2\"},
  \"slirp4netns\": {\"path\": \"/usr/bin/slirp4netns\", \"version\": \"slirp4netns version 1.0.1\"},
  "
          +arena-remote-test--capabilities "\n}")
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

(ert-deftest +agent-shell-arena-remote-test-egress-switch ()
  "A job carries its panel's egress switch to the host's panel, whatever
the host's own setting; a job without the field measures nothing."
  (let* ((job (+arena-remote-test--job))
         (round (lambda (job)
                  (+agent-shell-arena-remote-from-json
                   'job (+agent-shell-arena-remote-to-json 'job job)))))
    (should-not (+agent-shell-arena-remote-job-measure-egress
                 (funcall round job)))
    (setf (+agent-shell-arena-remote-job-measure-egress job) t)
    (should (+agent-shell-arena-remote-job-measure-egress (funcall round job)))
    (should-not (+agent-shell-arena-remote-job-measure-egress
                 (+agent-shell-arena-remote-from-json
                  'job (json-serialize
                        (assq-delete-all
                         'measure_egress
                         (json-parse-string
                          (+agent-shell-arena-remote-to-json 'job job)
                          :object-type 'alist))))))
    (let ((+agent-shell-pstack-arena-measure-egress nil)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal)))
      (should (+agent-shell-pstack-panel-measure-egress
               (+agent-shell-arena-remote--runner-panel-for
                (+agent-shell-arena-remote--make-runner :job job)))))))

(ert-deftest +agent-shell-arena-remote-test-code-files ()
  "The runner's code holds the libraries, the packages and the launcher."
  (let ((names (mapcar #'car (+agent-shell-arena-remote--code-files))))
    (dolist (name '("+agent-shell-arena-remote.el" "+agent-shell-sandbox.el"
                    "+agent-shell-egress.el" "+agent-shell-sandbox-launch.sh"
                    "agent-shell.el" "acp.el" "shell-maker.el"))
      (should (member name names)))
    (should-not (seq-some (lambda (name) (string-match-p "autoloads" name))
                          names))))

;;;; Capabilities

(defun +arena-remote-test--capable (&rest overrides)
  "Return a probe whose readiness file proves every capability.
OVERRIDES is a plist of capability keys, as strings, and values that
replace them; a value of `absent' leaves its key out."
  (let ((capabilities
         (seq-keep (pcase-lambda (`(,_ ,key . ,_))
                     (let ((value (if (plist-member overrides key #'equal)
                                      (plist-get overrides key #'equal)
                                    1)))
                       (unless (eq value 'absent) (cons (intern key) value))))
                   +agent-shell-arena-remote-capability-registry)))
    (list :toplevel "/r" :ready-file "~/ready.json"
          :ready (+agent-shell-arena-remote-from-json
                  'ready (json-serialize
                          `((version . 1)
                            (emacs (path . "/e") (version . "31.1"))
                            ,@(when capabilities
                                `((capabilities . ,capabilities)))))))))

(defun +arena-remote-test--delegate-job (sandbox)
  "Return a delegate job, sandboxed when SANDBOX is non-nil."
  (let ((job (+arena-remote-test--job)))
    (setf (+agent-shell-arena-remote-job-shape job) 'delegate
          (+agent-shell-arena-remote-job-members job)
          (list (+agent-shell-arena-remote--make-request-member
                 :role "delegate" :agent 'codex :kind 'delegate))
          (+agent-shell-arena-remote-job-delegate job)
          (+agent-shell-arena-remote--make-delegate-job :sandbox sandbox))
    job))

(ert-deftest +agent-shell-arena-remote-test-capability-registry ()
  "The readiness file's capabilities are the registry's, in its order."
  (should (equal (mapcar #'car +agent-shell-arena-remote-capability-registry)
                 (mapcar #'car (cdr (cl-struct-slot-info
                                     '+agent-shell-arena-remote-capabilities)))))
  (should (equal (mapcar #'cadr +agent-shell-arena-remote-capability-registry)
                 (mapcar #'cadr (nthcdr 3 (+agent-shell-arena-remote--kind
                                           'capabilities)))))
  (dolist (entry +agent-shell-arena-remote-capability-registry)
    (should (stringp (plist-get (nthcdr 2 entry) :proof)))
    (should (stringp (plist-get (nthcdr 2 entry) :setup)))))

(ert-deftest +agent-shell-arena-remote-test-capability-needs ()
  "A job needs what its shape, sandboxing and host's launcher use."
  (let ((coder (+agent-shell-arena-remote--make-host
                :kind 'coder :name "example-workspace"))
        (local (+agent-shell-arena-remote-parse-host
                '(local . "/tmp/arena-host"))))
    (should (equal '(git node-adapters systemd-user linux-namespaces bwrap
                         slirp4netns)
                   (+agent-shell-arena-remote--needs
                    coder (+arena-remote-test--job))))
    (should (equal '(git node-adapters systemd-user delegate)
                   (+agent-shell-arena-remote--needs
                    coder (+arena-remote-test--delegate-job nil))))
    (should (equal '(git node-adapters systemd-user delegate linux-namespaces
                         bwrap slirp4netns)
                   (+agent-shell-arena-remote--needs
                    coder (+arena-remote-test--delegate-job t))))
    (let ((+agent-shell-sandbox-backend 'sandbox-exec))
      (should (equal '(git node-adapters)
                     (+agent-shell-arena-remote--needs
                      local (+arena-remote-test--job)))))
    (let ((+agent-shell-sandbox-backend 'bwrap))
      (should (equal '(git node-adapters linux-namespaces bwrap slirp4netns)
                     (+agent-shell-arena-remote--needs
                      local (+arena-remote-test--job)))))))

(ert-deftest +agent-shell-arena-remote-test-capability-refusals ()
  "Each capability a job needs and the host lacks refuses it by name, with
what sets it up; one the job does not need refuses nothing."
  (let ((coder (+agent-shell-arena-remote--make-host
                :kind 'coder :name "example-workspace"))
        (arena (+arena-remote-test--job))
        (delegate (+arena-remote-test--delegate-job nil)))
    (should-not (+agent-shell-arena-remote--capability-error
                 (+arena-remote-test--capable) coder arena))
    (pcase-dolist (`(,name ,key . ,properties)
                   +agent-shell-arena-remote-capability-registry)
      (let ((probe (+arena-remote-test--capable key 0)))
        (dolist (job (list arena delegate))
          (let ((error (+agent-shell-arena-remote--capability-error
                        probe coder job)))
            (if (memq name (+agent-shell-arena-remote--needs coder job))
                (progn
                  (should (string-match-p
                           (format "needs: %s\\.  %s: %s\\." (regexp-quote key)
                                   (regexp-quote key)
                                   (regexp-quote
                                    (plist-get properties :setup)))
                           error))
                  (should-not (string-match-p "predates" error)))
              (should-not error))))))
    (should (string-match-p
             "needs: git, bwrap\\.  git: install git\\.  bwrap: install \
bubblewrap"
             (+agent-shell-arena-remote--capability-error
              (+arena-remote-test--capable "git" 0 "bwrap" 0) coder arena)))
    (should-not (+agent-shell-arena-remote--capability-error
                 (+arena-remote-test--capable "bwrap" 0 "slirp4netns" 'absent)
                 coder delegate))
    (should (string-match-p
             "needs: delegate\\..*keep delegates here with :delegate (:host \
local)"
             (+agent-shell-arena-remote--capability-error
              (+arena-remote-test--capable "delegate" 'absent) coder
              delegate)))))

(ert-deftest +agent-shell-arena-remote-test-capabilities-before-declaration ()
  "A readiness file from before hosts declared capabilities is refused for
a job that needs them, asking for setup to run again, never misread."
  (let ((coder (+agent-shell-arena-remote--make-host
                :kind 'coder :name "example-workspace"))
        (old (+arena-remote-test--capable
              "linux_namespaces" 'absent "bwrap" 'absent "slirp4netns" 'absent
              "systemd_user" 'absent "git" 'absent "node_adapters" 'absent
              "egress_tshark" 'absent)))
    (should (equal 1 (+agent-shell-arena-remote--capability
                      (plist-get old :ready) 'delegate)))
    (dolist (job (list (+arena-remote-test--job)
                       (+arena-remote-test--delegate-job nil)))
      (let ((error (+agent-shell-arena-remote--capability-error old coder job)))
        (should (string-match-p "~/ready\\.json does not prove" error))
        (should (string-match-p
                 "does not declare git, node_adapters, systemd_user.* at all, \
so the host's setup predates them; rerun it\\."
                 error))
        (should-not (string-match-p "declare.*delegate" error))))
    (should (string-match-p
             "needs: git, node_adapters, systemd_user, linux_namespaces, \
bwrap, slirp4netns\\."
             (+agent-shell-arena-remote--capability-error
              (list :ready-file "~/ready.json"
                    :ready (+agent-shell-arena-remote--make-ready
                            :emacs (+agent-shell-arena-remote--make-tool
                                    :path "/e" :version "31.1")))
              coder (+arena-remote-test--job))))))

(ert-deftest +agent-shell-arena-remote-test-egress-capability ()
  "A measured job on a host without tshark runs, its egress records
unavailable, rather than being refused."
  (let ((coder (+agent-shell-arena-remote--make-host
                :kind 'coder :name "example-workspace"))
        (job (+arena-remote-test--job))
        (probe (+arena-remote-test--capable "egress_tshark" 0)))
    (should-not (+agent-shell-arena-remote--capability-notes probe coder job))
    (setf (+agent-shell-arena-remote-job-measure-egress job) t)
    (should-not (+agent-shell-arena-remote--capability-error probe coder job))
    (should (string-match-p "does not prove egress_tshark, so egress records \
will be unavailable"
                            (+agent-shell-arena-remote--capability-notes
                             probe coder job)))
    (should-not (+agent-shell-arena-remote--capability-notes
                 (+arena-remote-test--capable) coder job))
    (let ((+agent-shell-sandbox-backend 'sandbox-exec))
      (should-not (+agent-shell-arena-remote--capability-notes
                   probe (+agent-shell-arena-remote-parse-host
                          '(local . "/tmp/arena-host"))
                   job)))))

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

(ert-deftest +agent-shell-arena-remote-test-registered-host ()
  "A registered host validates in profiles and survives the existing wire form."
  (let* ((+agent-shell-arena-remote-host-kinds
          '((fake :exec ignore :wake nil :extend nil :label identity)))
         (host (+agent-shell-arena-remote-parse-host '(fake . "build-host")))
         (record (+arena-remote-test--record host))
         (json (+agent-shell-arena-remote-to-json 'record record)))
    (should (+agent-shell-arena-git-host-p '(fake . "build-host")))
    (should (equal '(:host (fake . "build-host") :host-directory "~/src/repo")
                   (+agent-shell-arena-git--check-profile
                    "/tmp/" '(:host (fake . "build-host")
                                    :host-directory "~/src/repo"))))
    (should (equal "build-host" (+agent-shell-arena-remote--host-label host)))
    (should (string-match-p
             "\"host\":{\"kind\":\"fake\",\"name\":\"build-host\"}" json))
    (should (equal host (+agent-shell-arena-remote-host
                         (+agent-shell-arena-remote-from-json 'record json))))
    (should-not (+agent-shell-arena-git-host-p '(ssh . "build-host")))
    (dolist (value '(nil (fake . "") (fake . "bad name") (fake . 1)
                        (unknown . "build-host")))
      (should-not (+agent-shell-arena-git-host-p value)))))

(ert-deftest +agent-shell-arena-remote-test-unknown-host ()
  "Unknown kinds name exactly the registered kinds in each validation error."
  (let ((+agent-shell-arena-remote-host-kinds '((fake) (other))))
    (dolist (check
             (list (lambda ()
                     (+agent-shell-arena-remote-parse-host
                      '(unknown . "build-host")))
                   (lambda ()
                     (+agent-shell-arena-git--check-profile
                      "/tmp/" '(:host (unknown . "build-host"))))
                   (lambda ()
                     (+agent-shell-arena-remote--decode
                      'host '((kind . "unknown") (name . "build-host"))))))
      (let ((message (error-message-string (should-error (funcall check)))))
        (should (string-match-p "fake" message))
        (should (string-match-p "other" message))
        (should-not (string-match-p "coder\\|ssh\\|local" message))))))

(ert-deftest +agent-shell-arena-remote-test-registered-exec ()
  "The transport runs the command returned by the kind's exec callback."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let* ((received nil)
             (answer nil)
             (+agent-shell-arena-remote-host-kinds
              `((fake :exec ,(lambda (name args)
                               (setq received (cons name args))
                               (append (list "fake-exec" name "--") args))
                      :label ,(lambda (name) (concat "test host " name)))))
             (host (+agent-shell-arena-remote-parse-host
                    '(fake . "build-host"))))
        (+agent-shell-arena-remote--call
         host "printf '%s' \"$1\"" '("two words")
         :callback (lambda (output error) (setq answer (list output error))))
        (should (equal '("build-host" "sh" "-c" "printf '%s' \"$1\""
                                     "sh" "two words") received))
        (should (equal (append '("fake-exec" "build-host" "--") (cdr received))
                       (nth 1 (car spawned))))
        (funcall (nth 2 (car spawned)) 1 "" "unreachable")
        (should (equal '(nil "test host build-host: unreachable") answer))))))

(ert-deftest +agent-shell-arena-remote-test-optional-host-hooks ()
  "Kinds without wake or extend probe directly and finish extensions locally."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let* ((+agent-shell-arena-remote-host-kinds
              '((fake :exec ignore :wake nil :extend nil :label identity)))
             (host (+agent-shell-arena-remote-parse-host '(fake . "build-host")))
             (remote (+arena-remote-test--record host))
             (ready (list :toplevel "/tmp/repo"
                          :ready (+agent-shell-arena-remote-from-json
                                  'ready +arena-remote-test--ready)))
             (probed nil)
             (answer nil))
        (setf (+agent-shell-arena-remote-phase remote) 'submitting)
        (cl-letf (((symbol-function '+agent-shell-arena-remote--probe)
                   (lambda (_remote _job done)
                     (setq probed t)
                     (funcall done ready nil))))
          (+agent-shell-arena-remote--await
           remote (+arena-remote-test--job) (lambda (probe) (setq answer probe))))
        (should probed)
        (should (eq ready answer))
        (should (equal '(nil)
                       (+arena-remote-test--wait
                        (lambda (done)
                          (+agent-shell-arena-remote--extend host 120 done)))))
        (should-not spawned)))))

(ert-deftest +agent-shell-arena-remote-test-registered-host-hooks ()
  "A custom kind supplies its own wake and extension callbacks."
  (+arena-remote-test--with-scratch
    (let* ((events nil)
           (+agent-shell-arena-remote-host-kinds
            `((fake :exec ignore :label identity
                    :wake ,(lambda (name done)
                             (push (list 'wake name) events)
                             (funcall done 'ready nil nil))
                    :extend ,(lambda (name seconds done)
                               (push (list 'extend name seconds) events)
                               (funcall done "extension refused")))))
           (host (+agent-shell-arena-remote-parse-host '(fake . "build-host")))
           (remote (+arena-remote-test--record host))
           (probed nil))
      (setf (+agent-shell-arena-remote-phase remote) 'submitting)
      (cl-letf (((symbol-function '+agent-shell-arena-remote--probe)
                 (lambda (_remote _job done)
                   (funcall done
                            (list :toplevel "/tmp/repo"
                                  :ready (+agent-shell-arena-remote-from-json
                                          'ready +arena-remote-test--ready))
                            nil))))
        (+agent-shell-arena-remote--await
         remote (+arena-remote-test--job) (lambda (_) (setq probed t))))
      (should probed)
      (should (equal '("extension refused")
                     (+arena-remote-test--wait
                      (lambda (done)
                        (+agent-shell-arena-remote--extend host 121 done)))))
      (should (equal '((extend "build-host" 121) (wake "build-host")) events)))))

(ert-deftest +agent-shell-arena-remote-test-default-host-kinds ()
  "Default kinds retain SSH options, local paths, labels and extension units."
  (+arena-remote-test--with-scratch
    (+arena-remote-test--with-spawns
      (let ((script "printf '%s' \"$1\"")
            (args '("two words")))
        (dolist (value '((coder . "example-workspace") (ssh . "build-host")))
          (let* ((host (+agent-shell-arena-remote-parse-host value))
                 (target (if (eq (car value) 'coder)
                             "coder.example-workspace" "build-host"))
                 (command (+agent-shell-arena-remote--command host script args)))
            (should (equal (expand-file-name "~/") (car command)))
            (should (equal
                     (append (list "ssh") +agent-shell-arena-remote-ssh-options
                             (list target "--"
                                   (+agent-shell-arena-remote-remote-command
                                    script args)))
                     (cdr command)))
            (should (equal "systemd" (+agent-shell-arena-remote--launcher host)))
            (should (equal +agent-shell-arena-remote-ready-file
                           (+agent-shell-arena-remote--ready-file host)))))
        (let* ((directory (expand-file-name "host/" scratch))
               (host (+agent-shell-arena-remote-parse-host
                      (cons 'local (directory-file-name directory)))))
          (should (equal directory (+agent-shell-arena-remote-host-name host)))
          (should (equal (cons directory (append (list "/bin/sh" "-c" script "sh")
                                                args))
                         (+agent-shell-arena-remote--command host script args)))
          (should (file-directory-p directory))
          (should (equal "nohup" (+agent-shell-arena-remote--launcher host)))
          (should (equal (expand-file-name "ready.json" directory)
                         (+agent-shell-arena-remote--ready-file host)))
          (should (equal (format "local host %s"
                                 (abbreviate-file-name
                                  (directory-file-name directory)))
                         (+agent-shell-arena-remote--host-label host)))))
      (let ((host (+agent-shell-arena-remote-parse-host
                   '(coder . "example-workspace")))
            (answer nil))
        (+agent-shell-arena-remote--extend
         host 121 (lambda (error) (setq answer (list error))))
        (should (equal (list +agent-shell-arena-remote-coder "schedule" "extend"
                             "example-workspace" "3m")
                       (nth 1 (car spawned))))
        (funcall (nth 2 (car spawned)) 0 "" "")
        (should (equal '(nil) answer))))))

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

(defun +arena-remote-test--probe (boot written &optional capabilities)
  "Return a probe answer from a host booted at BOOT, its readiness file
written at WRITTEN, an ISO 8601 string, or missing when WRITTEN is nil.
CAPABILITIES is the file's capabilities field, by default every one."
  (concat "pstack-arena-probe 1\ntoplevel /home/user/src/big-repo\n"
          "tip " (make-string 40 ?e) "\ncode 0\n"
          "ready /home/user/.local/share/pstack-arena/ready.json\n"
          (format "boot %s\n" boot)
          (if written
              (format "{\"version\":1,\"written_at\":%S,\
\"emacs\":{\"path\":\"/nix/bin/emacs\",\"version\":\"31.1\"},%s}\n" written
                      (or capabilities +arena-remote-test--capabilities))
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

(ert-deftest +agent-shell-arena-remote-test-capability-refused-at-submission ()
  "A host lacking a capability the job needs is refused once it answers,
before any prerequisite, bundle or upload."
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             (- (float-time) 7200) "2026-10-26T19:41:44.010Z"
                             (string-replace "\"bwrap\": 1" "\"bwrap\": 0"
                                             +arena-remote-test--capabilities)))
    (should-not offered)
    (should-not spawned)
    (should (string-match-p "Coder workspace box: .*needs: bwrap\\.  bwrap: \
install bubblewrap"
                            (+agent-shell-pstack-panel-rejection panel))))
  (+arena-remote-test--with-submission
    (answer "\\`list\\'" 0 (+arena-remote-test--workspaces "box" "running"))
    (answer "coder\\.box" 0 (+arena-remote-test--probe
                             (- (float-time) 7200) "2026-10-26T19:41:44.010Z"
                             "\"capabilities\":{\"delegate\":1}"))
    (should-not offered)
    (should (string-match-p "predates them; rerun it"
                            (+agent-shell-pstack-panel-rejection panel)))))

(ert-deftest +agent-shell-arena-remote-test-egress-fails-open ()
  "A measured job goes on to a host without tshark, saying its egress
records will be unavailable."
  (+arena-remote-test--with-scratch
    (let* ((host (+agent-shell-arena-remote--make-host
                  :kind 'coder :name "example-workspace"))
           (remote (+arena-remote-test--record host))
           (job (+arena-remote-test--job))
           (probe (+arena-remote-test--capable "egress_tshark" 0))
           (messages nil)
           (answer nil))
      (setf (+agent-shell-arena-remote-phase remote) 'submitting
            (+agent-shell-arena-remote-job-measure-egress job) t)
      (cl-letf (((symbol-function '+agent-shell-arena-remote--coder-wake)
                 (lambda (_name done) (funcall done 'ready nil nil)))
                ((symbol-function '+agent-shell-arena-remote--probe)
                 (lambda (_remote _job done) (funcall done probe nil)))
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format-message format args) messages))))
        (+agent-shell-arena-remote--await
         remote job (lambda (probe) (setq answer probe))))
      (should (eq probe answer))
      (should (seq-some (lambda (text)
                          (string-match-p "egress records will be unavailable"
                                          text))
                        messages)))))

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

(ert-deftest +agent-shell-arena-remote-test-login-status ()
  "A status carries how each agent's host login stood, with nothing but
its agent, refresh, time left and refusal, and the laptop shows it."
  (+arena-remote-test--with-scratch
    (let* ((status (+agent-shell-arena-remote--make-status
                    :id "abc-12345678" :phase 'preparing :heartbeat 1
                    :logins
                    (list (+agent-shell-sandbox-login-result-create
                           :agent 'claude-code :refreshed t :remaining 28800)
                          (+agent-shell-sandbox-login-result-create
                           :agent 'codex :remaining 60
                           :error "Codex's login on this host is short."))))
           (json (+agent-shell-arena-remote-to-json 'status status))
           (back (+agent-shell-arena-remote-from-json 'status json))
           (remote (+arena-remote-test--record
                    (+agent-shell-arena-remote--make-host
                     :kind 'ssh :name "box"))))
      (should (equal json (+agent-shell-arena-remote-to-json 'status back)))
      (should (seq-every-p #'+agent-shell-sandbox-login-result-p
                           (+agent-shell-arena-remote-status-logins back)))
      (dolist (login (alist-get 'logins (json-parse-string
                                         json :object-type 'alist
                                         :array-type 'list)))
        (should-not (seq-difference (mapcar #'car login)
                                    '(agent refreshed remaining error))))
      (setf (+agent-shell-arena-remote-directory remote) scratch)
      (+agent-shell-arena-remote--mirror remote back)
      (should (string-search "logins: claude-code refreshed, 8.0 h left; \
codex refused"
                             (+agent-shell-arena-remote-describe remote))))))

(defmacro +arena-remote-test--with-admission (agents &rest body)
  "Run BODY with AGENTS as the sandbox registry and arena git stubbed.
BODY sees `started', the members started, and `statuses', the number of
logins each status written held, oldest first."
  (declare (indent 1))
  `(let ((+agent-shell-sandbox-agents ,agents)
         (started nil) (statuses nil))
     (cl-letf* ((write (symbol-function '+agent-shell-arena-remote--write-json))
                ((symbol-function '+agent-shell-arena-remote--write-json)
                 (lambda (file kind record)
                   (when (eq kind 'status)
                     (setq statuses
                           (append statuses
                                   (list (length
                                          (+agent-shell-arena-remote-status-logins
                                           record))))))
                   (funcall write file kind record)))
                ((symbol-function '+agent-shell-arena-git-base-at)
                 (lambda (directory &rest _)
                   (+agent-shell-arena-git--base
                    :git-dir (expand-file-name ".git" directory)
                    :toplevel directory :prefix "" :head "head"
                    :commit (make-string 40 ?a))))
                ((symbol-function '+agent-shell-arena-git-provision)
                 (lambda (_manifest _name callback)
                   (run-at-time 0 nil callback "/worktree" nil)))
                ((symbol-function '+agent-shell-arena-git-clean)
                 (lambda (_manifest callback)
                   (run-at-time 0 nil callback nil nil)))
                ((symbol-function '+agent-shell-drive-start)
                 (lambda (&rest arguments) (push arguments started) nil)))
       ,@body)))

(defun +arena-remote-test--fixture-login (agent file)
  "Return a :login reader of AGENT whose expiry is the number in FILE."
  (lambda ()
    (+agent-shell-sandbox--make-login
     :agent agent :access "fixture-access"
     :expires (string-to-number (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))))

(ert-deftest +agent-shell-arena-remote-test-login-refusal-reaches-status ()
  "A runner checks every agent's host login before any member starts, as
a local arena does.  One refusal refuses every member, and each agent's
result reaches the status as its check ends."
  (+arena-remote-test--with-scratch
    (let* ((expiry (expand-file-name "codex-expiry" scratch))
           (directory (file-name-as-directory
                       (expand-file-name "jobs/abc-12345678" scratch)))
           (runner (+agent-shell-arena-remote--make-runner
                    :job (+arena-remote-test--job) :root scratch
                    :directory directory)))
      (make-directory directory t)
      (with-temp-file expiry (insert (format "%d" (+ (float-time) 86400))))
      (+arena-remote-test--with-admission
          `((claude-code :name "Claude Code"
                         :login ,(lambda ()
                                   (user-error "No fixture Claude Code login")))
            (codex :name "Codex"
                   :login ,(+arena-remote-test--fixture-login 'codex expiry)))
        (add-hook '+agent-shell-pstack-panel-change-hook
                  (lambda () (+agent-shell-arena-remote--runner-status runner)))
        (+agent-shell-arena-remote--runner-start runner)
        (let ((panel (+agent-shell-arena-remote--runner-panel runner)))
          (should (< (abs (- (+agent-shell-pstack-panel-deadline panel)
                             (+ (float-time) 2400)))
                     5))
          (+arena-remote-test--wait
           (lambda (done)
             (let ((timer nil))
               (setq timer
                     (run-at-time
                      0 0.02 (lambda ()
                               (when (+agent-shell-arena-remote--runner-done
                                      runner)
                                 (cancel-timer timer)
                                 (funcall done)))))))))
        (should-not started)
        (should (equal '(0 1 2) (seq-uniq statuses)))
        (let* ((status (+agent-shell-arena-remote--read-json
                        (+agent-shell-arena-remote--runner-file
                         runner "status.json")
                        'status))
               (logins (+agent-shell-arena-remote-status-logins status))
               (remote (+arena-remote-test--record
                        (+agent-shell-arena-remote--make-host
                         :kind 'ssh :name "box"))))
          (should (eq 'finished
                      (+agent-shell-arena-remote-status-phase status)))
          (should (equal '(claude-code codex)
                         (mapcar #'+agent-shell-sandbox-login-result-agent
                                 logins)))
          (dolist (member (+agent-shell-arena-remote-status-members status))
            (should (eq 'error (+agent-shell-arena-remote-member-state member)))
            (should (string-search "No fixture Claude Code login"
                                   (+agent-shell-arena-remote-member-error
                                    member))))
          (setf (+agent-shell-arena-remote-directory remote) scratch)
          (+agent-shell-arena-remote--mirror remote status)
          (should (string-match-p
                   "logins: claude-code refused; codex, 2[34]\\.[0-9] h left"
                   (+agent-shell-arena-remote-describe remote))))))))

(ert-deftest +agent-shell-arena-remote-test-login-refresh-drains-on-exit ()
  "A runner cancelled while it refreshes a host login exits only once the
refresh has finished and released its lock."
  (+arena-remote-test--with-scratch
    (let* ((root scratch)
           (directory (file-name-as-directory
                       (expand-file-name "jobs/abc-12345678/" root)))
           (expiry (expand-file-name "claude-expiry" root))
           (later (expand-file-name "codex-expiry" root))
           (started-file (expand-file-name "refresh-started" root))
           (refreshed (expand-file-name "refreshed" root))
           (+agent-shell-sandbox--login-locks (expand-file-name "login-locks/"
                                                                root))
           (+agent-shell-arena-remote-tick 0.1)
           (+agent-shell-arena-remote--runner nil)
           (watch nil))
      (make-directory directory t)
      (+agent-shell-arena-remote--write-json
       (expand-file-name "job.json" directory) 'job (+arena-remote-test--job))
      (with-temp-file expiry (insert "0"))
      (with-temp-file later (insert (format "%d" (+ (float-time) 86400))))
      (+arena-remote-test--with-admission
          `((claude-code :name "Claude Code"
                         :login ,(+arena-remote-test--fixture-login
                                  'claude-code expiry)
                         :refresh
                         ("/bin/sh" "-c"
                          ,(format "touch %s; sleep 1; echo $(( $(date +%%s) \
+ 86400 )) > %s; touch %s"
                                   (shell-quote-argument started-file)
                                   (shell-quote-argument expiry)
                                   (shell-quote-argument refreshed))))
            (codex :name "Codex"
                   :login ,(+arena-remote-test--fixture-login 'codex later)))
        (setq watch
              (run-at-time 0 0.05
                           (lambda ()
                             (when (file-exists-p started-file)
                               (cancel-timer watch)
                               (with-temp-file (expand-file-name "cancel"
                                                                 directory)
                                 (insert "Stop now."))))))
        (unwind-protect
            (should (eql 0 (+agent-shell-arena-remote-runner-main directory)))
          (cancel-timer watch)
          (advice-remove 'yes-or-no-p #'+agent-shell-arena-remote--refuse)
          (advice-remove 'y-or-n-p #'+agent-shell-arena-remote--refuse))
        (should (file-exists-p refreshed))
        (should-not (file-exists-p (+agent-shell-sandbox--login-lock
                                    'claude-code)))
        (should-not started)
        (let ((status (+agent-shell-arena-remote--read-json
                       (expand-file-name "status.json" directory) 'status)))
          (should (eq 'finished
                      (+agent-shell-arena-remote-status-phase status)))
          (should (equal '(claude-code codex)
                         (mapcar #'+agent-shell-sandbox-login-result-agent
                                 (+agent-shell-arena-remote-status-logins
                                  status))))
          (dolist (member (+agent-shell-arena-remote-status-members status))
            (should (equal "Stop now."
                           (+agent-shell-arena-remote-member-error
                            member)))))))))

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

(ert-deftest +agent-shell-arena-remote-test-runner-extends-deadline ()
  "Move a passed deadline on while a candidate runs a command, then cancel."
  (let* ((shell (generate-new-buffer " *candidate*"))
         (call (list (cons :kind "execute") (cons :status "in_progress")))
         (member (+agent-shell-pstack-panel--make-member
                  :role "a" :agent 'codex :kind 'candidate :state 'running
                  :shell shell))
         (panel (+agent-shell-pstack-panel--make
                 :id "p1" :phase 'candidates :members (list member)
                 :deadline (- (float-time) 1)))
         (runner (+agent-shell-arena-remote--make-runner
                  :directory (make-temp-file "runner" t) :panel panel
                  :limit (+ (float-time) 600)
                  :job (+agent-shell-arena-remote--make-job :deadline 100)))
         (cancelled nil))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-shell-arena-remote--runner-status)
                   #'ignore)
                  ((symbol-function '+agent-shell-pstack-panel-cancel)
                   (lambda (_panel reason &rest _) (setq cancelled reason))))
          (with-current-buffer shell
            (setq-local agent-shell--state
                        (list (cons :tool-calls (list (cons "t1" call))))))
          (+agent-shell-arena-remote--runner-tick runner)
          (should-not cancelled)
          (should (> (+agent-shell-pstack-panel-deadline panel) (float-time)))
          (setf (alist-get :status call) "completed"
                (+agent-shell-pstack-panel-deadline panel) (- (float-time) 1))
          (+agent-shell-arena-remote--runner-tick runner)
          (should (equal "The arena reached its 100-second deadline."
                         cancelled)))
      (kill-buffer shell)
      (delete-directory (+agent-shell-arena-remote--runner-directory runner)
                        t))))

(provide 'agent-shell-arena-remote-test)
;;; agent-shell-arena-remote-test.el ends here
