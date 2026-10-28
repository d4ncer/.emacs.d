;;; agent-shell-pstack-remote-delegate-test.el --- Tests for remote delegates -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for delegate policy, the version-2 job protocol, the laptop
;; identity, a sandboxed delegate's commit message and one commit, the
;; sandbox's protected git metadata, the successful-final marker and the
;; delegate transcript.  Git runs against scratch repositories with a
;; private global config.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-arena-remote)

(defmacro +remote-delegate-test--with-scratch (&rest body)
  "Run BODY with `scratch', a private git config and fresh arena folders."
  (declare (indent 0))
  `(let* ((coding-system-for-write 'utf-8-unix)
          (scratch (file-name-as-directory
                    (file-truename (make-temp-file "remote-delegate-test-" t))))
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
          (+agent-shell-pstack-panel-change-hook nil))
     (with-temp-file config
       (insert "[user]\n\tname = Laptop User\n\temail = laptop@example.com\n"
               "[init]\n\tdefaultBranch = main\n"
               "[commit]\n\tgpgSign = false\n"))
     (unwind-protect (progn ,@body)
       (call-process "chmod" nil nil nil "-R" "u+rwX" scratch)
       (delete-directory scratch t))))

(defun +remote-delegate-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return trimmed output, failing on error."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +remote-delegate-test--write (file text)
  "Write TEXT to FILE, making its folder."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun +remote-delegate-test--repo (scratch)
  "Return a repository in SCRATCH with one commit of two files."
  (let ((repo (expand-file-name "repo/" scratch)))
    (make-directory repo t)
    (+remote-delegate-test--git repo "init" "-q")
    (+remote-delegate-test--write (expand-file-name "keep.txt" repo) "keep\n")
    (+remote-delegate-test--write (expand-file-name "gone.txt" repo) "gone\n")
    (+remote-delegate-test--git repo "add" "-A")
    (+remote-delegate-test--git repo "commit" "-q" "-m" "initial")
    repo))

(defun +remote-delegate-test--wait (start)
  "Call START with a callback and return the arguments it receives."
  (let ((outcome nil) (deadline (+ (float-time) 30)))
    (funcall start (lambda (&rest arguments) (setq outcome (or arguments '(nil)))))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (or outcome (error "Timed out waiting for a callback"))))

(defun +remote-delegate-test--delegate (repo &optional branch-mode)
  "Return a sandboxed delegate's manifest over REPO's HEAD, provisioned.
With BRANCH-MODE it works on its branch in REPO itself."
  (let* ((base (car (+remote-delegate-test--wait
                     (lambda (done)
                       (+agent-shell-arena-git-repository repo done)))))
         (manifest (+agent-shell-arena-git-create
                    base "abc123def456"
                    (+agent-shell-arena-git--delegate
                     :isolation (if branch-mode 'branch 'worktree)
                     :sandbox t
                     :origin (when branch-mode
                               (cons 'branch "refs/heads/main"))))))
    (if branch-mode
        (should (car (+remote-delegate-test--wait
                      (lambda (done)
                        (+agent-shell-arena-git-take manifest done)))))
      (should (car (+remote-delegate-test--wait
                    (lambda (done)
                      (+agent-shell-arena-git-provision
                       manifest "builder" done
                       (+agent-shell-arena-git-delegate-branch manifest)))))))
    manifest))

(defun +remote-delegate-test--workspace (manifest)
  "Return where MANIFEST's delegate works."
  (if-let* ((role (car (+agent-shell-arena-git-manifest-roles manifest))))
      (+agent-shell-arena-git-role-worktree role)
    (+agent-shell-arena-git-manifest-toplevel manifest)))

(defun +remote-delegate-test--finalize (manifest &optional message identity)
  "Finalize MANIFEST's delegate with MESSAGE as IDENTITY; return its record."
  (let ((outcome (+remote-delegate-test--wait
                  (lambda (done)
                    (+agent-shell-arena-git-finalize-delegate
                     manifest
                     (or identity (+agent-shell-arena-git--user
                                   :name "Laptop User"
                                   :email "laptop@example.com"))
                     (or message "[uploads] Add rate limiting\n\nCover it.")
                     done)))))
    (should-not (cadr outcome))
    (car outcome)))

;;;; Policy

(ert-deftest +agent-shell-pstack-remote-delegate-test-options ()
  "Delegates inherit the profile's host unless :host is local; branch
isolation on a host is refused; sandboxing is off unless asked for."
  (let ((host '(coder . "example-workspace")))
    (should (equal `(:host ,host :isolation worktree :sandbox nil)
                   (+agent-shell-arena-git-delegate-options
                    `(:host ,host :host-directory "~/src/big-repo"))))
    (should (equal '(:host nil :isolation worktree :sandbox nil)
                   (+agent-shell-arena-git-delegate-options nil)))
    (should (equal '(:host nil :isolation branch :sandbox t)
                   (+agent-shell-arena-git-delegate-options
                    `(:host ,host :host-directory "~/src/big-repo"
                            :delegate (:host local :isolation branch
                                             :sandbox t)))))
    (should (equal '(:host (local . "/tmp/host-root") :isolation worktree
                           :sandbox t)
                   (+agent-shell-arena-git-delegate-options
                    '(:host (local . "/tmp/host-root") :host-directory "/r"
                            :delegate (:sandbox t)))))
    (let ((err (should-error (+agent-shell-arena-git-delegate-options
                              `(:host ,host :host-directory "~/r"
                                      :delegate (:isolation branch)))
                             :type 'user-error)))
      (should (string-match-p "\\`Remote delegates require :isolation \
worktree; use :delegate (:host local)\nto use :isolation branch on the laptop\\."
                              (cadr err))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-profile-keys ()
  "A :delegate plist refuses unknown and repeated keys and loose values."
  (dolist (case '(((:sandbox yes) . ":sandbox must be t or nil")
                  ((:sandbox t :sandbox nil) . "names :sandbox more than once")
                  ((:isolation worktree :isolation branch)
                   . "names :isolation more than once")
                  ((:hostname x) . "unknown :delegate key :hostname")
                  ((:host (coder . "x")) . ":host must be `local'")
                  ((:isolation detached) . "`worktree' or `branch'")))
    (let ((err (should-error (+agent-shell-arena-git--check-profile
                              "/r/" (list :delegate (car case)))
                             :type 'user-error)))
      (should (string-match-p (regexp-quote (cdr case)) (cadr err)))))
  (should (+agent-shell-arena-git--check-profile
           "/r/" '(:delegate (:host local :sandbox nil :isolation branch)))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-branch-on-host-refused ()
  "Branch isolation on a host is refused before any branch or arena exists."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (+agent-shell-pstack-arena-profiles
            `((,repo :host (coder . "example-workspace")
                     :host-directory "~/src/big-repo"
                     :delegate (:isolation branch))))
           (panel (+agent-shell-pstack-panel--make
                   :id "abc123def456" :shape 'delegate :brief "Do it"
                   :timeout 60 :cwd repo :phase 'preparing
                   :members (list (+agent-shell-pstack-panel--make-member
                                   :role "builder" :agent 'codex
                                   :kind 'delegate :state 'pending)))))
      (puthash "abc123def456" panel +agent-shell-pstack-panel--panels)
      (+agent-shell-pstack-panel--prepare-delegate panel)
      (let ((deadline (+ (float-time) 10)))
        (while (and (not (+agent-shell-pstack-panel-rejection panel))
                    (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (string-match-p "Remote delegates require :isolation worktree"
                              (+agent-shell-pstack-panel-rejection panel)))
      (should-not (and (file-directory-p +agent-shell-pstack-arena-directory)
                       (directory-files +agent-shell-pstack-arena-directory
                                        nil "\\`[^.]")))
      (should (equal "" (+remote-delegate-test--git
                         repo "branch" "--list" "pstack/*"))))))

;;;; Wire

(defconst +remote-delegate-test--identity
  (+agent-shell-arena-git--user :name "Laptop Üser" :email "laptop@example.com"))

(defun +remote-delegate-test--job (&rest changes)
  "Return a delegate job, with CHANGES, a plist of slots, applied."
  (let ((job (+agent-shell-arena-remote--make-job
              :id "abc123def456-12345678" :panel "abc123def456"
              :shape 'delegate :brief "Add rate limiting" :rubric nil
              :timeout 2700
              :members (list (+agent-shell-arena-remote--make-request-member
                              :role "rate-limit" :agent 'codex :kind 'delegate))
              :delegate (+agent-shell-arena-remote--make-delegate-job
                         :sandbox t :identity +remote-delegate-test--identity)
              :prefix "" :base (make-string 40 ?a) :repository "~/src/big-repo"
              :code (make-string 64 ?c) :deadline 2700)))
    (cl-loop for (slot value) on changes by #'cddr
             do (setf (cl-struct-slot-value '+agent-shell-arena-remote-job
                                            (intern (substring (symbol-name slot)
                                                               1))
                                            job)
                      value))
    job))

(ert-deftest +agent-shell-pstack-remote-delegate-test-v1-job-normalized ()
  "A version-1 job's candidates and judge become an arena's members."
  (let* ((json (json-serialize
                `((version . 1) (kind . "job") (id . "abc-12345678")
                  (panel . "abc") (brief . "Do it") (rubric . ["a" "b" "c"])
                  (timeout . 60) (judge_timeout . 30)
                  (candidates . [((role . "opus") (agent . "claude-code")
                                  (focus . :null))
                                 ((role . "codex") (agent . "codex")
                                  (focus . "Short"))])
                  (judge . "codex") (prefix . "") (base . ,(make-string 40 ?a))
                  (prerequisite . :null) (sparse . :null) (profile . :null)
                  (repository . "~/r") (code . ,(make-string 64 ?c))
                  (deadline . 100))))
         (job (+agent-shell-arena-remote-from-json 'job json)))
    (should (eq 'arena (+agent-shell-arena-remote-job-shape job)))
    (should (equal '(("opus" claude-code candidate nil)
                     ("codex" codex candidate "Short")
                     ("judge" codex judge nil))
                   (mapcar (lambda (member)
                             (list (+agent-shell-arena-remote-request-member-role
                                    member)
                                   (+agent-shell-arena-remote-request-member-agent
                                    member)
                                   (+agent-shell-arena-remote-request-member-kind
                                    member)
                                   (+agent-shell-arena-remote-request-member-focus
                                    member)))
                           (+agent-shell-arena-remote-job-members job))))
    (let ((written (+agent-shell-arena-remote-to-json 'job job)))
      (should (string-match-p "\"version\":2" written))
      (should (string-match-p "\"members\":" written))
      (should-not (string-match-p "\"candidates\"\\|\"judge\":" written)))
    (should (equal '("opus" "codex" "judge")
                   (mapcar #'+agent-shell-pstack-panel-member-role
                           (+agent-shell-arena-remote--members job))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-delegate-job-round-trips ()
  "A delegate job carries its sandbox and identity both ways."
  (let* ((job (+remote-delegate-test--job))
         (json (+agent-shell-arena-remote-to-json 'job job))
         (back (+agent-shell-arena-remote-from-json 'job json)))
    (should (equal json (+agent-shell-arena-remote-to-json 'job back)))
    (should (eq 'delegate (+agent-shell-arena-remote-job-shape back)))
    (let ((delegate (+agent-shell-arena-remote-job-delegate back)))
      (should (+agent-shell-arena-remote-delegate-job-sandbox delegate))
      (should (equal "Laptop Üser"
                     (+agent-shell-arena-git-user-name
                      (+agent-shell-arena-remote-delegate-job-identity
                       delegate)))))
    (should (equal '(:host nil :isolation worktree :sandbox t)
                   (+agent-shell-arena-remote--job-options back)))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-job-shape-errors ()
  "Fields that do not fit a job's shape are refused, naming the field."
  (let ((two (list (+agent-shell-arena-remote--make-request-member
                    :role "a" :agent 'codex :kind 'delegate)
                   (+agent-shell-arena-remote--make-request-member
                    :role "b" :agent 'codex :kind 'delegate))))
    (dolist (case
             `(((:members ,two) . "job\\.members must be one member of kind \
delegate")
               ((:delegate nil) . "job\\.delegate must be present")
               ((:delegate ,(+agent-shell-arena-remote--make-delegate-job
                             :sandbox nil
                             :identity (+agent-shell-arena-git--user
                                        :name "Bad <name>" :email "e@x")))
                . "job\\.delegate\\.identity must be a valid git identity")
               ((:shape arena :judge-timeout 30) . "job\\.delegate must be \
absent for an arena")))
      (let ((err (should-error
                  (+agent-shell-arena-remote-from-json
                   'job (+agent-shell-arena-remote-to-json
                         'job (apply #'+remote-delegate-test--job (car case))))
                  :type '+agent-shell-arena-remote-wire)))
        (should (string-match-p (cdr case) (cadr err))))))
  (let ((err (should-error
              (+agent-shell-arena-remote-from-json
               'job (string-replace "\"shape\":\"delegate\""
                                    "\"shape\":\"swarm\""
                                    (+agent-shell-arena-remote-to-json
                                     'job (+remote-delegate-test--job))))
              :type '+agent-shell-arena-remote-wire)))
    (should (string-match-p "job\\.shape must be one of arena, delegate"
                            (cadr err)))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-records-round-trip ()
  "Status, report and record carry a delegate's shape, transcript, cursor
and outcome; their version-1 forms still read as arenas."
  (let ((status (+agent-shell-arena-remote--make-status
                 :id "abc-12345678" :phase 'finalizing :heartbeat 1
                 :shape 'delegate
                 :transcript (+agent-shell-arena-remote--make-transcript
                              :epoch "e1" :first 2 :last 9 :seq 400)
                 :members (list (+agent-shell-arena-remote--make-member
                                 :role "rate-limit" :agent 'codex
                                 :kind 'delegate :state 'done))
                 :results nil))
        (report (+agent-shell-arena-remote--make-report
                 :id "abc-12345678" :base (make-string 40 ?a) :shape 'delegate
                 :delegate (+agent-shell-arena-remote--make-outcome
                            :branch "refs/heads/pstack/delegate/abc"
                            :tip (make-string 40 ?b)
                            :commits (list (+agent-shell-arena-remote--make-commit
                                            :oid (make-string 40 ?b)
                                            :subject "[uploads] Add it"))
                            :workspace "/h/arenas/abc-1/rate-limit" :sandbox t
                            :dirty-paths '("a") :dirty-count 1
                            :finalization 'blocked
                            :retained-reason "No message.")))
        (record (+agent-shell-arena-remote--make
                 :id "abc-12345678" :panel "abc" :phase 'remote :shape 'delegate
                 :host (+agent-shell-arena-remote--make-host
                        :kind 'coder :name "example-workspace")
                 :cursor (+agent-shell-arena-remote--make-cursor
                          :epoch "e1" :segment 3 :offset 120 :seq 40))))
    (dolist (case `((status . ,status) (report . ,report) (record . ,record)))
      (let ((json (+agent-shell-arena-remote-to-json (car case) (cdr case))))
        (should (equal json (+agent-shell-arena-remote-to-json
                             (car case)
                             (+agent-shell-arena-remote-from-json (car case)
                                                                  json))))))
    (let ((old (+agent-shell-arena-remote-from-json
                'status "{\"version\":1,\"kind\":\"status\",\"id\":\"a-1\",\
\"phase\":\"candidates\",\"heartbeat\":1,\"members\":[],\"results\":false}")))
      (should-not (+agent-shell-arena-remote-status-shape old))
      (should-not (+agent-shell-arena-remote-status-transcript old)))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-readiness ()
  "A delegate needs its host's readiness file to list the capability;
an arena does not."
  (let* ((tool (+agent-shell-arena-remote--make-tool :path "/e" :version "31.1"))
         (old (+agent-shell-arena-remote--make-ready :emacs tool))
         (ready (+agent-shell-arena-remote-from-json
                 'ready (json-serialize
                         '((version . 1)
                           (emacs (path . "/e") (version . "31.1"))
                           (capabilities (delegate . 1))))))
         (probe (lambda (ready) (list :toplevel "/r" :ready ready
                                      :ready-file "~/ready.json"))))
    (should-not (+agent-shell-arena-remote--ready-error
                 (funcall probe old) "/r" 'arena))
    (should (string-match-p "does not list the capability \"delegate\":1"
                            (+agent-shell-arena-remote--ready-error
                             (funcall probe old) "/r" 'delegate)))
    (should-not (+agent-shell-arena-remote--ready-error
                 (funcall probe ready) "/r" 'delegate))))

;;;; Identity

(ert-deftest +agent-shell-pstack-remote-delegate-test-identity ()
  "Read the repository's effective identity, through conditional includes;
refuse a missing or malformed one, whatever the environment says."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (read (lambda ()
                   (+remote-delegate-test--wait
                    (lambda (done)
                      (+agent-shell-arena-git-user-identity repo done))))))
      (pcase-let ((`(,user ,error) (funcall read)))
        (should-not error)
        (should (equal "Laptop User" (+agent-shell-arena-git-user-name user))))
      (+remote-delegate-test--write
       (expand-file-name "work.inc" scratch)
       "[user]\n\tname = Ünïcode Name\n\temail = work@example.com\n")
      (with-temp-file config
        (insert (format "[includeIf \"gitdir:%s\"]\n\tpath = %s\n" repo
                        (expand-file-name "work.inc" scratch))))
      (pcase-let ((`(,user ,error) (funcall read)))
        (should-not error)
        (should (equal "Ünïcode Name" (+agent-shell-arena-git-user-name user)))
        (should (equal "work@example.com"
                       (+agent-shell-arena-git-user-email user))))
      (with-temp-file config (insert "[user]\n\tname = Only Name\n"))
      (let ((process-environment (append '("GIT_AUTHOR_EMAIL=env@example.com"
                                           "GIT_COMMITTER_EMAIL=env@example.com")
                                         process-environment)))
        (pcase-let ((`(,user ,error) (funcall read)))
          (should-not user)
          (should (string-match-p "user\\.email nil is missing or invalid"
                                  error))))
      (with-temp-file config
        (insert "[user]\n\tname = Evil <x@y>\n\temail = e@example.com\n"))
      (should (string-match-p "user\\.name \"Evil <x@y>\""
                              (cadr (funcall read)))))))

;;;; Commit message

(ert-deftest +agent-shell-pstack-remote-delegate-test-commit-message ()
  "Only one terminal, well-formed pstack-commit block proposes a message."
  (let ((block (lambda (json) (format "Report.\n\n```pstack-commit\n%s\n```\n"
                                      json))))
    (should (equal "[uploads] Add rate limiting\n\nReject requests.\n\n\
Cover refill."
                   (+agent-shell-pstack-panel--delegate-commit-message
                    (funcall block "{\"version\":1,\"subject\":\"[uploads] Add \
rate limiting\",\"body\":\"Reject requests.\\n\\nCover refill.\"}"))))
    (should (equal "Fix it"
                   (+agent-shell-pstack-panel--delegate-commit-message
                    (funcall block "{\"version\":1,\"subject\":\"Fix it\",\
\"body\":\"\"}"))))
    (dolist (case
             `(("Report without a block." . "no pstack-commit block")
               (,(concat (funcall block "{\"version\":1,\"subject\":\"A\",\
\"body\":\"\"}")
                         (funcall block "{\"version\":1,\"subject\":\"B\",\
\"body\":\"\"}"))
                . "2 pstack-commit blocks")
               (,(concat (funcall block "{\"version\":1,\"subject\":\"A\",\
\"body\":\"\"}")
                         "And one more thing.")
                . "Text follows")
               (,(funcall block "{\"version\":1,\"subject\":\"A\",\"subject\":\
\"B\",\"body\":\"\"}")
                . "names subject more than once")
               (,(funcall block "{\"version\":2,\"subject\":\"A\",\"body\":\"\"}")
                . "version must be 1")
               (,(funcall block "{\"version\":1,\"subject\":\"\",\"body\":\"\"}")
                . "nonempty")
               (,(funcall block "{\"version\":1,\"subject\":\"A\\nB\",\
\"body\":\"\"}")
                . "one line")
               (,(funcall block "{\"version\":1,\"subject\":\"A\",\
\"body\":\"x\\u001b[31m\"}")
                . "no control characters")
               (,(funcall block "{\"version\":1,\"subject\":\"A\",\"body\":\
\"Did it.\\n\\nCo-authored-by: Bot <bot@example.com>\"}")
                . "trailers")
               (,(funcall block "{\"version\":1,\"subject\":\"A\",\"body\":\"\",\
\"author\":\"x\"}")
                . "unknown keys: author")
               (,(funcall block (format "{\"version\":1,\"subject\":\"A\",\
\"body\":\"%s\"}" (make-string 70000 ?x)))
                . "longer than 65536 bytes")
               ("Partial ```pstack-commit\n{\"version\":1" . "no pstack-commit \
block")))
      (let ((err (should-error (+agent-shell-pstack-panel--delegate-commit-message
                                (car case))
                               :type 'user-error)))
        (should (string-match-p (regexp-quote (cdr case)) (cadr err)))))))

;;;; The one commit

(defun +remote-delegate-test--count (repo range)
  "Return how many commits REPO has in RANGE."
  (string-to-number (+remote-delegate-test--git repo "rev-list" "--count"
                                                range)))

(ert-deftest +agent-shell-pstack-remote-delegate-test-finalize-commits-once ()
  "One commit of edits, new and deleted files, as the laptop identity
with the exact message, the base as parent and no trailers; the index
then matches it."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo))
           (worktree (+remote-delegate-test--workspace manifest))
           (base (+agent-shell-arena-git-manifest-base manifest))
           (branch (+agent-shell-arena-git-delegate-branch manifest))
           (message "[uploads] Add rate limiting\n\nReject requests.\n"))
      (+remote-delegate-test--write (expand-file-name "keep.txt" worktree)
                                    "kept and changed\n")
      (+remote-delegate-test--write (expand-file-name "new/file.txt" worktree)
                                    "new\n")
      (delete-file (expand-file-name "gone.txt" worktree))
      (+remote-delegate-test--write (expand-file-name ".agent-shell/t.md"
                                                      worktree)
                                    "transcript\n")
      (with-temp-file config
        (insert "[user]\n\tname = Host User\n\temail = host@example.com\n"))
      (let* ((delegate (+remote-delegate-test--finalize
                        manifest message
                        (+agent-shell-arena-git--user
                         :name "Laptop Üser" :email "laptop@example.com")))
             (finalization (+agent-shell-arena-git-delegate-finalization
                            delegate))
             (oid (plist-get finalization :oid)))
        (should (eq 'committed (plist-get finalization :state)))
        (should (equal oid (+remote-delegate-test--git repo "rev-parse" branch)))
        (should (= 1 (+remote-delegate-test--count repo (concat base ".." branch))))
        (should (equal base (+remote-delegate-test--git repo "rev-parse"
                                                        (concat oid "^"))))
        (should (equal (concat "Laptop Üser <laptop@example.com>\n"
                               "Laptop Üser <laptop@example.com>")
                       (+remote-delegate-test--git
                        repo "log" "-1" "--format=%an <%ae>%n%cn <%ce>" oid)))
        (should (equal (string-trim-right message)
                       (string-trim-right
                        (+remote-delegate-test--git repo "log" "-1"
                                                    "--format=%B" oid))))
        (should (equal "" (+remote-delegate-test--git
                           repo "log" "-1" "--format=%(trailers)" oid)))
        (should (equal '("gone.txt" "keep.txt" "new/file.txt")
                       (split-string (+remote-delegate-test--git
                                      repo "diff" "--name-only" base oid)
                                     "\n" t)))
        (should (equal "?? .agent-shell/"
                       (+remote-delegate-test--git worktree "status"
                                                   "--porcelain")))
        (should (equal "main" (+remote-delegate-test--git
                               repo "branch" "--show-current")))
        (should (equal "" (+remote-delegate-test--git repo "status"
                                                      "--porcelain")))
        ;; Collecting again finds the commit made, and makes no other.
        (+remote-delegate-test--finalize manifest "Another message")
        (should (equal oid (+remote-delegate-test--git repo "rev-parse"
                                                       branch)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-finalize-unchanged ()
  "A delegate that changed nothing gets no commit."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo))
           (base (+agent-shell-arena-git-manifest-base manifest)))
      (should (eq 'empty (plist-get (+agent-shell-arena-git-delegate-finalization
                                     (+remote-delegate-test--finalize manifest))
                                    :state)))
      (should (equal base (+remote-delegate-test--git
                           repo "rev-parse"
                           (+agent-shell-arena-git-delegate-branch manifest)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-finalize-recovers ()
  "However far a crash let it get, rerunning makes exactly one commit,
the same one."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo))
           (worktree (+remote-delegate-test--workspace manifest))
           (base (+agent-shell-arena-git-manifest-base manifest))
           (branch (concat "refs/heads/"
                           (+agent-shell-arena-git-delegate-branch manifest)))
           (delegate (+agent-shell-arena-git-manifest-delegate manifest))
           (crash (lambda (plist &optional tip)
                    (setf (+agent-shell-arena-git-delegate-finalization delegate)
                          plist)
                    (+agent-shell-arena-git-write-manifest manifest)
                    (when tip
                      (+remote-delegate-test--git repo "update-ref" branch tip))
                    (+agent-shell-arena-git-read-manifest
                     (+agent-shell-arena-git-manifest-directory manifest)))))
      (+remote-delegate-test--write (expand-file-name "keep.txt" worktree) "x\n")
      (let* ((done (+agent-shell-arena-git-delegate-finalization
                    (+remote-delegate-test--finalize manifest)))
             (oid (plist-get done :oid))
             (intent (plist-put (copy-sequence done) :state 'prepared)))
        (dolist (state
                 (list
                  ;; After the commit's oid was saved, before the branch moved.
                  (list intent base)
                  ;; After the branch moved, before the index or the record.
                  (list intent nil)
                  ;; After the intent, before the commit was even made.
                  (list (cl-loop for (key value) on intent by #'cddr
                                 unless (eq key :oid) append (list key value))
                        base)))
          (let ((manifest (apply crash state)))
            (+remote-delegate-test--finalize manifest "Not this one")
            (should (equal oid (+remote-delegate-test--git repo "rev-parse"
                                                           branch)))
            (should (= 1 (+remote-delegate-test--count
                          repo (concat base ".." branch))))
            (should (eq 'committed
                        (plist-get (+agent-shell-arena-git-delegate-finalization
                                    (+agent-shell-arena-git-manifest-delegate
                                     manifest))
                                   :state)))))
        ;; A branch that moved elsewhere meanwhile is never overwritten.
        (let* ((other (+remote-delegate-test--git
                       repo "commit-tree" "-p" base "-m" "elsewhere"
                       (concat base "^{tree}")))
               (manifest (funcall crash intent other)))
          (should (eq 'blocked
                      (plist-get (+agent-shell-arena-git-delegate-finalization
                                  (+remote-delegate-test--finalize manifest))
                                 :state)))
          (should (equal other (+remote-delegate-test--git repo "rev-parse"
                                                           branch))))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-finalize-blocks ()
  "A branch that moved before finalizing, or a checkout off its branch,
blocks the commit and keeps the work."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo))
           (worktree (+remote-delegate-test--workspace manifest))
           (branch (+agent-shell-arena-git-delegate-branch manifest)))
      (+remote-delegate-test--write (expand-file-name "keep.txt" worktree) "x\n")
      (+remote-delegate-test--git worktree "-c" "user.name=A" "-c"
                                  "user.email=a@b" "commit" "-q" "-am" "sneaky")
      (+remote-delegate-test--write (expand-file-name "keep.txt" worktree) "y\n")
      (let ((finalization (+agent-shell-arena-git-delegate-finalization
                           (+remote-delegate-test--finalize manifest))))
        (should (eq 'blocked (plist-get finalization :state)))
        (should (string-match-p "moved from its base"
                                (plist-get finalization :reason))))
      (should (equal "sneaky" (+remote-delegate-test--git
                               repo "log" "-1" "--format=%s" branch)))
      (should (equal "M keep.txt" (+remote-delegate-test--git
                                   worktree "status" "--porcelain"))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-finalize-branch-mode ()
  "In the lead's own checkout the commit lands on the delegate's branch and
the checkout is left clean, so it can be returned."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo t))
           (branch (+agent-shell-arena-git-delegate-branch manifest)))
      (+remote-delegate-test--write (expand-file-name "added.txt" repo) "a\n")
      (should (eq 'committed
                  (plist-get (+agent-shell-arena-git-delegate-finalization
                              (+remote-delegate-test--finalize manifest))
                             :state)))
      (should (equal "" (+remote-delegate-test--git repo "status" "--porcelain")))
      (should (equal "[uploads] Add rate limiting"
                     (+remote-delegate-test--git repo "log" "-1" "--format=%s"
                                                 branch)))
      (should (eq 'restored
                  (+agent-shell-arena-git-delegate-restore
                   (car (+remote-delegate-test--wait
                         (lambda (done)
                           (+agent-shell-arena-git-restore manifest done)))))))
      (should (equal "main" (+remote-delegate-test--git
                             repo "branch" "--show-current"))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-manifest-versions ()
  "A version-1 manifest reads with no sandbox and no finalization; the
current one keeps both."
  (+remote-delegate-test--with-scratch
    (let* ((directory (expand-file-name "arenas/abc-1/" scratch))
           (file (expand-file-name "manifest.json" directory)))
      (make-directory directory t)
      (with-temp-file file
        (insert (json-serialize
                 '((version . 1) (id . "abc-1") (git_dir . "/r/.git")
                   (toplevel . "/r/") (prefix . "") (base . "b") (roles . [])
                   (delegate (isolation . "worktree") (origin . :null)
                             (commits . []) (restore . :null)
                             (note . :null))))))
      (let ((delegate (+agent-shell-arena-git-manifest-delegate
                       (+agent-shell-arena-git-read-manifest directory))))
        (should-not (+agent-shell-arena-git-delegate-sandbox delegate))
        (should-not (+agent-shell-arena-git-delegate-finalization delegate)))
      (let ((manifest (+agent-shell-arena-git-read-manifest directory)))
        (setf (+agent-shell-arena-git-delegate-sandbox
               (+agent-shell-arena-git-manifest-delegate manifest))
              t
              (+agent-shell-arena-git-delegate-finalization
               (+agent-shell-arena-git-manifest-delegate manifest))
              (list :state 'blocked :reason "why"))
        (+agent-shell-arena-git-write-manifest manifest))
      (let ((delegate (+agent-shell-arena-git-manifest-delegate
                       (+agent-shell-arena-git-read-manifest directory))))
        (should (+agent-shell-arena-git-delegate-sandbox delegate))
        (should (equal '(:state blocked :reason "why")
                       (+agent-shell-arena-git-delegate-finalization
                        delegate)))))))

;;;; Protected git metadata

(ert-deftest +agent-shell-pstack-remote-delegate-test-protected-paths ()
  "Protected paths are true names that leave the root writable."
  (let* ((root (file-truename (make-temp-file "pa-protect-" t)))
         (outside (file-truename (make-temp-file "pa-common-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".git" root) (insert "gitdir: x\n"))
          (should (equal (list (concat root "/.git") outside)
                         (+agent-shell-sandbox--check-protected-paths
                          root (list (concat root "/.git") outside
                                     (concat outside "/")))))
          (dolist (path (list "relative/.git" root (file-name-directory root)))
            (should-error (+agent-shell-sandbox--check-protected-paths
                           root (list path))
                          :type 'user-error)))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-protection-wins ()
  "Both backends keep protected paths read-only after every allowance."
  (let* ((root (file-truename (make-temp-file "pa-protect-" t)))
         (common (file-truename (make-temp-file "pa-common-" t)))
         (refs (expand-file-name "refs" common)))
    (make-directory refs)
    (with-temp-file (expand-file-name ".git" root) (insert "gitdir: x\n"))
    (unwind-protect
        (progn
          (when (eq system-type 'darwin)
            (let* ((sandbox (+agent-shell-sandbox-create
                             root :allow-writes (list common)
                             :protect (list common)))
                   (profile (+agent-shell-sandbox-profile sandbox))
                   (allow (string-search (format "(subpath \"%s\")" common)
                                         profile))
                   (deny (string-search
                          (format "(deny file-write* (with message \"%s\") \
(subpath \"%s\"))" (+agent-shell-sandbox-tag sandbox) common)
                          profile)))
              (should (and allow deny (< allow deny)))
              (should-not (+agent-shell-drive--file-allowed-p
                           sandbox t (expand-file-name "HEAD" common)))
              (should (+agent-shell-drive--file-allowed-p
                       sandbox t (expand-file-name "work.txt" root)))
              (+agent-shell-sandbox-discard sandbox)))
          (let* ((+agent-shell-sandbox-backend 'bwrap)
                 (sandbox (+agent-shell-sandbox-create
                           root :allow-writes (list common refs)
                           :protect (list common)))
                 (mounts (+agent-shell-sandbox-linux-mounts sandbox))
                 (last (lambda (path)
                         (car (last (seq-filter (lambda (mount)
                                                  (+agent-shell-sandbox--under-p
                                                   path (list (cadr mount))))
                                                mounts))))))
            (should (equal (list 'ro common common) (funcall last common)))
            (should (equal (list 'ro common common) (funcall last refs)))
            (should-not (+agent-shell-drive--file-allowed-p
                         sandbox t (expand-file-name "HEAD" refs)))
            (+agent-shell-sandbox-delete
             (list (+agent-shell-sandbox-tmp sandbox)
                   (+agent-shell-sandbox-config sandbox)))))
      (delete-directory root t)
      (delete-directory common t))))

;;;; Briefs and the successful-final marker

(ert-deftest +agent-shell-pstack-remote-delegate-test-briefs ()
  "A sandboxed delegate is told to leave its work uncommitted and propose
the commit; an unsandboxed one keeps the commit instruction."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (manifest (+remote-delegate-test--delegate repo))
           (member (+agent-shell-pstack-panel--make-member
                    :role "builder" :agent 'codex :kind 'delegate))
           (panel (+agent-shell-pstack-panel--make
                   :id "abc123def456" :shape 'delegate :brief "Do it."
                   :arena manifest :members (list member)
                   :delegate-options '(:host nil :isolation worktree :sandbox t))))
      (let ((brief (+agent-shell-pstack-panel--brief panel member)))
        (should (string-search "Leave your changes uncommitted." brief))
        (should (string-search "```pstack-commit\n{\"version\":1," brief))
        (should-not (string-search "commit your work with git" brief)))
      (setf (+agent-shell-pstack-panel-delegate-options panel)
            '(:host nil :isolation worktree :sandbox nil))
      (let ((brief (+agent-shell-pstack-panel--brief panel member)))
        (should (string-search "commit your work with git" brief))
        (should-not (string-search "pstack-commit" brief))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-final-marker ()
  "Only a report written for a successful turn, unchanged since, counts as
a final response."
  (+remote-delegate-test--with-scratch
    (let* ((member (+agent-shell-pstack-panel--make-member
                    :role "builder" :agent 'codex :kind 'delegate
                    :text "Done.\n```pstack-commit\n{}\n```"))
           (panel (+agent-shell-pstack-panel--make :id "abc" :members
                                                   (list member))))
      (+agent-shell-pstack-panel--write panel member)
      (should-not (+agent-shell-pstack-panel--final-text panel member))
      (+agent-shell-pstack-panel--write panel member "end_turn")
      (should (equal "Done.\n```pstack-commit\n{}\n```"
                     (+agent-shell-pstack-panel--final-text panel member)))
      (with-temp-file (+agent-shell-pstack-panel-member-report member)
        (insert "Changed after the turn."))
      (should-not (+agent-shell-pstack-panel--final-text panel member))
      (setf (+agent-shell-pstack-panel-member-text member) "Partial.")
      (+agent-shell-pstack-panel--write panel member)
      (should-not (file-exists-p (+agent-shell-pstack-panel--report-file
                                  panel member ".final.json"))))))

(provide 'agent-shell-pstack-remote-delegate-test)
;;; agent-shell-pstack-remote-delegate-test.el ends here
