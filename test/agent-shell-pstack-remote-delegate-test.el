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
    (should (equal `(:host ,host :isolation worktree :sandbox nil :link nil)
                   (+agent-shell-arena-git-delegate-options
                    `(:host ,host :host-directory "~/src/big-repo"))))
    (should (equal '(:host nil :isolation worktree :sandbox nil :link nil)
                   (+agent-shell-arena-git-delegate-options nil)))
    (should (equal '(:host nil :isolation branch :sandbox t :link nil)
                   (+agent-shell-arena-git-delegate-options
                    `(:host ,host :host-directory "~/src/big-repo"
                            :delegate (:host local :isolation branch
                                             :sandbox t)))))
    (should (equal '(:host (local . "/tmp/host-root") :isolation worktree
                           :sandbox t :link nil)
                   (+agent-shell-arena-git-delegate-options
                    '(:host (local . "/tmp/host-root") :host-directory "/r"
                            :delegate (:sandbox t)))))
    (let ((err (should-error (+agent-shell-arena-git-delegate-options
                              `(:host ,host :host-directory "~/r"
                                      :delegate (:isolation branch)))
                             :type 'user-error)))
      (should (string-match-p "\\`Remote delegates require :isolation \
worktree; use :delegate (:host local)\nto use :isolation branch on the laptop\\."
                              (cadr err))))
    (should (equal '(:host nil :isolation worktree :sandbox nil
                           :link ("elpaca"))
                   (+agent-shell-arena-git-delegate-options
                    `(:host ,host :host-directory "~/r"
                            :delegate (:host local :link ("elpaca"))))))
    (let ((err (should-error (+agent-shell-arena-git-delegate-options
                              `(:host ,host :host-directory "~/r"
                                      :delegate (:link ("elpaca"))))
                             :type 'user-error)))
      (should (string-prefix-p "Remote delegates cannot use :delegate :link"
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

;;;; Snapshot base

(ert-deftest +agent-shell-pstack-remote-delegate-test-snapshot-base ()
  "A remote delegate's base snapshots the lead's uncommitted work, and the
base bundle carries it to a repository holding only the lead's HEAD."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (head (+remote-delegate-test--git repo "rev-parse" "HEAD"))
           (+agent-shell-pstack-arena-profiles
            `((,repo :host (coder . "example-workspace")
                     :host-directory "~/src/big-repo")))
           (panel (+agent-shell-pstack-panel--make
                   :id "abc123def456" :shape 'delegate :brief "Do it"
                   :timeout 60 :cwd repo :phase 'preparing
                   :members (list (+agent-shell-pstack-panel--make-member
                                   :role "builder" :agent 'codex
                                   :kind 'delegate :state 'pending))))
           (submitted nil))
      (+remote-delegate-test--write (expand-file-name "keep.txt" repo)
                                    "staged\n")
      (+remote-delegate-test--git repo "add" "keep.txt")
      (+remote-delegate-test--write (expand-file-name "gone.txt" repo)
                                    "unstaged\n")
      (+remote-delegate-test--write (expand-file-name "new.txt" repo) "new\n")
      (puthash "abc123def456" panel +agent-shell-pstack-panel--panels)
      (cl-letf (((symbol-function '+agent-shell-arena-remote--submit)
                 (lambda (remote job) (setq submitted (cons remote job)))))
        (+agent-shell-pstack-panel--prepare-delegate panel)
        (let ((deadline (+ (float-time) 30)))
          (while (and (not submitted)
                      (not (+agent-shell-pstack-panel-rejection panel))
                      (< (float-time) deadline))
            (accept-process-output nil 0.02))))
      (should-not (+agent-shell-pstack-panel-rejection panel))
      (should submitted)
      (let* ((manifest (+agent-shell-pstack-panel-arena panel))
             (snapshot (+agent-shell-arena-git-manifest-base manifest))
             (copy (expand-file-name "copy/" scratch))
             (bundle (expand-file-name "base.bundle" scratch)))
        (should-not (equal head snapshot))
        (should (equal "Laptop User <laptop@example.com>\n\
Laptop User <laptop@example.com>\n\
Snapshot uncommitted work for pstack delegate abc123def456"
                       (+remote-delegate-test--git
                        repo "log" "-1" "--format=%an <%ae>%n%cn <%ce>%n%B"
                        snapshot)))
        (should (equal snapshot (+agent-shell-arena-remote-job-base
                                 (cdr submitted))))
        (should (equal head (+remote-delegate-test--git
                             repo "rev-parse" (concat snapshot "^"))))
        (should (equal head (+agent-shell-arena-git-delegate-head
                             (+agent-shell-arena-git-manifest-delegate
                              (+agent-shell-arena-git-read-manifest
                               (+agent-shell-arena-git-manifest-directory
                                manifest))))))
        (should (string-search
                 (format "commit %s on top of %s" snapshot head)
                 (+agent-shell-pstack-panel--snapshot-text manifest)))
        (should (equal '("keep.txt" "gone.txt" "new.txt")
                       (list (+remote-delegate-test--git
                              repo "diff" "--cached" "--name-only")
                             (+remote-delegate-test--git
                              repo "diff" "--name-only")
                             (+remote-delegate-test--git
                              repo "ls-files" "--others"
                              "--exclude-standard"))))
        (should (equal (list bundle nil)
                       (+remote-delegate-test--wait
                        (lambda (done)
                          (+agent-shell-arena-remote-bundle
                           manifest head bundle done)))))
        (+remote-delegate-test--git scratch "clone" "-q" "--no-checkout"
                                    repo copy)
        (+remote-delegate-test--git
         copy "fetch" "-q" bundle
         (concat (+agent-shell-arena-remote--base-ref manifest) ":refs/base"))
        (should (equal snapshot (+remote-delegate-test--git
                                 copy "rev-parse" "refs/base")))
        (should (equal '("staged" "unstaged" "new")
                       (mapcar (lambda (file)
                                 (+remote-delegate-test--git
                                  copy "show" (concat "refs/base:" file)))
                               '("keep.txt" "gone.txt" "new.txt"))))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-clean-base-is-head ()
  "A remote delegate over a clean tree starts from HEAD itself."
  (+remote-delegate-test--with-scratch
    (let* ((repo (+remote-delegate-test--repo scratch))
           (+agent-shell-pstack-arena-profiles
            `((,repo :host (coder . "example-workspace")
                     :host-directory "~/src/big-repo")))
           (panel (+agent-shell-pstack-panel--make
                   :id "abc123def456" :shape 'delegate :brief "Do it"
                   :timeout 60 :cwd repo :phase 'preparing
                   :members (list (+agent-shell-pstack-panel--make-member
                                   :role "builder" :agent 'codex
                                   :kind 'delegate :state 'pending))))
           (submitted nil))
      (puthash "abc123def456" panel +agent-shell-pstack-panel--panels)
      (cl-letf (((symbol-function '+agent-shell-arena-remote--submit)
                 (lambda (_remote job) (setq submitted job))))
        (+agent-shell-pstack-panel--prepare-delegate panel)
        (let ((deadline (+ (float-time) 30)))
          (while (and (not submitted) (< (float-time) deadline))
            (accept-process-output nil 0.02))))
      (let ((manifest (+agent-shell-pstack-panel-arena panel)))
        (should (equal (+remote-delegate-test--git repo "rev-parse" "HEAD")
                       (+agent-shell-arena-remote-job-base submitted)))
        (should-not (+agent-shell-arena-git-delegate-head
                     (+agent-shell-arena-git-manifest-delegate manifest)))
        (should-not (+agent-shell-pstack-panel--snapshot-text manifest))
        (should (string-search "To merge: git merge --ff-only"
                               (+agent-shell-pstack-panel--review-text
                                manifest)))))))

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

;;;; Transcripts

(defmacro +remote-delegate-test--with-transcript (&rest body)
  "Run BODY with `job', a fresh job folder, `recorder' writing its
transcript, small bounds, and `remote', a laptop record caching it."
  (declare (indent 0))
  `(+remote-delegate-test--with-scratch
     (let* ((job (expand-file-name "jobs/abc-1/" scratch))
            (+agent-shell-arena-remote-transcript-segment-bytes 400)
            (+agent-shell-arena-remote-transcript-segments 3)
            (+agent-shell-arena-remote-transcript-record-bytes 120)
            (+agent-shell-arena-remote-transcript-page-bytes 300)
            (+agent-shell-arena-remote-transcript-cache-bytes 2000)
            (recorder (+agent-shell-arena-remote--recorder-open
                       (expand-file-name "transcript/" job)))
            (laptop (expand-file-name "laptop/" scratch))
            (remote (+agent-shell-arena-remote--make
                     :id "abc-1" :panel "abc" :phase 'remote :shape 'delegate
                     :directory laptop
                     :host (+agent-shell-arena-remote--make-host
                            :kind 'local :name scratch))))
       (make-directory laptop t)
       (puthash "abc-1" remote +agent-shell-arena-remote--arenas)
       (unwind-protect (progn ,@body)
         (when-let* ((buffer (+agent-shell-arena-remote--transcript-live
                              remote)))
           (kill-buffer buffer))))))

(defun +remote-delegate-test--page (job remote)
  "Fetch the next transcript page of JOB for REMOTE; return whether more."
  (let* ((cursor (+agent-shell-arena-remote-cursor remote))
         (envelope (json-parse-string
                    (json-serialize
                     (+agent-shell-arena-remote--poll-page
                      job (and cursor
                               (list (+agent-shell-arena-remote-cursor-epoch
                                      cursor)
                                     (+agent-shell-arena-remote-cursor-segment
                                      cursor)
                                     (+agent-shell-arena-remote-cursor-offset
                                      cursor)
                                     (+agent-shell-arena-remote-cursor-seq
                                      cursor)))))
                    :object-type 'alist :array-type 'list :null-object nil
                    :false-object nil)))
    (+agent-shell-arena-remote--cache-transcript remote
                                                 (alist-get 'tail envelope))))

(defun +remote-delegate-test--drain (job remote)
  "Fetch JOB's transcript pages for REMOTE until none is left."
  (let ((pages 0))
    (while (progn (cl-incf pages) (+remote-delegate-test--page job remote))
      (should (< pages 100)))
    pages))

(ert-deftest +agent-shell-pstack-remote-delegate-test-recorder-bounds ()
  "Records stay within their byte bound, split between characters, never
span segments, and only the newest segments stay."
  (+remote-delegate-test--with-transcript
    (let ((directory (expand-file-name "transcript/" job))
          (text (concat (make-string 100 ?é) "\"quoted\"\n" (make-string 50 ?x))))
      (dotimes (_ 12)
        (+agent-shell-arena-remote--record recorder "assistant" text))
      (let ((segments (+agent-shell-arena-remote--segments directory))
            (seqs nil) (joined ""))
        (should (= 3 (length segments)))
        (dolist (segment segments)
          (let ((bytes (+agent-shell-arena-remote--read-bytes
                        (+agent-shell-arena-remote--segment-file directory
                                                                 segment))))
            (should (<= (length bytes) 400))
            (should (string-suffix-p "\n" bytes))
            (dolist (line (split-string (decode-coding-string bytes 'utf-8)
                                        "\n" t))
              (should (<= (1+ (string-bytes (encode-coding-string line 'utf-8)))
                          120))
              (let ((record (json-parse-string line :object-type 'alist)))
                (push (alist-get 'seq record) seqs)
                (setq joined (concat joined (alist-get 'text record)))))))
        (should (equal (reverse seqs) (number-sequence (car (last seqs))
                                                       (car seqs))))
        (should (string-search "\"quoted\"\n" joined))
        (should (string-suffix-p (make-string 50 ?x) joined))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-recorder-resumes ()
  "A restarted runner drops a torn last line and numbers on from the last
whole record, in the same epoch."
  (+remote-delegate-test--with-transcript
    (let ((directory (expand-file-name "transcript/" job)))
      (+agent-shell-arena-remote--record recorder "notice" "one")
      (+agent-shell-arena-remote--record recorder "notice" "two")
      (let ((file (+agent-shell-arena-remote--segment-file
                   directory (+agent-shell-arena-remote--recorder-segment
                              recorder))))
        (write-region "{\"seq\":3,\"ti" nil file t 'silent)
        (let ((again (+agent-shell-arena-remote--recorder-open directory)))
          (should (equal (+agent-shell-arena-remote--recorder-epoch recorder)
                         (+agent-shell-arena-remote--recorder-epoch again)))
          (should (= 2 (+agent-shell-arena-remote--recorder-seq again)))
          (+agent-shell-arena-remote--record again "notice" "three")
          (should (equal '(1 2 3)
                         (mapcar (lambda (line)
                                   (alist-get 'seq (json-parse-string
                                                    line :object-type 'alist)))
                                 (split-string
                                  (+agent-shell-arena-remote--read-bytes file)
                                  "\n" t)))))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-projection ()
  "The transcript keeps agent text, tool titles and lifecycle notices, and
never a final text, permission or tool argument."
  (+remote-delegate-test--with-transcript
    (let ((runner (+agent-shell-arena-remote--make-runner :transcript recorder)))
      (dolist (event '((:kind progress :part assistant :text "Reading ")
                       (:kind progress :part assistant :text "the handler.")
                       (:kind progress :part tool :text "Read upload.el"
                              :status "completed")
                       (:kind permission :text "secret-token")
                       (:kind turn-complete :stop-reason "end_turn"
                              :text "FINAL REPORT")))
        (+agent-shell-arena-remote--record-transcript runner event))
      (+remote-delegate-test--drain job remote)
      (should (equal '(("assistant" . "Reading the handler.")
                       ("tool" . "Read upload.el (completed)")
                       ("notice" . "The turn ended: end_turn."))
                     (mapcar (lambda (record)
                               (cons (alist-get 'kind record)
                                     (alist-get 'text record)))
                             (+agent-shell-arena-remote--cached-records
                              remote)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-pages ()
  "Pages bring whole records in order within their bound, across
segments, leave a partial last line for later, replay without
duplicates, and mark what rotated away as a gap."
  (+remote-delegate-test--with-transcript
    (let ((directory (expand-file-name "transcript/" job))
          (seqs (lambda ()
                  (delq 0 (mapcar (lambda (record) (alist-get 'seq record))
                                  (+agent-shell-arena-remote--cached-records
                                   remote))))))
      (dotimes (index 6)
        (+agent-shell-arena-remote--record recorder "notice"
                                           (format "record %d %s" index
                                                   (make-string 40 ?n))))
      (should (> (+remote-delegate-test--drain job remote) 1))
      (should (equal (number-sequence 1 6) (funcall seqs)))
      ;; A torn line waits; a replayed page adds nothing.
      (let ((file (+agent-shell-arena-remote--segment-file
                   directory (+agent-shell-arena-remote--recorder-segment
                              recorder)))
            (cursor (+agent-shell-arena-remote-cursor remote)))
        (write-region "{\"seq\":7," nil file t 'silent)
        (+remote-delegate-test--drain job remote)
        (should (equal (number-sequence 1 6) (funcall seqs)))
        (setf (+agent-shell-arena-remote-cursor remote)
              (+agent-shell-arena-remote--make-cursor
               :epoch (+agent-shell-arena-remote-cursor-epoch cursor)
               :segment (car (+agent-shell-arena-remote--segments directory))
               :offset 0 :seq 6))
        (+remote-delegate-test--drain job remote)
        (should (equal (number-sequence 1 6) (funcall seqs))))
      ;; Records that rotated away before a poll leave a gap notice.
      (setq recorder (+agent-shell-arena-remote--recorder-open directory))
      (dotimes (index 20)
        (+agent-shell-arena-remote--record recorder "notice"
                                           (format "later %d %s" index
                                                   (make-string 40 ?l))))
      (+remote-delegate-test--drain job remote)
      (let ((records (+agent-shell-arena-remote--cached-records remote)))
        (should (seq-find (lambda (record)
                            (string-prefix-p "Part of the transcript is missing"
                                             (alist-get 'text record)))
                          records))
        (should (equal (car (last (funcall seqs)))
                       (+agent-shell-arena-remote--recorder-seq recorder)))
        (let ((seqs (funcall seqs)))
          (should (equal seqs (sort (seq-uniq seqs) #'<))))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-malformed-page ()
  "A malformed page is refused before the cache or cursor change."
  (+remote-delegate-test--with-transcript
    (dolist (page '(((epoch . "../x") (next (segment . 1) (offset . 0))
                     (records))
                    ((epoch . "e1") (next (segment . 1) (offset . 0))
                     (records ((seq . 2) (time . 1) (kind . "notice")
                               (text . "b"))
                              ((seq . 1) (time . 1) (kind . "notice")
                               (text . "a"))))
                    ((epoch . "e1") (next (segment . 1) (offset . 0))
                     (records ((seq . 1) (time . 1) (kind . "shell")
                               (text . "rm -rf"))))))
      (should-error (+agent-shell-arena-remote--cache-transcript remote page)
                    :type '+agent-shell-arena-remote-wire)
      (should-not (+agent-shell-arena-remote-cursor remote))
      (should-not (file-exists-p (+agent-shell-arena-remote--cache-file
                                  remote))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-cache-bound ()
  "The laptop's cache keeps whole newest records within its bound and
says it dropped the rest."
  (+remote-delegate-test--with-transcript
    (dotimes (index 60)
      (+agent-shell-arena-remote--append-cache
       remote (list `((seq . ,(1+ index)) (time . 1) (kind . "notice")
                      (text . ,(make-string 50 ?c))))))
    (let ((records (+agent-shell-arena-remote--cached-records remote)))
      (should (<= (file-attribute-size
                   (file-attributes (+agent-shell-arena-remote--cache-file
                                     remote)))
                  2000))
      (should (string-prefix-p "Older transcript was dropped"
                               (alist-get 'text (car records))))
      (should (= 60 (alist-get 'seq (car (last records))))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-buffer ()
  "The transcript buffer shows records literally, follows a window at its
end, keeps a reader's place elsewhere, and starts no process."
  (+remote-delegate-test--with-transcript
    (+agent-shell-arena-remote--append-cache
     remote (list '((seq . 1) (time . 1) (kind . "assistant")
                    (text . "Plain \e[31mred\e[0m -*- eval: (kill-emacs) -*-"))
                  '((seq . 2) (time . 2) (kind . "tool") (text . "Read a"))))
    (let* ((processes (length (process-list)))
           (scheduled nil)
           (buffer (cl-letf (((symbol-function
                               '+agent-shell-arena-remote--schedule)
                              (lambda (&rest _) (setq scheduled t))))
                     (+agent-shell-arena-remote-transcript-buffer remote))))
      (should scheduled)
      (should (= processes (length (process-list))))
      (with-current-buffer buffer
        (should (derived-mode-p '+agent-shell-arena-remote-transcript-mode))
        (should buffer-read-only)
        (should (string-search "\e[31mred" (buffer-string)))
        (should-not (local-variable-p 'eval))
        (save-window-excursion
          (switch-to-buffer buffer)
          (goto-char (point-min))
          (+agent-shell-arena-remote--append-cache
           remote (list '((seq . 3) (time . 3) (kind . "notice")
                          (text . "third"))))
          (+agent-shell-arena-remote--show-transcript remote)
          (should (= (point) (point-min)))
          (should (string-search "third" (buffer-string)))
          (goto-char (point-max))
          (+agent-shell-arena-remote--append-cache
           remote (list '((seq . 4) (time . 4) (kind . "notice")
                          (text . "fourth"))))
          (+agent-shell-arena-remote--show-transcript remote)
          (should (= (window-point) (point-max)))
          (should (= 1 (how-many "third" (point-min) (point-max)))))
        (should (string-match-p "local host .* | delegate | remote | \
read-only | last poll never"
                                (+agent-shell-arena-remote--transcript-header)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-poll-envelope ()
  "The version-2 poll answer parses with its status and tail, and a
version-1 answer still parses."
  (let* ((status (+agent-shell-arena-remote--make-status
                  :id "abc-1" :phase 'delegating :heartbeat 1 :shape 'delegate
                  :results nil))
         (output (concat "pstack-arena-poll 2\n"
                         (json-serialize
                          `((version . 2) (alive . t) (cancel . :false)
                            (status . ,(+agent-shell-arena-remote--encode
                                        'status status))
                            (tail (epoch . "e1") (records . [])
                                  (next (segment . 1) (offset . 0)))))))
         (poll (+agent-shell-arena-remote-parse-poll output)))
    (should (plist-get poll :alive))
    (should-not (plist-get poll :cancel))
    (should (eq 'delegating (+agent-shell-arena-remote-status-phase
                             (plist-get poll :status))))
    (should (equal "e1" (alist-get 'epoch (plist-get poll :tail))))
    (should (plist-get (+agent-shell-arena-remote-parse-poll
                        "pstack-arena-poll alive=1 cancel=0\n")
                       :alive))
    (should-error (+agent-shell-arena-remote-parse-poll
                   "pstack-arena-poll 2\n{not json")
                  :type '+agent-shell-arena-remote-wire)))

;; A regression: a status with members once failed to serialize, and the
;; poll script fell back to the version-1 answer without a transcript.
(ert-deftest +agent-shell-pstack-remote-delegate-test-poll-page-status ()
  "The host's poll page carries the runner's status as the laptop reads it."
  (+remote-delegate-test--with-transcript
    (let ((status (+agent-shell-arena-remote--make-status
                   :id "abc-1" :phase 'delegating :heartbeat 1 :shape 'delegate
                   :members (list (+agent-shell-arena-remote--make-member
                                   :role "builder" :agent 'codex
                                   :kind 'delegate :state 'running))
                   :results nil)))
      (+agent-shell-arena-remote--write-json
       (expand-file-name "status.json" job) 'status status)
      (+agent-shell-arena-remote--record recorder "notice" "hello")
      (let ((poll (+agent-shell-arena-remote-parse-poll
                   (concat "pstack-arena-poll 2\n"
                           (json-serialize
                            (+agent-shell-arena-remote--poll-page job nil))))))
        (should (equal '(running)
                       (mapcar #'+agent-shell-arena-remote-member-state
                               (+agent-shell-arena-remote-status-members
                                (plist-get poll :status)))))
        (+agent-shell-arena-remote--cache-transcript remote
                                                     (plist-get poll :tail))
        (should (equal '("hello")
                       (mapcar (lambda (record) (alist-get 'text record))
                               (+agent-shell-arena-remote--cached-records
                                remote))))))))

;;;; Poll output

(ert-deftest +agent-shell-pstack-remote-delegate-test-poll-batch-utf-8 ()
  "The host's poll prints valid UTF-8 the laptop parses, non-ASCII included."
  (+remote-delegate-test--with-transcript
    (+agent-shell-arena-remote--record recorder "assistant" "caf\u00e9 \u2014 ok")
    (let* ((output
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (should (eql 0 (apply
                                #'call-process
                                (expand-file-name invocation-name
                                                  invocation-directory)
                                nil t nil "-Q" "--batch"
                                "--eval" "(setq load-prefer-newer t)"
                                (append
                                 (mapcan (lambda (dir) (list "-L" dir))
                                         (seq-filter #'stringp load-path))
                                 (list "-l" "+agent-shell-arena-remote"
                                       "-f" "+agent-shell-arena-remote-poll-batch"
                                       job ""))))))
              (buffer-string)))
           (poll (+agent-shell-arena-remote-parse-poll output)))
      (+agent-shell-arena-remote--cache-transcript remote (plist-get poll :tail))
      (should (member "caf\u00e9 \u2014 ok"
                      (mapcar (lambda (record) (alist-get 'text record))
                              (+agent-shell-arena-remote--cached-records
                               remote)))))))

;;;; Process leases

(defmacro +remote-delegate-test--with-systemd (&rest body)
  "Run BODY with fake systemd-run and systemctl in `bin', logging to `log'.
The fake scope runs its command; systemctl stop kills the processes it
started and is-active then answers inactive."
  (declare (indent 0))
  `(let* ((bin (file-name-as-directory (make-temp-file "pa-systemd-" t)))
          (log (expand-file-name "log" bin))
          (+agent-shell-drive-systemd-run (expand-file-name "systemd-run" bin))
          (+agent-shell-drive-systemctl (expand-file-name "systemctl" bin)))
     (with-temp-file +agent-shell-drive-systemd-run
       (insert "#!/bin/sh\necho \"run $*\" >>" log "\n"
               "while [ \"$1\" != -- ]; do shift; done; shift\n"
               "echo $$ >" bin "scope.pid\nexec \"$@\"\n"))
     (with-temp-file +agent-shell-drive-systemctl
       (insert "#!/bin/sh\necho \"ctl $*\" >>" log "\n"
               "case $2 in\n"
               "  stop) kill -9 $(cat " bin "scope.pid) 2>/dev/null; "
               "touch " bin "stopped;;\n"
               "  is-active) [ -f " bin "stopped ] && { echo inactive; exit 3; }; "
               "echo active;;\nesac\n"))
     (set-file-modes +agent-shell-drive-systemd-run #o755)
     (set-file-modes +agent-shell-drive-systemctl #o755)
     (unwind-protect (progn ,@body)
       (delete-directory bin t))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-scope-lease ()
  "A leased adapter starts outermost in its own scope, after its owner
record is written, and stopping the lease stops the scope."
  (+remote-delegate-test--with-systemd
    (let* ((file (expand-file-name "leases/builder.json" bin))
           (lease (list :file file :unit "pstack-delegate-abc-1-builder"))
           (client (+agent-shell-drive--lease-client
                    lease '((:command . "/bin/sleep")
                            (:command-params . ("30"))
                            (:environment-variables . ("A=1"))))))
      (should (equal (+agent-shell-drive--read-lease file)
                     `((version . 1)
                       (nonce . ,(alist-get 'nonce
                                            (+agent-shell-drive--read-lease
                                             file)))
                       (unit . "pstack-delegate-abc-1-builder"))))
      (should (equal #o600 (file-modes file)))
      (should (equal +agent-shell-drive-systemd-run (alist-get :command client)))
      (should (equal '("--user" "--scope" "--quiet" "--collect"
                       "--unit=pstack-delegate-abc-1-builder" "--"
                       "/bin/sleep" "30")
                     (alist-get :command-params client)))
      (should (equal '("A=1") (alist-get :environment-variables client)))
      (let ((process (make-process :name "pa-scope" :noquery t
                                   :command (cons (alist-get :command client)
                                                  (alist-get :command-params
                                                             client)))))
        (+remote-delegate-test--wait
         (lambda (done)
           (run-at-time 0.3 nil done)))
        (should (process-live-p process))
        (should (equal '(nil)
                       (+remote-delegate-test--wait
                        (lambda (done)
                          (+agent-shell-drive-reap-lease file done)))))
        (+remote-delegate-test--wait
         (lambda (done) (run-at-time 0.3 nil done)))
        (should-not (process-live-p process))
        (should (string-search "ctl --user stop pstack-delegate-abc-1-builder"
                               (+remote-delegate-test--read-file log)))))))

(defun +remote-delegate-test--read-file (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest +agent-shell-pstack-remote-delegate-test-process-lease ()
  "Without a scope, a lease kills only the adapter whose pid still has its
recorded start time, with its process tree."
  (let* ((directory (make-temp-file "pa-lease-" t))
         (file (expand-file-name "lease.json" directory))
         (process (make-process :name "pa-lease" :noquery t
                                :command '("/bin/sh" "-c"
                                           "sleep 30 & sleep 30; wait"))))
    (unwind-protect
        (progn
          (+agent-shell-drive--write-lease
           (list :file file)
           `((version . 1) (unit . :null) (pid . ,(process-id process))
             (start . "(1 2 3 4)")))
          (should (equal '(nil) (+remote-delegate-test--wait
                                 (lambda (done)
                                   (+agent-shell-drive-reap-lease file done)))))
          (should (process-live-p process))
          (+agent-shell-drive--write-lease
           (list :file file)
           `((version . 1) (unit . :null) (pid . ,(process-id process))
             (start . ,(format "%S" (alist-get 'start (process-attributes
                                                       (process-id
                                                        process)))))))
          (+remote-delegate-test--wait
           (lambda (done) (+agent-shell-drive-reap-lease file done)))
          (+remote-delegate-test--wait (lambda (done) (run-at-time 0.3 nil done)))
          (should-not (process-live-p process)))
      (when (process-live-p process) (delete-process process))
      (delete-directory directory t))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-quarantine ()
  "A quarantined lock is never taken over, even once its owner is gone."
  (+remote-delegate-test--with-scratch
    (let* ((lock (expand-file-name "locks/repo" scratch))
           (runner (+agent-shell-arena-remote--make-runner :lock lock)))
      (make-directory lock t)
      (with-temp-file (expand-file-name "owner" lock)
        (insert "999999999 \"gone-job\"\n"))
      (+agent-shell-arena-remote--quarantine runner "Writers survived.")
      (should-not (+agent-shell-arena-remote-acquire lock "next-job"))
      (delete-file (expand-file-name "quarantine" lock))
      (should (+agent-shell-arena-remote-acquire lock "next-job")))))

(ert-deftest +agent-shell-pstack-remote-delegate-test-progress-events ()
  "Only progress watchers get agent text chunks and tool calls, even a
tool call with no agent text before it."
  (let ((+agent-shell-drive--watchers (make-hash-table :test #'equal))
        (+agent-shell-drive--shells (make-hash-table :test #'eq))
        (+agent-shell-drive--progress (make-hash-table :test #'equal))
        (shell (generate-new-buffer " *progress*"))
        (plain nil) (progress nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest _) 'token))
                  ((symbol-function 'agent-shell-session-id)
                   (lambda (&rest _) "s1")))
          (+agent-shell-drive-watch shell 'plain 1
                                    (lambda (event) (push event plain)))
          (+agent-shell-drive-watch-progress
           shell 'observer 1 (lambda (event) (push event progress)))
          (+agent-shell-drive--dispatch
           shell '((:event . tool-call-update)
                   (:data (:tool-call (:title . "Run tests")
                                      (:status . "in_progress")))))
          (+agent-shell-drive--dispatch
           shell '((:event . agent-message-chunk) (:data (:text-chunk . "Hi"))))
          (should-not plain)
          (should (equal '((:kind progress :shell nil :part assistant :text "Hi")
                           (:kind progress :shell nil :part tool
                                  :text "Run tests" :status "in_progress"))
                         (mapcar (lambda (event) (plist-put event :shell nil))
                                 progress))))
      (kill-buffer shell))))

(provide 'agent-shell-pstack-remote-delegate-test)
;;; agent-shell-pstack-remote-delegate-test.el ends here
