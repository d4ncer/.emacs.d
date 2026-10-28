;;; agent-shell-pstack-delegate-test.el --- Tests for pstack delegate panels -*- lexical-binding: t; -*-

;;; Commentary:
;; Delegate panels against a stubbed `+agent-shell-drive' surface and real
;; git in a scratch repository.  A test plays the delegate by committing in
;; the directory its shell was started in, then sending member events.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack-panel)

(defmacro +agent-shell-pstack-delegate-test--with (&rest body)
  "Run BODY with a pstack lead in a scratch repository and stubbed drive.
BODY sees `repo' (the lead's checkout, on main), `arenas', `sent' (newest
first, each (SHELL . TEXT)), `started' (each (AGENT SHELL POLICY
DIRECTORY)), `stopped' (shells stopped, newest first), `survivors' (the
pids each stop reports), `interrupted', and the functions `git',
`lead-event', `member-event', `wait-for' and `followup'."
  (declare (indent 0))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "pstack-delegate-" t))))
          (repo (expand-file-name "repo/" scratch))
          (arenas (expand-file-name "arenas/" scratch))
          (gitconfig (expand-file-name "gitconfig" scratch))
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                     gitconfig)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (+agent-shell-pstack-arena-directory arenas)
          (+agent-shell-pstack-arena-profiles nil)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-reminder-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (+agent-shell-drive-read-only-modes
           '((claude-code . "plan") (codex . "read-only")))
          (+agent-shell-sandbox-agents
           '((claude-code :mode "bypassPermissions")
             (codex :mode "agent-full-access")))
          (handlers (make-hash-table :test #'equal))
          (sent nil) (started nil) (stopped nil) (survivors nil)
          (interrupted nil)
          (lead (generate-new-buffer " *lead*")))
     (cl-labels
         ((git (&rest args)
            (with-temp-buffer
              (let ((default-directory repo))
                (unless (eql 0 (apply #'process-file "git" nil t nil args))
                  (error "git %s: %s" (string-join args " ") (buffer-string)))
                (string-trim (buffer-string)))))
          (lead-event (&rest event)
            (funcall (gethash (cons 'pstack-lead lead) handlers)
                     (append event (list :shell lead))))
          (member-event (role &rest event)
            (let ((id (+agent-shell-pstack-panel-id
                       (car (last (+agent-shell-pstack-panel-list))))))
              (funcall (gethash (cons 'pstack-panel (cons id role)) handlers)
                       event)))
          (wait-for (predicate)
            (let ((deadline (+ (float-time) 30)))
              (while (and (not (funcall predicate)) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should (funcall predicate)))
          (followup ()
            (wait-for (lambda () (assq lead sent)))
            (cdr (assq lead sent))))
       (with-temp-file gitconfig
         (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
                 "[init]\n\tdefaultBranch = main\n"))
       (make-directory repo)
       (git "init" "-q")
       (with-temp-file (expand-file-name "README" repo) (insert "Scratch\n"))
       (git "add" "-A")
       (git "commit" "-q" "-m" "initial")
       (cl-letf (((symbol-function '+agent-shell-drive--configs)
                  (lambda () '(((:identifier . codex))
                               ((:identifier . claude-code))
                               ((:identifier . gemini)))))
                 ((symbol-function 'agent-shell-cwd) (lambda () repo))
                 ((symbol-function '+agent-shell-drive-watch)
                  (lambda (_shell owner id function)
                    (puthash (cons owner id) function handlers)))
                 ((symbol-function '+agent-shell-drive-unwatch)
                  (lambda (owner &optional id)
                    (dolist (key (hash-table-keys handlers))
                      (when (and (eq (car key) owner)
                                 (or (null id) (equal (cdr key) id)))
                        (remhash key handlers)))))
                 ((symbol-function '+agent-shell-drive-send)
                  (lambda (shell text) (push (cons shell text) sent) t))
                 ((symbol-function '+agent-shell-drive-interrupt)
                  (lambda (shell) (push shell interrupted)))
                 ((symbol-function '+agent-shell-drive-partial-text)
                  (lambda (_shell) "partial"))
                 ((symbol-function '+agent-shell-drive-kill)
                  (lambda (shell &optional _reason)
                    (when (buffer-live-p shell) (kill-buffer shell))))
                 ((symbol-function '+agent-shell-drive-start)
                  (cl-function
                   (lambda (&key config directory policy)
                     (let ((shell (generate-new-buffer " *delegate*")))
                       (setq started (append started
                                             (list (list config shell policy
                                                         directory))))
                       shell))))
                 ((symbol-function '+agent-shell-drive-stop-unconfined)
                  (lambda (shell &optional _reason callback)
                    (push shell stopped)
                    (run-at-time 0 nil
                                 (lambda ()
                                   (when (buffer-live-p shell)
                                     (kill-buffer shell))
                                   (funcall callback survivors))))))
         (unwind-protect
             (progn
               (with-current-buffer lead
                 (setq-local +agent-shell-pstack-mode t))
               (+agent-shell-pstack-panel-mode 1)
               ,@body)
           (+agent-shell-pstack-delegate-test--settle)
           (+agent-shell-pstack-panel-mode -1)
           (dolist (panel (+agent-shell-pstack-panel-list))
             (+agent-shell-pstack-panel--disarm panel))
           (dolist (entry started)
             (when (buffer-live-p (cadr entry)) (kill-buffer (cadr entry))))
           (when (get-buffer "*pstack panels*") (kill-buffer "*pstack panels*"))
           (kill-buffer lead)
           (delete-directory scratch t))))))

(defun +agent-shell-pstack-delegate-test--settle ()
  "Let pending git processes and timers finish, for up to ten seconds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline)
                (seq-some (lambda (process)
                            (string-match-p "\\`pstack-\\(?:arena\\|sandbox\\)"
                                            (process-name process)))
                          (process-list)))
      (accept-process-output nil 0.02))
    (dotimes (_ 5) (accept-process-output nil 0.01))))

(defun +agent-shell-pstack-delegate-test--request (&rest fields)
  "Return a delegate request with FIELDS, a plist overriding the defaults."
  (let ((request (list :version 1 :shape "delegate" :brief "Add a feature."
                       :members (vector '(:role "builder" :agent "codex")))))
    (cl-loop for (key value) on fields by #'cddr
             do (setq request (plist-put request key value)))
    (json-serialize request)))

(defun +agent-shell-pstack-delegate-test--message (json)
  "Return a lead message ending with a pstack-panel block of JSON."
  (format "Delegating.\n\n```pstack-panel\n%s\n```\n" json))

(defun +agent-shell-pstack-delegate-test--commit (directory file)
  "Commit a new FILE in DIRECTORY, as a delegate would."
  (with-temp-file (expand-file-name file directory) (insert "new\n"))
  (let ((default-directory (file-name-as-directory directory)))
    (process-file "git" nil nil nil "add" file)
    (process-file "git" nil nil nil "commit" "-q" "-m" (concat "Add " file))))

(defmacro +agent-shell-pstack-delegate-test--started (&rest body)
  "Request the default delegate, wait for its shell, then run BODY.
BODY sees `panel', `shell', `directory' where it works, and `branch'."
  (declare (indent 0))
  `(progn
     (lead-event :kind 'turn-complete :session "s1"
                 :text (+agent-shell-pstack-delegate-test--message
                        (+agent-shell-pstack-delegate-test--request)))
     (wait-for (lambda () started))
     (let* ((panel (car (+agent-shell-pstack-panel-list)))
            (shell (nth 1 (car started)))
            (directory (nth 3 (car started)))
            (branch (concat "pstack/delegate/"
                            (+agent-shell-pstack-panel-id panel))))
       (ignore shell directory branch)
       ,@body)))

(defun +agent-shell-pstack-delegate-test--line (text)
  "Move to the panels buffer line containing TEXT and return the buffer."
  (+agent-shell-pstack-panels)
  (with-current-buffer "*pstack panels*"
    (goto-char (point-min))
    (should (search-forward text nil t))
    (current-buffer)))

(ert-deftest +agent-shell-pstack-delegate-test-validation ()
  "Reject malformed delegate requests with reasons the lead can act on."
  (+agent-shell-pstack-delegate-test--with
    (let ((+agent-shell-sandbox-backend nil)
          (panel (+agent-shell-pstack-panel--parse
                  (+agent-shell-pstack-delegate-test--request))))
      (should (eq 'delegate (+agent-shell-pstack-panel-shape panel)))
      (should (= 2700 (+agent-shell-pstack-panel-timeout panel)))
      (should (eq 'delegate (+agent-shell-pstack-panel-member-kind
                             (car (+agent-shell-pstack-panel-members panel))))))
    (pcase-dolist
        (`(,fields ,reason)
         `(((:members [])
            "Delegate panels take exactly one member, the agent you hand the \
task to.")
           ((:members ,(vector '(:role "a" :agent "codex")
                               '(:role "b" :agent "codex")))
            "Delegate panels take exactly one member, the agent you hand the \
task to.")
           ((:members ,(vector '(:role "a" :agent "gemini")))
            "Member a has agent \"gemini\"; available agents: codex, \
claude-code")
           ((:rubric ["x" "y" "z"] :judge_timeout 60)
            "Delegate panels have no judge, so remove \"rubric\" and \
\"judge_timeout\"")
           ((:judge (:agent "codex"))
            "Delegate panels have no judge, so remove \"judge\"")
           ((:timeout 3601) "Timeout must be a whole number of seconds from 1 \
to 3600")))
      (let ((err (should-error
                  (+agent-shell-pstack-panel--parse
                   (apply #'+agent-shell-pstack-delegate-test--request fields))
                  :type 'user-error)))
        (should (equal reason (cadr err)))))))

(ert-deftest +agent-shell-pstack-delegate-test-profile-rejection ()
  "Reject the request when the repository's :delegate profile is malformed."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation shared))))
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-delegate-test--message
                       (+agent-shell-pstack-delegate-test--request)))
    (should (string-search ":delegate :isolation must be `worktree' or \
`branch', not shared" (followup)))
    (should-not started)
    (should-not (+agent-shell-arena-git-arenas))))

(ert-deftest +agent-shell-pstack-delegate-test-worktree ()
  "Hand a worktree delegate's commits back on its branch, then clean up."
  (+agent-shell-pstack-delegate-test--with
    (+agent-shell-pstack-delegate-test--started
      (let ((base (git "rev-parse" "HEAD")))
        (should (equal '(codex full-access) (list (car (car started))
                                                  (nth 2 (car started)))))
        (should (string-prefix-p arenas directory))
        (should (eq 'delegating (+agent-shell-pstack-panel-phase panel)))
        (member-event "builder" :kind 'init-finished)
        (should (string-search (format "You work on branch %s, in a git \
worktree of your own." branch)
                               (cdr (assq shell sent))))
        (should (string-search "commit your work with git"
                               (cdr (assq shell sent))))
        (should (string-search "Workspace: "
                               (with-current-buffer
                                   (+agent-shell-pstack-delegate-test--line
                                    "builder")
                                 (buffer-string))))
        (with-current-buffer "*pstack panels*"
          (+agent-shell-pstack-panels-visit)
          (should (eq shell (window-buffer (selected-window)))))
        (+agent-shell-pstack-delegate-test--commit directory "one.txt")
        (+agent-shell-pstack-delegate-test--commit directory "two.txt")
        (member-event "builder" :kind 'turn-complete :text "Built it.")
        (let ((text (followup)))
          (should (string-prefix-p
                   (format "pstack panel %s (delegate) has finished: builder \
(codex): done.\nBranch: %s, 2 commits since %s, the commit it started from:\n"
                           (+agent-shell-pstack-panel-id panel) branch base)
                   text))
          (should (string-match-p "^  [0-9a-f]\\{12\\} Add one.txt\n  \
[0-9a-f]\\{12\\} Add two.txt\nWorkspace: " text))
          (should (string-search "Report:\nBuilt it.\n" text))
          (should (string-search (format "To review: git log %s..%s\n\
To merge: git merge --ff-only %s, or push it" base branch branch)
                                 text))
          (should (string-search "Review the branch before you merge it"
                                 text)))
        (should (equal (list shell) stopped))
        (should (equal "main" (git "branch" "--show-current")))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (with-current-buffer (+agent-shell-pstack-delegate-test--line
                                "Workspace:")
            (+agent-shell-pstack-panels-clean)))
        (wait-for (lambda () (not (file-exists-p directory))))
        (should (equal "Add two.txt" (git "log" "-1" "--format=%s" branch)))
        (should (= 1 (length (split-string (git "worktree" "list") "\n"))))))))

(ert-deftest +agent-shell-pstack-delegate-test-branch-dirty-start ()
  "Refuse a branch-mode delegate in a dirty checkout, creating nothing."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (with-temp-file (expand-file-name "loose.txt" repo) (insert "x\n"))
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-delegate-test--message
                       (+agent-shell-pstack-delegate-test--request)))
    (should (string-search "Emacs rejected your pstack-panel request: \
Branch-mode delegates need a clean checkout; commit or stash your changes \
first.  Changed: loose.txt.\nNo panel started." (followup)))
    (should-not started)
    (should (equal "main" (git "branch" "--show-current")))
    (should (equal "main" (git "branch" "--format=%(refname:short)")))
    (should-not (+agent-shell-arena-git-arenas))))

(ert-deftest +agent-shell-pstack-delegate-test-branch-restores ()
  "Work in the lead's checkout on the delegate branch and switch back."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (+agent-shell-pstack-delegate-test--started
      (should (equal repo directory))
      (should (equal branch (git "branch" "--show-current")))
      (should (string-search "do not edit or commit there"
                             (with-current-buffer
                                 (+agent-shell-pstack-delegate-test--line
                                  "Isolation:")
                               (buffer-string))))
      (should (string-search
               "the delegate works in your checkout"
               (get-text-property
                0 'help-echo
                (+agent-shell-pstack-panel--mode-line-text lead))))
      (member-event "builder" :kind 'init-finished)
      (should (string-search "in the other agent's own checkout"
                             (cdr (assq shell sent))))
      (+agent-shell-pstack-delegate-test--commit repo "one.txt")
      (member-event "builder" :kind 'turn-complete :text "Built it.")
      (let ((text (followup)))
        (should (string-search "1 commit since" text))
        (should (string-search (format "Isolation: your own checkout, on %s \
while it ran.  Your checkout was clean, so Emacs switched it back to main.\n"
                                       branch)
                               text)))
      (should (equal "main" (git "branch" "--show-current")))
      (should (equal "Add one.txt" (git "log" "-1" "--format=%s" branch))))))

(ert-deftest +agent-shell-pstack-delegate-test-branch-left-dirty ()
  "Leave a dirty checkout on the delegate branch until x returns it."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (+agent-shell-pstack-delegate-test--started
      (member-event "builder" :kind 'init-finished)
      (with-temp-file (expand-file-name "loose.txt" repo) (insert "x\n"))
      (member-event "builder" :kind 'turn-complete :text "Left a file.")
      (should (string-search "Emacs left your checkout there instead of \
switching back to main.  The checkout has uncommitted changes: loose.txt.  \
Commit or stash, then run git switch main." (followup)))
      (should (equal branch (git "branch" "--show-current")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (with-current-buffer (+agent-shell-pstack-delegate-test--line
                              "x switches it back to main")
          (+agent-shell-pstack-panels-clean))
        (accept-process-output nil 0.5)
        (should (equal branch (git "branch" "--show-current")))
        (delete-file (expand-file-name "loose.txt" repo))
        (with-current-buffer (+agent-shell-pstack-delegate-test--line
                              "x switches it back to main")
          (+agent-shell-pstack-panels-clean)))
      (wait-for (lambda () (equal "main" (git "branch" "--show-current"))))
      (wait-for (lambda () (not (+agent-shell-arena-git-arenas)))))))

(ert-deftest +agent-shell-pstack-delegate-test-restart-recovery ()
  "List a delegate an earlier Emacs left mid-run, and restore it once."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (+agent-shell-pstack-delegate-test--started
      (clrhash +agent-shell-pstack-panel--panels)
      (should (equal branch (git "branch" "--show-current")))
      (let ((buffer (+agent-shell-pstack-delegate-test--line "interrupted")))
        (with-current-buffer buffer
          (should (string-search (concat "is on " branch) (buffer-string)))
          (forward-line 1)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (+agent-shell-pstack-panels-clean))))
      (wait-for (lambda () (equal "main" (git "branch" "--show-current"))))
      (wait-for (lambda () (not (+agent-shell-arena-git-arenas))))
      (with-current-buffer (+agent-shell-pstack-delegate-test--line "No panels")
        (should-not (string-search "interrupted" (buffer-string))))
      (should (equal branch (git "branch" "--list" "--format=%(refname:short)"
                                 branch))))))

(ert-deftest +agent-shell-pstack-delegate-test-timeout-keeps-commits ()
  "Collect what a timed-out delegate committed."
  (+agent-shell-pstack-delegate-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-delegate-test--message
                       (+agent-shell-pstack-delegate-test--request
                        :timeout 1)))
    (wait-for (lambda () started))
    (+agent-shell-pstack-delegate-test--commit (nth 3 (car started)) "one.txt")
    (member-event "builder" :kind 'init-finished)
    (let ((text (followup)))
      (should (string-search "builder (codex): timeout.\nNo report within the \
1-second timeout.\nBranch: " text))
      (should (string-search "1 commit since" text))
      (should (string-search "Add one.txt" text))
      (should (string-search "Report:\npartial" text)))))

(ert-deftest +agent-shell-pstack-delegate-test-cancel-keeps-commits ()
  "Collect a cancelled delegate's commits and return a branch-mode checkout."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (+agent-shell-pstack-delegate-test--started
      (member-event "builder" :kind 'init-finished)
      (+agent-shell-pstack-delegate-test--commit repo "one.txt")
      (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")
      (let ((text (followup)))
        (should (string-search "builder (codex): error.\nYou cancelled the \
panel.\n" text))
        (should (string-search "Add one.txt" text))
        (should (string-search "switched it back to main" text)))
      (should (equal "main" (git "branch" "--show-current"))))))

(ert-deftest +agent-shell-pstack-delegate-test-cancel-while-preparing ()
  "Cancel before the delegate starts, leaving the checkout untouched."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-delegate-test--message
                       (+agent-shell-pstack-delegate-test--request)))
    (+agent-shell-pstack-panel-cancel (car (+agent-shell-pstack-panel-list))
                                      "You cancelled the panel.")
    (should (string-search "(delegate) ended before its delegate started: \
You cancelled the panel." (followup)))
    (accept-process-output nil 0.5)
    (should-not started)
    (should (equal "main" (git "branch" "--show-current")))
    (should-not (+agent-shell-arena-git-arenas))))

(ert-deftest +agent-shell-pstack-delegate-test-lead-ends ()
  "Treat the lead's shell ending like a cancellation."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch))))
    (+agent-shell-pstack-delegate-test--started
      (lead-event :kind 'clean-up)
      (wait-for (lambda () (eq 'finished
                               (+agent-shell-pstack-panel-phase panel))))
      (should (equal (list shell) stopped))
      (should (equal "The lead's shell ended."
                     (+agent-shell-pstack-panel-member-error
                      (car (+agent-shell-pstack-panel-members panel)))))
      (should (equal "main" (git "branch" "--show-current"))))))

(ert-deftest +agent-shell-pstack-delegate-test-survivors-hold-checkout ()
  "Keep a checkout on the delegate branch while a writer outlived the stop."
  (+agent-shell-pstack-delegate-test--with
    (setq +agent-shell-pstack-arena-profiles
          `((,repo :delegate (:isolation branch)))
          survivors '(4242))
    (+agent-shell-pstack-delegate-test--started
      (member-event "builder" :kind 'init-finished)
      (member-event "builder" :kind 'turn-complete :text "Done.")
      (should (string-search "Emacs left your checkout there: Processes 4242 \
outlived the delegate.  Once nothing runs there, press x" (followup)))
      (should (equal branch (git "branch" "--show-current")))
      (+agent-shell-pstack-delegate-test--line
       "Processes 4242 outlived the delegate.  (x switches it back to main)"))))

(ert-deftest +agent-shell-pstack-delegate-test-waits-for-interrupted-turn ()
  "Prepare only once the lead's interrupted turn has ended."
  (+agent-shell-pstack-delegate-test--with
    (let ((text (+agent-shell-pstack-delegate-test--message
                 (+agent-shell-pstack-delegate-test--request))))
      (lead-event :kind 'tool-call-update :session "s1" :text text
                  :title "Run git commit")
      (should (equal (list lead) interrupted))
      (should (eq 'interrupting (+agent-shell-pstack-panel-phase
                                 (car (+agent-shell-pstack-panel-list)))))
      (accept-process-output nil 0.2)
      (should-not started)
      (lead-event :kind 'turn-complete :session "s1" :text text)
      (wait-for (lambda () started)))))

(provide 'agent-shell-pstack-delegate-test)
;;; agent-shell-pstack-delegate-test.el ends here
