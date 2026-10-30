;;; agent-shell-pstack-delegate-acp-test.el --- pstack delegates over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; Delegate panels end to end: a pstack lead in a scratch git repository
;; requests a delegate, a fixture delegate commits a file in its full-access
;; shell, and the lead gets the follow-up.  Every agent is
;; test/fixtures/agent-shell-projects-agent.py.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require '+agent-shell-pstack-panel)

(defvar +agent-shell-pstack-delegate-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-pstack-delegate-acp-test--config (identifier)
  "Return a fixture agent config named IDENTIFIER."
  (agent-shell-make-agent-config
   :identifier identifier :mode-line-name (symbol-name identifier)
   :buffer-name (format "Delegate %s" identifier) :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params
                      (list +agent-shell-pstack-delegate-acp-test--fixture)))))

(defun +agent-shell-pstack-delegate-acp-test--wait-for (predicate)
  "Wait up to 60 seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 60)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun +agent-shell-pstack-delegate-acp-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defmacro +agent-shell-pstack-delegate-acp-test--with (isolation brief
                                                                 &rest body)
  "Run BODY once a fixture lead requests a delegate with BRIEF.
ISOLATION is the repository's delegate isolation.  BODY sees `scratch',
`repo', `git', `lead', `panel' once its follow-up was delivered, and
`shells', each shell the delegate ran in while it ran, with whether
`agent-shell-buffers' listed it."
  (declare (indent 2))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "pstack-delegate-acp-" t))))
          (repo (expand-file-name "repo/" scratch))
          (gitconfig (expand-file-name "gitconfig" scratch))
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                     gitconfig)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" scratch))
          (+agent-shell-pstack-arena-profiles
           `((,repo :delegate (:isolation ,,isolation))))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (agent-shell-show-welcome-message nil)
          (shell-maker-prompt-before-killing-buffer nil)
          (agent-shell-show-config-icons nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-mcp-servers nil)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-agent-configs
           (list (+agent-shell-pstack-delegate-acp-test--config
                  'fixture-codex)))
          (+agent-shell-drive-read-only-modes
           '((fixture-codex . "read-only")))
          (+agent-shell-sandbox-agents
           '((fixture-codex :mode "agent-full-access" :name "Codex")))
          (+agent-shell-pstack-panel-retry-delay 0)
          (+agent-shell-pstack-invocations '((fixture-codex . "$poteto-mode")))
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-drive--watchers (make-hash-table :test #'equal))
          (+agent-shell-drive--shells (make-hash-table :test #'eq))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-reminder-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (git (lambda (&rest args)
                 (apply #'+agent-shell-pstack-delegate-acp-test--git repo
                        args)))
          (shells nil)
          (note-shell
           (lambda ()
             (when-let* ((panel (car (+agent-shell-pstack-panel-list)))
                         (shell (+agent-shell-pstack-panel-member-shell
                                 (car (+agent-shell-pstack-panel-members
                                       panel))))
                         ((buffer-live-p shell))
                         ((not (assq shell shells))))
               (push (cons shell (and (memq shell (agent-shell-buffers)) t))
                     shells))))
          (lead nil) (panel nil))
     (with-temp-file gitconfig
       (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
               "[init]\n\tdefaultBranch = main\n"))
     (make-directory repo)
     (funcall git "init" "-q")
     (with-temp-file (expand-file-name "README" repo) (insert "Scratch\n"))
     (funcall git "add" "-A")
     (funcall git "commit" "-q" "-m" "initial")
     (unwind-protect
         (save-window-excursion
           (+agent-shell-pstack-panel-mode 1)
           (add-hook '+agent-shell-pstack-panel-change-hook note-shell)
           (setq lead (+agent-shell-drive-start :config 'fixture-codex
                                                :directory repo))
           (+agent-shell-pstack-delegate-acp-test--wait-for
            (lambda () (and (agent-shell-session-id :shell-buffer lead)
                            (eq (agent-shell-status :shell-buffer lead)
                                'ready))))
           (with-current-buffer lead (+agent-shell-pstack-mode 1))
           (should (+agent-shell-drive-send
                    lead
                    (concat "PSTACK-LEAD "
                            (json-serialize
                             `((version . 1) (shape . "delegate")
                               (brief . ,,brief) (timeout . 60)
                               (members . [((role . "builder")
                                            (agent . "fixture-codex"))]))))))
           (+agent-shell-pstack-delegate-acp-test--wait-for
            (lambda ()
              (setq panel (car (+agent-shell-pstack-panel-list)))
              (and panel (+agent-shell-pstack-panel-delivered panel))))
           ,@body)
       (let ((deadline (+ (float-time) 10)))
         (while (and (< (float-time) deadline)
                     (seq-some (lambda (process)
                                 (string-match-p
                                  "\\`pstack-\\(?:arena\\|sandbox\\)"
                                  (process-name process)))
                               (process-list)))
           (accept-process-output nil 0.05)))
       (remove-hook '+agent-shell-pstack-panel-change-hook note-shell)
       (+agent-shell-pstack-panel-mode -1)
       (when panel
         (+agent-shell-drive-kill
          (+agent-shell-pstack-panel-member-shell
           (car (+agent-shell-pstack-panel-members panel)))))
       (+agent-shell-drive-kill lead)
       (when (get-buffer "*pstack panels*") (kill-buffer "*pstack panels*"))
       (delete-directory scratch t))))

(defun +agent-shell-pstack-delegate-acp-test--available-p ()
  "Return whether this host can run a delegate end to end."
  (and (executable-find "python3") (file-executable-p "/bin/ps")))

(defun +agent-shell-pstack-delegate-acp-test--alive-p (pid)
  "Return whether PID runs and is not a zombie."
  (when-let* ((process (assq pid (+agent-shell-drive--processes))))
    (not (nth 3 process))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-worktree ()
  "Commit on the delegate's branch in its worktree, stop its writer, clean."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (let* ((outside (file-name-as-directory
                   (file-truename (make-temp-file "pstack-writer-" t))))
         (log (expand-file-name "writer.log" outside))
         (pidfile (expand-file-name "writer.pid" outside)))
    (unwind-protect
        (+agent-shell-pstack-delegate-acp-test--with 'worktree
            (format "Add the builder file. WRITER %s %s" log pidfile)
          (let* ((manifest (+agent-shell-pstack-panel-arena panel))
                 (base (+agent-shell-arena-git-manifest-base manifest))
                 (branch (concat "pstack/delegate/"
                                 (+agent-shell-pstack-panel-id panel)))
                 (member (car (+agent-shell-pstack-panel-members panel)))
                 (worktree (+agent-shell-arena-git-role-worktree
                            (+agent-shell-arena-git-role manifest "builder")))
                 (followup (+agent-shell-pstack-panel-followup panel))
                 (writer (with-temp-buffer
                           (insert-file-contents pidfile)
                           (string-to-number (buffer-string)))))
            (should (equal '(nil) (mapcar #'cdr shells)))
            (should (memq lead (agent-shell-buffers)))
            (should (equal (format "DELEGATE role=builder \
mode=agent-full-access branch=%s" branch)
                           (+agent-shell-pstack-panel-member-text member)))
            (should (string-search (format "Branch: %s, 1 commit since %s"
                                           branch base)
                                   followup))
            (should (string-search "Add builder.txt" followup))
            (should (string-search
                     (concat "Workspace: " (directory-file-name worktree))
                     followup))
            (should-not (+agent-shell-pstack-delegate-acp-test--alive-p
                         writer))
            (let ((size (file-attribute-size (file-attributes log))))
              (accept-process-output nil 0.5)
              (should (= size (file-attribute-size (file-attributes log)))))
            (should (equal "main" (funcall git "branch" "--show-current")))
            (should (equal "Add builder.txt"
                           (funcall git "log" "-1" "--format=%s" branch)))
            (+agent-shell-pstack-delegate-acp-test--wait-for
             (lambda () (with-current-buffer lead
                          (save-excursion
                            (goto-char (point-min))
                            (search-forward "TRIAGED" nil t)))))
            (let ((cleaned nil))
              (cl-letf* ((message (symbol-function 'message))
                         ((symbol-function 'message)
                          (lambda (format &rest args)
                            (when (and format (string-prefix-p "Arena" format))
                              (setq cleaned
                                    (apply #'format-message format args)))
                            (apply message format args)))
                         ((symbol-function 'yes-or-no-p) (lambda (_) t)))
                (+agent-shell-pstack-panels)
                (with-current-buffer "*pstack panels*"
                  (goto-char (point-min))
                  (search-forward "    Workspace  ")
                  (+agent-shell-pstack-panels-clean))
                (+agent-shell-pstack-delegate-acp-test--wait-for
                 (lambda () cleaned)))
              (should (string-search (format "kept branch %s: 1 commit on it \
is not on main" branch)
                                     cleaned)))
            (should-not (file-exists-p worktree))
            (should (equal "Add builder.txt"
                           (funcall git "log" "-1" "--format=%s" branch)))
            (should (equal "" (funcall git "status" "--porcelain")))))
      (delete-directory outside t))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-branch ()
  "Commit in the lead's own checkout on the delegate branch, then return it."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--with 'branch "Add the builder file."
    (let ((branch (concat "pstack/delegate/"
                          (+agent-shell-pstack-panel-id panel)))
          (followup (+agent-shell-pstack-panel-followup panel)))
      (should (equal (format "DELEGATE role=builder mode=agent-full-access \
branch=%s" branch)
                     (+agent-shell-pstack-panel-member-text
                      (car (+agent-shell-pstack-panel-members panel)))))
      (should (string-search "Your checkout was clean, so Emacs switched it \
back to main." followup))
      (should (equal "main" (funcall git "branch" "--show-current")))
      (should (equal "Add builder.txt"
                     (funcall git "log" "-1" "--format=%s" branch)))
      (should (equal "" (funcall git "status" "--porcelain"))))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-branch-left-dirty ()
  "Leave a checkout the delegate left dirty on its branch, and say so."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--with 'branch
      "Add the builder file. LEAVE-DIRTY"
    (let ((branch (concat "pstack/delegate/"
                          (+agent-shell-pstack-panel-id panel))))
      (should (string-search "The checkout has uncommitted changes: \
loose.txt.  Commit or stash, then run git switch main."
                             (+agent-shell-pstack-panel-followup panel)))
      (should (equal branch (funcall git "branch" "--show-current"))))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-provider-failure ()
  "Settle a delegate a login failure stopped as error, keeping its commit."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--with 'branch
      "Add the builder file. PROVIDER-FAIL error:authentication_failed 1"
    (let ((branch (concat "pstack/delegate/"
                          (+agent-shell-pstack-panel-id panel)))
          (member (car (+agent-shell-pstack-panel-members panel)))
          (followup (+agent-shell-pstack-panel-followup panel)))
      (should (eq 'error (+agent-shell-pstack-panel-member-state member)))
      (should-not (+agent-shell-pstack-panel-member-retry member))
      (should (string-search "builder (fixture-codex): error.\nCodex needs \
to log in again: Internal error\n" followup))
      (should (string-search (format "Branch: %s, 1 commit since" branch)
                             followup))
      (should (string-search "Partial before the failure." followup))
      (should (equal "Add builder.txt"
                     (funcall git "log" "-1" "--format=%s" branch))))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-provider-retry ()
  "Ask a delegate an overload stopped to continue, then report it done."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--with 'branch
      "Add the builder file. PROVIDER-FAIL codex 1"
    (let ((branch (concat "pstack/delegate/"
                          (+agent-shell-pstack-panel-id panel)))
          (member (car (+agent-shell-pstack-panel-members panel))))
      (should (eq 'done (+agent-shell-pstack-panel-member-state member)))
      (should (eq 'overloaded (plist-get (+agent-shell-pstack-panel-member-failure
                                          member)
                                         :kind)))
      (should (equal "REPORT continued after codex"
                     (+agent-shell-pstack-panel-member-text member)))
      (should (equal "Add builder.txt"
                     (funcall git "log" "-1" "--format=%s" branch))))))

(defmacro +agent-shell-pstack-delegate-acp-test--tool-calls (brief &rest body)
  "Run a delegate whose fixture brief is TOOL-CALLS PIDFILE and BRIEF.
BODY sees `member', `followup', `infra', the pids of what its adapter
started without a tool call, and `gates', those of the test commands
its tool calls left running.  Every pid is killed afterwards."
  (declare (indent 1))
  `(let* ((outside (file-name-as-directory
                    (file-truename (make-temp-file "pstack-tool-calls-" t))))
          (pidfile (expand-file-name "pids.json" outside))
          (infra nil) (gates nil))
     (unwind-protect
         (+agent-shell-pstack-delegate-acp-test--with 'branch
             (format "Add the builder file. TOOL-CALLS %s %s" pidfile ,brief)
           (let* ((pids (json-parse-string
                         (with-temp-buffer (insert-file-contents pidfile)
                                           (buffer-string))
                         :object-type 'plist :array-type 'list))
                  (member (car (+agent-shell-pstack-panel-members panel)))
                  (followup (+agent-shell-pstack-panel-followup panel)))
             (setq infra (plist-get pids :infra)
                   gates (plist-get pids :gates))
             (should (= 4 (length infra)))
             ,@body))
       (dolist (pid (append infra gates))
         (ignore-errors (signal-process pid 'SIGKILL)))
       (delete-directory outside t))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-adapter-processes ()
  "Settle without asking when only the adapter's own processes still run."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--tool-calls ""
    (should-not gates)
    (should (string-prefix-p "DELEGATE role=builder"
                             (+agent-shell-pstack-panel-member-text member)))
    (should-not (string-search "still running" followup))
    (should-not (seq-some #'+agent-shell-pstack-delegate-acp-test--alive-p
                          infra))))

(ert-deftest +agent-shell-pstack-delegate-acp-test-tool-call-commands ()
  "Ask once to wait for running tool call commands, naming only those."
  (skip-unless (+agent-shell-pstack-delegate-acp-test--available-p))
  (+agent-shell-pstack-delegate-acp-test--tool-calls "RUN-GATE"
    (let ((asked (+agent-shell-pstack-panel-member-text member))
          (note (substring followup
                           (string-search "Commands it started" followup))))
      (should (= 2 (length gates)))
      (should (string-prefix-p "WAITED\nYour turn ended while commands you \
started are still running:\n" asked))
      (dolist (text (list asked note))
        (dolist (gate gates)
          (should (string-search (format "(pid %d)" gate) text)))
        (dolist (child '("sleep 117" "sleep 118"))
          (should (string-match-p (concat "^  " child " (pid [0-9]+)$") text)))
        (dolist (pid infra)
          (should-not (string-search (format "(pid %d)" pid) text)))
        (dolist (name '("example-mcp-server" "mcp serve" "agent-hook"
                        "exec-host" "sleep 120" "git status"))
          (should-not (string-search name text))))
      (should (string-search "after Emacs had asked it once to wait for them; \
Emacs stopped them with the delegate:\n" note))
      (should-not (seq-some #'+agent-shell-pstack-delegate-acp-test--alive-p
                            (append infra gates))))))

(provide 'agent-shell-pstack-delegate-acp-test)
;;; agent-shell-pstack-delegate-acp-test.el ends here
