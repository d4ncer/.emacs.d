;;; agent-shell-pstack-remote-delegate-acp-test.el --- Remote delegates over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; Delegates end to end.  A sandboxed delegate runs here, and delegates run
;; on a (local . DIRECTORY) host, whose commands run as local processes: a
;; pstack lead in a scratch clone requests a delegate, a "host" clone of
;; the same origin takes the job, and a batch Emacs runner there runs the
;; fixture delegate in a worktree of the host checkout.  The laptop's
;; repository has a git identity of its own, unlike the host's, and every
;; commit the delegate's branch brings back must carry it.  Every agent
;; is test/fixtures/agent-shell-projects-agent.py.  macOS only, since the
;; sandboxed cases use this machine's sandbox-exec.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require '+agent-shell-pstack-panel)
(require '+agent-shell-arena-remote)

(defvar +remote-delegate-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +remote-delegate-acp-test--config (identifier)
  "Return a fixture agent config named IDENTIFIER."
  (agent-shell-make-agent-config
   :identifier identifier :mode-line-name (symbol-name identifier)
   :buffer-name (format "Delegate %s" identifier) :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params (list +remote-delegate-acp-test--fixture)))))

(defun +remote-delegate-acp-test--wait-for (predicate &optional seconds)
  "Wait up to SECONDS, 120 by default, for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) (or seconds 120))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun +remote-delegate-acp-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +remote-delegate-acp-test--read (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun +remote-delegate-acp-test--login (agent)
  "Return a fixture host login for AGENT, valid for a day."
  (lambda ()
    (+agent-shell-sandbox--make-login :agent agent :access "fixture-access"
                                      :expires (+ (float-time) 86400))))

(defun +remote-delegate-acp-test--runner-init (file)
  "Write the host's runner-init.el, FILE, configuring the fixture agent."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert ";;; runner-init.el --- fixture agents -*- lexical-binding: t; -*-\n")
      (prin1
       `(progn
          (setq agent-shell-agent-configs
                (list (agent-shell-make-agent-config
                       :identifier 'fixture-codex :mode-line-name "fixture-codex"
                       :buffer-name "Runner fixture-codex"
                       :shell-prompt "Fixture> " :shell-prompt-regexp "Fixture> "
                       :client-maker
                       (lambda (buffer)
                         (acp-make-client
                          :context-buffer buffer
                          :command ,(executable-find "python3")
                          :command-params
                          (list ,+remote-delegate-acp-test--fixture)))))
                +agent-shell-drive-read-only-modes
                '((fixture-codex . "read-only"))
                +agent-shell-sandbox-agents
                '((fixture-codex
                   :mode "agent-full-access" :prepare (lambda (_) nil)
                   :login (lambda ()
                            (+agent-shell-sandbox--make-login
                             :agent 'fixture-codex :access "fixture-access"
                             :expires (+ (float-time) 86400)))))
                agent-shell-show-welcome-message nil
                agent-shell-show-config-icons nil
                agent-shell-prefer-viewport-interaction nil
                agent-shell-mcp-servers nil
                +agent-shell-arena-remote-tick 0.3
                +agent-shell-arena-remote-heartbeat 1))
       (current-buffer))
      (insert "\n"))))

(defmacro +remote-delegate-acp-test--with (profile &rest body)
  "Run BODY with a lead, an origin, a host checkout and a local host.
PROFILE is the lead repository's profile, which may use `host-root' and
`host-repo'.  BODY sees `scratch', `lead', `lead-repo', `host-repo',
`host-root', `lead-git', `host-git' and `delegated', which sends a
delegate request with a brief and returns its panel once its follow-up
was delivered."
  (declare (indent 1))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "pstack-rdelegate-acp-" t))))
          (gitconfig (expand-file-name "gitconfig" scratch))
          (origin (expand-file-name "origin.git" scratch))
          (lead-repo (expand-file-name "lead/" scratch))
          (host-repo (expand-file-name "host/" scratch))
          (host-root (expand-file-name "host-root/" scratch))
          (coding-system-for-write 'utf-8-unix)
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                     gitconfig)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" scratch))
          (+agent-shell-pstack-arena-profiles
           (list (cons lead-repo ,profile)))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (agent-shell-show-welcome-message nil)
          (shell-maker-prompt-before-killing-buffer nil)
          (agent-shell-show-config-icons nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-mcp-servers nil)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-agent-configs
           (list (+remote-delegate-acp-test--config 'fixture-codex)))
          (+agent-shell-drive-read-only-modes '((fixture-codex . "read-only")))
          (+agent-shell-sandbox-agents
           `((fixture-codex :mode "agent-full-access" :prepare ,(lambda (_) nil)
                            :login ,(+remote-delegate-acp-test--login
                                     'fixture-codex))))
          (+agent-shell-pstack-invocations '((fixture-codex . "$poteto-mode")))
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-drive--watchers (make-hash-table :test #'equal))
          (+agent-shell-drive--shells (make-hash-table :test #'eq))
          (+agent-shell-arena-remote--arenas (make-hash-table :test #'equal))
          (+agent-shell-arena-remote--lines (make-hash-table :test #'equal))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-reminder-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (+agent-shell-pstack-panel-change-hook nil)
          (+agent-shell-arena-remote-poll-interval 0.5)
          (+agent-shell-arena-remote-transcript-interval 0.5)
          (+agent-shell-arena-remote-max-interval 2)
          (+agent-shell-arena-remote-stale-after 3)
          (+agent-shell-arena-remote-unreachable-after 120)
          (lead-git (lambda (&rest args)
                      (apply #'+remote-delegate-acp-test--git lead-repo args)))
          (host-git (lambda (&rest args)
                      (apply #'+remote-delegate-acp-test--git host-repo args)))
          (lead nil)
          (delegated
           (lambda (brief &optional wait timeout)
             (let ((before (+agent-shell-pstack-panel-list)) (panel nil))
               (should (+agent-shell-drive-send
                        lead
                        (concat "PSTACK-LEAD "
                                (json-serialize
                                 `((version . 1) (shape . "delegate")
                                   (brief . ,brief)
                                   (timeout . ,(or timeout 120))
                                   (members . [((role . "builder")
                                                (agent . "fixture-codex"))]))))))
               (+remote-delegate-acp-test--wait-for
                (lambda ()
                  (setq panel (car (seq-difference
                                    (+agent-shell-pstack-panel-list) before
                                    #'eq)))))
               (unless (eq wait 'started)
                 (+remote-delegate-acp-test--wait-for
                  (lambda () (+agent-shell-pstack-panel-delivered panel))))
               panel))))
     (ignore host-git delegated)
     (with-temp-file gitconfig
       (insert "[user]\n\tname = Host Person\n\temail = host@example.com\n"
               "[init]\n\tdefaultBranch = main\n"
               "[commit]\n\tgpgSign = false\n"))
     (+remote-delegate-acp-test--git scratch "init" "-q" "--bare" origin)
     (+remote-delegate-acp-test--git scratch "clone" "-q" origin lead-repo)
     (funcall lead-git "config" "user.name" "Laptop Person")
     (funcall lead-git "config" "user.email" "laptop@example.com")
     (with-temp-file (expand-file-name "README" lead-repo) (insert "Scratch\n"))
     (funcall lead-git "add" "-A")
     (funcall lead-git "commit" "-q" "-m" "initial")
     (funcall lead-git "push" "-q" "origin" "main")
     (funcall lead-git "remote" "set-head" "origin" "main")
     (+remote-delegate-acp-test--git scratch "clone" "-q" origin host-repo)
     (with-temp-file (expand-file-name "lead.txt" lead-repo) (insert "Lead\n"))
     (funcall lead-git "add" "-A")
     (funcall lead-git "commit" "-q" "-m" "local work")
     (with-temp-file (expand-file-name "README" lead-repo)
       (insert "Scratch\nAn uncommitted edit\n"))
     (make-directory host-root t)
     (with-temp-file (expand-file-name "ready.json" host-root)
       (insert (json-serialize
                `((version . 1)
                  (emacs (path . ,(expand-file-name invocation-name
                                                    invocation-directory))
                         (version . ,emacs-version))
                  (capabilities (delegate . 1))))))
     (+remote-delegate-acp-test--runner-init
      (expand-file-name "runner-init.el" host-root))
     (unwind-protect
         (save-window-excursion
           (+agent-shell-pstack-panel-mode 1)
           (setq lead (+agent-shell-drive-start :config 'fixture-codex
                                                :directory lead-repo))
           (+remote-delegate-acp-test--wait-for
            (lambda () (and (agent-shell-session-id :shell-buffer lead)
                            (eq (agent-shell-status :shell-buffer lead)
                                'ready))))
           (with-current-buffer lead (+agent-shell-pstack-mode 1))
           ,@body)
       (maphash (lambda (_ remote)
                  (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
                    (cancel-timer timer)))
                +agent-shell-arena-remote--arenas)
       (+agent-shell-pstack-panel-mode -1)
       (dolist (panel (+agent-shell-pstack-panel-list))
         (+agent-shell-pstack-panel--disarm panel)
         (dolist (member (+agent-shell-pstack-panel-members panel))
           (+agent-shell-drive-kill
            (+agent-shell-pstack-panel-member-shell member))))
       (+agent-shell-drive-kill lead)
       (dolist (directory (and (file-directory-p
                                (expand-file-name "jobs/" host-root))
                               (directory-files (expand-file-name "jobs/"
                                                                  host-root)
                                                t "\\`[^.]")))
         (when-let* ((pid (ignore-errors
                            (string-to-number
                             (+remote-delegate-acp-test--read
                              (expand-file-name "pid" directory)))))
                     ((> pid 0)))
           (ignore-errors (signal-process pid 'SIGKILL))))
       (dolist (buffer (buffer-list))
         (when (string-prefix-p "*pstack delegate " (buffer-name buffer))
           (kill-buffer buffer)))
       (when (get-buffer "*pstack panels*") (kill-buffer "*pstack panels*"))
       (call-process "chmod" nil nil nil "-R" "u+rwX" scratch)
       (ignore-errors (delete-directory scratch t)))))

(defmacro +remote-delegate-acp-test--skip (&optional sandboxed)
  "Skip unless this machine can run the fixture, SANDBOXED if non-nil."
  `(skip-unless (and (executable-find "python3") (file-executable-p "/bin/ps")
                     ,(if sandboxed
                          '(and (eq system-type 'darwin)
                                (file-executable-p "/usr/bin/sandbox-exec"))
                        '(memq system-type '(darwin gnu/linux))))))

(defun +remote-delegate-acp-test--branch-commits (git branch base)
  "Return BRANCH's commits since BASE via GIT as (SUBJECT AUTHOR COMMITTER)."
  (mapcar (lambda (line) (split-string line "|"))
          (split-string (funcall git "log" "--reverse"
                                 "--format=%s|%an <%ae>|%cn <%ce>"
                                 (concat base ".." branch))
                        "\n" t)))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-local-sandboxed ()
  "A sandboxed delegate here cannot commit, write a ref or git config; Emacs
makes its one commit as the laptop identity with the proposed message."
  (+remote-delegate-acp-test--skip t)
  (+remote-delegate-acp-test--with '(:delegate (:sandbox t))
    (let* ((panel (funcall delegated "Add the builder file."))
           (id (+agent-shell-pstack-panel-id panel))
           (branch (concat "pstack/delegate/" id))
           (manifest (+agent-shell-pstack-panel-arena panel))
           (base (+agent-shell-arena-git-manifest-base manifest))
           (member (car (+agent-shell-pstack-panel-members panel)))
           (followup (+agent-shell-pstack-panel-followup panel)))
      (should (string-prefix-p "DELEGATE role=builder mode=agent-full-access \
commit=refused ref=refused config=refused pointer=refused"
                               (+agent-shell-pstack-panel-member-text member)))
      (should (equal '(("[builder] Add builder.txt"
                        "Laptop Person <laptop@example.com>"
                        "Laptop Person <laptop@example.com>"))
                     (+remote-delegate-acp-test--branch-commits
                      lead-git branch base)))
      (should (equal "[builder] Add builder.txt\n\nWrite the file.\n\n\
Checked by the fixture."
                     (funcall lead-git "log" "-1" "--format=%B" branch)))
      (should (string-search "Sandboxed: Emacs made its one commit" followup))
      (should (string-search (format "Branch: %s, 1 commit since %s" branch base)
                             followup))
      (should-not (string-search "sneaky" (funcall lead-git "branch" "-a")))
      (should-not (string-search "Sneaky" (+remote-delegate-acp-test--read
                                           (expand-file-name ".git/config"
                                                             lead-repo))))
      (should (equal "main" (funcall lead-git "branch" "--show-current"))))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-local-no-message ()
  "A sandboxed delegate that proposes no commit leaves its work uncommitted."
  (+remote-delegate-acp-test--skip t)
  (+remote-delegate-acp-test--with '(:delegate (:sandbox t))
    (let* ((panel (funcall delegated "Add the builder file. NO-COMMIT-BLOCK"))
           (manifest (+agent-shell-pstack-panel-arena panel))
           (followup (+agent-shell-pstack-panel-followup panel))
           (worktree (+agent-shell-arena-git-role-worktree
                      (car (+agent-shell-arena-git-manifest-roles manifest)))))
      (should (string-search "Sandboxed: No commit created.  The final response \
has no pstack-commit block" followup))
      (should (string-search "The worktree has uncommitted changes: builder.txt"
                             followup))
      (should (file-exists-p (expand-file-name "builder.txt" worktree))))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-remote ()
  "A full-access delegate on the host commits there as the laptop identity;
its branch comes back through the fetch, with its transcript, and the
follow-up names where it ran, once."
  (+remote-delegate-acp-test--skip)
  (+remote-delegate-acp-test--with
      `(:host (local . ,host-root) :host-directory ,host-repo)
    (let* ((panel (funcall delegated "Add the builder file. NARRATE"))
           (id (+agent-shell-pstack-panel-id panel))
           (branch (concat "pstack/delegate/" id))
           (remote (+agent-shell-pstack-panel-remote panel))
           (manifest (+agent-shell-pstack-panel-arena panel))
           (base (+agent-shell-arena-git-manifest-base manifest))
           (followup (+agent-shell-pstack-panel-followup panel))
           (host-arena (+agent-shell-arena-remote-arena remote)))
      (should (eq 'delegate (+agent-shell-arena-remote-shape remote)))
      ;; The lead's uncommitted README edit reached the host in the bundle,
      ;; as a snapshot commit on top of its unpushed HEAD.
      (should (equal (funcall lead-git "rev-parse" "HEAD")
                     (funcall lead-git "rev-parse" (concat base "^"))))
      (dolist (git (list lead-git host-git))
        (should (equal
                 (format "Snapshot uncommitted work for pstack delegate %s\n\
Laptop Person <laptop@example.com>\nLaptop Person <laptop@example.com>"
                         id)
                 (funcall git "log" "-1" "--format=%B%an <%ae>%n%cn <%ce>"
                          base))))
      (should (equal "Scratch\nAn uncommitted edit"
                     (funcall host-git "show" (concat branch ":README"))))
      (should (equal "Lead" (funcall host-git "show" (concat branch
                                                            ":lead.txt"))))
      (should (equal '(("Add builder.txt" "Laptop Person <laptop@example.com>"
                        "Laptop Person <laptop@example.com>"))
                     (+remote-delegate-acp-test--branch-commits
                      lead-git branch base)))
      (should (equal (funcall host-git "rev-parse" branch)
                     (funcall lead-git "rev-parse" branch)))
      (should (equal "main" (funcall host-git "branch" "--show-current")))
      (should (equal "" (funcall host-git "status" "--porcelain")))
      (should (equal "main" (funcall lead-git "branch" "--show-current")))
      (should (equal " M README" (concat " " (funcall lead-git "status"
                                                      "--porcelain"))))
      (should (string-search (format "Ran on: local host %s"
                                     (abbreviate-file-name
                                      (directory-file-name host-root)))
                             followup))
      (should (string-search "full access" followup))
      (should (string-search (format "Branch: %s, 1 commit since %s" branch base)
                             followup))
      (should (string-search (format "Workspace on host: %s"
                                     (expand-file-name
                                      (format "arenas/%s/builder/" host-arena)
                                      host-root))
                             followup))
      (should (string-search
               (format "Started from: your uncommitted work, which Emacs \
snapshotted as commit %s on top of %s"
                       base (funcall lead-git "rev-parse" "HEAD"))
               followup))
      (should (string-search (format "git log %s..%s" base branch) followup))
      (should (string-search (format "git log %s..%s"
                                     (funcall lead-git "rev-parse" "HEAD")
                                     branch)
                             followup))
      (should-not (string-search "--ff-only" followup))
      ;; The transcript came over the polls and the final fetch.
      (let ((records (+agent-shell-arena-remote--cached-records remote)))
        (should (seq-find (lambda (record)
                            (and (equal "assistant" (alist-get 'kind record))
                                 (string-search "I will add the file."
                                                (alist-get 'text record))))
                          records))
        (should (seq-find (lambda (record)
                            (and (equal "tool" (alist-get 'kind record))
                                 (string-prefix-p "Write builder file"
                                                  (alist-get 'text record))))
                          records))
        (should-not (seq-find (lambda (record)
                                (string-search "fixture-access"
                                               (alist-get 'text record)))
                              records))
        (let ((seqs (delq 0 (mapcar (lambda (record) (alist-get 'seq record))
                                    records))))
          (should (equal seqs (seq-uniq seqs)))))
      ;; RET on the member line opens the read-only transcript, and makes no
      ;; process.
      (let ((processes (length (process-list))))
        (+agent-shell-pstack-panels)
        (with-current-buffer "*pstack panels*"
          (goto-char (point-min))
          (search-forward "transcript (")
          (+agent-shell-pstack-panels-visit))
        (with-current-buffer (+agent-shell-arena-remote--transcript-name remote)
          (should (derived-mode-p '+agent-shell-arena-remote-transcript-mode))
          (should buffer-read-only)
          (should (string-search "I will add the file." (buffer-string))))
        (should (<= (length (process-list)) processes)))
      ;; Exactly one follow-up, even after the laptop forgets and re-attaches.
      (+remote-delegate-acp-test--wait-for
       (lambda () (with-current-buffer lead
                    (save-excursion (goto-char (point-min))
                                    (search-forward "TRIAGED" nil t)))))
      (clrhash +agent-shell-arena-remote--arenas)
      (clrhash +agent-shell-pstack-panel--panels)
      (+agent-shell-arena-remote-reattach-all)
      (accept-process-output nil 1)
      (should (= 1 (with-current-buffer lead
                     (how-many "TRIAGED" (point-min) (point-max)))))
      ;; x cleans the host job and worktree, and the branch, which main
      ;; lacks, stays.
      (let ((cleaned nil))
        (cl-letf* ((message (symbol-function 'message))
                   ((symbol-function 'message)
                    (lambda (format &rest args)
                      (when (and format (string-prefix-p "Arena" format))
                        (setq cleaned (apply #'format-message format args)))
                      (apply message format args)))
                   ((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (+agent-shell-pstack-arena--clean manifest)
          (+remote-delegate-acp-test--wait-for (lambda () cleaned)))
        (should (string-search (format "kept branch %s: 1 commit on it is \
not on main" branch)
                               cleaned)))
      (should-not (file-exists-p
                   (+agent-shell-arena-git-manifest-directory manifest)))
      (should-not (file-exists-p (expand-file-name
                                  (format "arenas/%s" host-arena) host-root)))
      (should (equal "Add builder.txt"
                     (funcall lead-git "log" "-1" "--format=%s" branch)))
      (should (equal "Add builder.txt"
                     (funcall host-git "log" "-1" "--format=%s" branch)))
      (should (+agent-shell-arena-remote--cached-records remote)))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-remote-sandboxed ()
  "A sandboxed delegate on the host: refused git writes there, one commit
by Emacs as the laptop identity, fetched as the delegate's branch."
  (+remote-delegate-acp-test--skip t)
  (+remote-delegate-acp-test--with
      `(:host (local . ,host-root) :host-directory ,host-repo
              :delegate (:sandbox t))
    (let* ((panel (funcall delegated "Add the builder file."))
           (id (+agent-shell-pstack-panel-id panel))
           (branch (concat "pstack/delegate/" id))
           (base (+agent-shell-arena-git-manifest-base
                  (+agent-shell-pstack-panel-arena panel)))
           (followup (+agent-shell-pstack-panel-followup panel)))
      (should (string-search "commit=refused ref=refused config=refused \
pointer=refused" followup))
      (should (string-search "sandboxed" followup))
      (should (string-search "Sandboxed: Emacs made its one commit on the host"
                             followup))
      (should (equal '(("[builder] Add builder.txt"
                        "Laptop Person <laptop@example.com>"
                        "Laptop Person <laptop@example.com>"))
                     (+remote-delegate-acp-test--branch-commits
                      lead-git branch base)))
      (should-not (string-search "sneaky" (funcall host-git "branch" "-a"))))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-remote-cancel ()
  "Cancelling a running remote delegate stops it and keeps its commits."
  (+remote-delegate-acp-test--skip)
  (+remote-delegate-acp-test--with
      `(:host (local . ,host-root) :host-directory ,host-repo)
    (let* ((panel (funcall delegated "Add the builder file. HANG" 'started))
           (member (car (+agent-shell-pstack-panel-members panel))))
      (+remote-delegate-acp-test--wait-for
       (lambda () (eq 'running (+agent-shell-pstack-panel-member-state member))))
      ;; The running delegate's text reaches the laptop over its polls.
      (+remote-delegate-acp-test--wait-for
       (lambda ()
         (seq-find (lambda (record)
                     (string-prefix-p "PARTIAL DELEGATE" (alist-get 'text record)))
                   (+agent-shell-arena-remote--cached-records
                    (+agent-shell-pstack-panel-remote panel)))))
      (should (eq 'remote (+agent-shell-arena-remote-phase
                           (+agent-shell-pstack-panel-remote panel))))
      (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")
      (+remote-delegate-acp-test--wait-for
       (lambda () (+agent-shell-pstack-panel-delivered panel)))
      (let ((followup (+agent-shell-pstack-panel-followup panel))
            (branch (concat "pstack/delegate/"
                            (+agent-shell-pstack-panel-id panel))))
        (should (string-search "builder (fixture-codex): error.\nYou cancelled \
the panel." followup))
        (should (string-search "1 commit since" followup))
        (should (equal "Add builder.txt"
                       (funcall lead-git "log" "-1" "--format=%s" branch)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-remote-timeout ()
  "A sandboxed delegate on the host that times out without a final response
gets no commit; its work stays uncommitted on the host, and the
follow-up says where."
  (+remote-delegate-acp-test--skip t)
  (+remote-delegate-acp-test--with
      `(:host (local . ,host-root) :host-directory ,host-repo
              :delegate (:sandbox t))
    (let* ((panel (funcall delegated "Add the builder file. HANG" nil 6))
           (followup (+agent-shell-pstack-panel-followup panel))
           (branch (concat "pstack/delegate/"
                           (+agent-shell-pstack-panel-id panel)))
           (workspace (expand-file-name
                       (format "arenas/%s/builder/"
                               (+agent-shell-arena-remote-arena
                                (+agent-shell-pstack-panel-remote panel)))
                       host-root)))
      (should (string-search "builder (fixture-codex): timeout." followup))
      (should (string-search "Sandboxed: No commit created.  The delegate's \
turn did not end with a successful final response" followup))
      (should (string-search (format "Its changes stay uncommitted in %s"
                                     workspace)
                             followup))
      (should (string-search "The host worktree has 1 uncommitted path: \
builder.txt." followup))
      (should (string-search (format "Branch: %s, 0 commits since" branch)
                             followup))
      (should-not (string-search "To merge:" followup))
      (should (string-search "Partial text, not a final response:" followup))
      (should (equal "builder\n" (+remote-delegate-acp-test--read
                                  (expand-file-name "builder.txt"
                                                    workspace)))))))

(ert-deftest +agent-shell-pstack-remote-delegate-acp-test-killed-runner ()
  "A runner killed mid-run restarts, stops what the delegate left, never
reruns it, and reports its commits."
  (+remote-delegate-acp-test--skip)
  (+remote-delegate-acp-test--with
      `(:host (local . ,host-root) :host-directory ,host-repo)
    (let* ((panel (funcall delegated "Add the builder file. HANG" 'started))
           (member (car (+agent-shell-pstack-panel-members panel)))
           (remote nil) (job nil))
      (+remote-delegate-acp-test--wait-for
       (lambda () (eq 'running (+agent-shell-pstack-panel-member-state member))))
      (setq remote (+agent-shell-pstack-panel-remote panel)
            job (expand-file-name (concat "jobs/" (+agent-shell-arena-remote-id
                                                   remote))
                                  host-root))
      (let* ((lease (json-parse-string
                     (+remote-delegate-acp-test--read
                      (expand-file-name "leases/builder.json" job))
                     :object-type 'alist))
             (adapter (alist-get 'pid lease))
             (pid (string-to-number (+remote-delegate-acp-test--read
                                     (expand-file-name "pid" job)))))
        (should (natnump adapter))
        (signal-process pid 'SIGKILL)
        (+remote-delegate-acp-test--wait-for
         (lambda () (+agent-shell-pstack-panel-delivered panel)))
        (let ((followup (+agent-shell-pstack-panel-followup panel)))
          (should (equal 1 (+agent-shell-arena-remote-restarts remote)))
          (should (string-search "The runner stopped while the delegate ran; \
Emacs did not run it again." followup))
          (should (string-search "1 commit since" followup)))
        (should-not (process-attributes adapter))
        (should (= 1 (seq-count
                      (lambda (record)
                        (equal "Emacs sent the brief." (alist-get 'text record)))
                      (+agent-shell-arena-remote--cached-records remote))))))))

(provide 'agent-shell-pstack-remote-delegate-acp-test)
;;; agent-shell-pstack-remote-delegate-acp-test.el ends here
