;;; agent-shell-pstack-arena-remote-acp-test.el --- Remote pstack arenas over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; A remote arena end to end on a (local . DIRECTORY) host, whose commands
;; run as local processes: a pstack lead in a scratch clone with an
;; uncommitted change requests an arena, a "host" clone of the same
;; origin at a newer commit takes the job, and a batch Emacs runner there
;; runs two sandboxed fixture candidates and a fixture judge.  Every agent
;; is test/fixtures/agent-shell-projects-agent.py.  macOS only, since the
;; local host still uses this machine's sandbox.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require '+agent-shell-pstack-panel)
(require '+agent-shell-arena-remote)
(require '+agent-shell-egress)

(defvar +agent-shell-pstack-arena-remote-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-pstack-arena-remote-acp-test--capabilities ()
  "Return the capabilities the local test host declares in ready.json.
It has git and the fixture adapters, and on Linux the sandbox's
namespaces; its runner starts under nohup, not systemd."
  `((git . 1) (node_adapters . 1)
    ,@(when (eq +agent-shell-sandbox-backend 'bwrap)
        '((linux_namespaces . 1) (bwrap . 1) (slirp4netns . 1)))))

(defun +agent-shell-pstack-arena-remote-acp-test--config (identifier)
  "Return a fixture agent config named IDENTIFIER."
  (agent-shell-make-agent-config
   :identifier identifier :mode-line-name (symbol-name identifier)
   :buffer-name (format "Remote arena %s" identifier) :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params
                      (list +agent-shell-pstack-arena-remote-acp-test--fixture)))))

(defun +agent-shell-pstack-arena-remote-acp-test--wait-for (predicate
                                                            &optional seconds)
  "Wait up to SECONDS, 90 by default, for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) (or seconds 90))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun +agent-shell-pstack-arena-remote-acp-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +agent-shell-pstack-arena-remote-acp-test--read (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun +agent-shell-pstack-arena-remote-acp-test--request (&optional focus)
  "Return the arena request, with FOCUS for both candidates when non-nil."
  (format "{\"version\":1,\"shape\":\"arena\",\"brief\":\"Add your file.\",\
\"rubric\":[\"RUBRIC-ONE correctness\",\"RUBRIC-TWO clarity\",\
\"RUBRIC-THREE tests\"],\"timeout\":60,\"judge_timeout\":60,\
\"members\":[{\"role\":\"opus\",\"agent\":\"fixture-claude\"%s},\
{\"role\":\"codex\",\"agent\":\"fixture-codex\",\"focus\":\"%s\"}]}"
          (if focus (format ",\"focus\":\"%s\"" focus) "")
          (or focus "Keep it short")))

(defun +agent-shell-pstack-arena-remote-acp-test--runner-init (file)
  "Write the host's runner-init.el, FILE, configuring the fixture agents."
  (with-temp-file file
    (insert ";;; runner-init.el --- fixture agents -*- lexical-binding: t; -*-\n")
    (prin1
     `(progn
        (defun pa-test-config (identifier)
          (agent-shell-make-agent-config
           :identifier identifier :mode-line-name (symbol-name identifier)
           :buffer-name (format "Runner %s" identifier)
           :shell-prompt "Fixture> " :shell-prompt-regexp "Fixture> "
           :client-maker
           (lambda (buffer)
             (acp-make-client :context-buffer buffer
                              :command ,(executable-find "python3")
                              :command-params
                              (list ,+agent-shell-pstack-arena-remote-acp-test--fixture)))))
        (defun pa-test-prepare (_sandbox)
          (list (concat "PA_LEAD="
                        (expand-file-name
                         "job.json"
                         (+agent-shell-arena-remote--runner-directory
                          +agent-shell-arena-remote--runner)))))
        (setq agent-shell-agent-configs
              (list (pa-test-config 'fixture-codex)
                    (pa-test-config 'fixture-claude))
              +agent-shell-drive-read-only-modes
              '((fixture-codex . "read-only") (fixture-claude . "plan"))
              +agent-shell-sandbox-agents
              '((fixture-codex :mode "agent-full-access"
                               :prepare pa-test-prepare
                               :login (lambda ()
                                        (+agent-shell-sandbox--make-login
                                         :agent 'fixture-codex
                                         :access "fixture-access"
                                         :expires (+ (float-time) 86400))))
                (fixture-claude :mode "agent-full-access"
                                :prepare pa-test-prepare
                               :login (lambda ()
                                        (+agent-shell-sandbox--make-login
                                         :agent 'fixture-claude
                                         :access "fixture-access"
                                         :expires (+ (float-time) 86400)))))
              agent-shell-show-welcome-message nil
              agent-shell-show-config-icons nil
              agent-shell-prefer-viewport-interaction nil
              agent-shell-mcp-servers nil
              +agent-shell-arena-remote-tick 0.3
              +agent-shell-arena-remote-heartbeat 1))
     (current-buffer))
    (insert "\n")))

(defun +agent-shell-pstack-arena-remote-acp-test--normalize (text roots)
  "Return follow-up TEXT with ROOTS, ids and commits replaced by tokens.
The line naming the host goes too, so a remote follow-up compares with a
local one."
  (dolist (root roots)
    (setq text (string-replace (file-name-as-directory root) "<root>/" text)))
  (replace-regexp-in-string
   "^It ran on .*\n" ""
   (replace-regexp-in-string
    "[0-9a-f]\\{12\\}-[0-9a-f]\\{8\\}" "<arena>"
    (replace-regexp-in-string "\\b[0-9a-f]\\{40\\}\\b" "<commit>" text))))

(defmacro +agent-shell-pstack-arena-remote-acp-test--with (&rest body)
  "Run BODY with a lead, an origin, a host checkout and a local host.
BODY sees `scratch', `lead', `lead-repo', `host-repo', `host-root',
`merge-base' (the commit both checkouts share), `host-tip' (the host's
newer commit), the functions `lead-git', `host-git' and `request', which
sends a request and returns its panel once the panel has a remote
record, and `delivered', which waits for a panel's follow-up."
  (declare (indent 0))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "pstack-remote-acp-" t))))
          (gitconfig (expand-file-name "gitconfig" scratch))
          (origin (expand-file-name "origin.git" scratch))
          (lead-repo (expand-file-name "lead/" scratch))
          (host-repo (expand-file-name "host/" scratch))
          (host-root (expand-file-name "host-root/" scratch))
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                     gitconfig)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" scratch))
          (+agent-shell-pstack-arena-profiles
           `((,lead-repo :host (local . ,host-root)
                         :host-directory ,host-repo)))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (agent-shell-show-welcome-message nil)
          (shell-maker-prompt-before-killing-buffer nil)
          (agent-shell-show-config-icons nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-mcp-servers nil)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-agent-configs
           (list (+agent-shell-pstack-arena-remote-acp-test--config
                  'fixture-codex)
                 (+agent-shell-pstack-arena-remote-acp-test--config
                  'fixture-claude)))
          (+agent-shell-drive-read-only-modes
           '((fixture-codex . "read-only") (fixture-claude . "plan")))
          (prepare (lambda (_)
                     (list (concat "PA_LEAD=" (expand-file-name "README"
                                                                lead-repo)))))
          (+agent-shell-sandbox-agents
           `((fixture-codex :mode "agent-full-access" :prepare ,prepare
             :login ,(lambda ()
                       (+agent-shell-sandbox--make-login
                        :agent 'fixture-codex :access "fixture-access"
                        :expires (+ (float-time) 86400))))
             (fixture-claude :mode "agent-full-access" :prepare ,prepare
             :login ,(lambda ()
                       (+agent-shell-sandbox--make-login
                        :agent 'fixture-claude :access "fixture-access"
                        :expires (+ (float-time) 86400))))))
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
          (+agent-shell-arena-remote-max-interval 2)
          (+agent-shell-arena-remote-stale-after 3)
          (+agent-shell-arena-remote-unreachable-after 120)
          (tmp-before (directory-files "/private/tmp" t "\\`pa-[0-9a-f]\\{8\\}\\'"))
          (lead-git (lambda (&rest args)
                      (apply #'+agent-shell-pstack-arena-remote-acp-test--git
                             lead-repo args)))
          (host-git (lambda (&rest args)
                      (apply #'+agent-shell-pstack-arena-remote-acp-test--git
                             host-repo args)))
          (lead nil) (merge-base nil) (host-tip nil)
          (request
           (lambda (text)
             (should (+agent-shell-drive-send lead (concat "PSTACK-LEAD " text)))
             (let ((before (+agent-shell-pstack-panel-list)) (panel nil))
               (+agent-shell-pstack-arena-remote-acp-test--wait-for
                (lambda ()
                  (setq panel (car (seq-difference
                                    (+agent-shell-pstack-panel-list) before #'eq)))
                  (and panel (+agent-shell-pstack-panel-remote panel))))
               panel)))
          (delivered
           (lambda (panel)
             (+agent-shell-pstack-arena-remote-acp-test--wait-for
              (lambda () (+agent-shell-pstack-panel-delivered panel)))
             (+agent-shell-pstack-panel-followup panel))))
     (ignore request delivered host-tip merge-base)
     (with-temp-file gitconfig
       (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
               "[init]\n\tdefaultBranch = main\n"
               "[commit]\n\tgpgSign = false\n"))
     (+agent-shell-pstack-arena-remote-acp-test--git
      scratch "init" "-q" "--bare" origin)
     (+agent-shell-pstack-arena-remote-acp-test--git
      scratch "clone" "-q" origin lead-repo)
     (with-temp-file (expand-file-name "README" lead-repo) (insert "Scratch\n"))
     (funcall lead-git "add" "-A")
     (funcall lead-git "commit" "-q" "-m" "initial")
     (funcall lead-git "push" "-q" "origin" "main")
     (funcall lead-git "remote" "set-head" "origin" "main")
     (setq merge-base (funcall lead-git "rev-parse" "HEAD"))
     (+agent-shell-pstack-arena-remote-acp-test--git
      scratch "clone" "-q" origin host-repo)
     (with-temp-file (expand-file-name "host.txt" host-repo) (insert "Host\n"))
     (funcall host-git "add" "-A")
     (funcall host-git "commit" "-q" "-m" "newer upstream work")
     (funcall host-git "push" "-q" "origin" "main")
     (funcall host-git "fetch" "-q")
     (setq host-tip (funcall host-git "rev-parse" "HEAD"))
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
                  (capabilities
                   ,@(+agent-shell-pstack-arena-remote-acp-test--capabilities))))))
     (+agent-shell-pstack-arena-remote-acp-test--runner-init
      (expand-file-name "runner-init.el" host-root))
     (unwind-protect
         (save-window-excursion
           (+agent-shell-pstack-panel-mode 1)
           (setq lead (+agent-shell-drive-start :config 'fixture-codex
                                                :directory lead-repo))
           (+agent-shell-pstack-arena-remote-acp-test--wait-for
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
         (dolist (member (+agent-shell-pstack-panel-members panel))
           (+agent-shell-drive-kill
            (+agent-shell-pstack-panel-member-shell member))))
       (+agent-shell-drive-kill lead)
       (dolist (directory (directory-files (expand-file-name "jobs/" host-root)
                                           t "\\`[^.]"))
         (when-let* ((pid (ignore-errors
                            (string-to-number
                             (+agent-shell-pstack-arena-remote-acp-test--read
                              (expand-file-name "pid" directory)))))
                     ((> pid 0)))
           (ignore-errors (signal-process pid 'SIGKILL))))
       (dolist (tmp (seq-difference
                     (directory-files "/private/tmp" t
                                      "\\`pa-[0-9a-f]\\{8\\}\\'")
                     tmp-before))
         (ignore-errors (delete-directory tmp t)))
       (when (get-buffer "*pstack panels*") (kill-buffer "*pstack panels*"))
       (ignore-errors (delete-directory scratch t)))))

(defmacro +agent-shell-pstack-arena-remote-acp-test--skip ()
  "Skip unless this machine can run a local host's sandboxed candidates."
  '(skip-unless (and (eq system-type 'darwin)
                     (file-executable-p "/usr/bin/sandbox-exec")
                     (executable-find "python3"))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-end-to-end ()
  "Run an arena on the host, mirror its status, land its results, clean up.
The follow-up matches the one the same arena gives when run locally."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (+agent-shell-pstack-arena-remote-acp-test--with
    (let* ((local-followup
            (let ((+agent-shell-pstack-arena-profiles nil))
              (let ((panel (progn
                             (should (+agent-shell-drive-send
                                      lead
                                      (concat
                                       "PSTACK-LEAD "
                                       (+agent-shell-pstack-arena-remote-acp-test--request))))
                             (+agent-shell-pstack-arena-remote-acp-test--wait-for
                              (lambda () (car (+agent-shell-pstack-panel-list))))
                             (car (+agent-shell-pstack-panel-list)))))
                (prog1 (funcall delivered panel)
                  (remhash (+agent-shell-pstack-panel-id panel)
                           +agent-shell-pstack-panel--panels)))))
           (mirrored nil)
           (note (lambda (remote status)
                   (push (list (+agent-shell-pstack-panel-phase
                                (+agent-shell-arena-remote--panel remote))
                               (+agent-shell-arena-remote-status-phase status)
                               (mapcar #'+agent-shell-arena-remote-member-state
                                       (+agent-shell-arena-remote-status-members
                                        status))
                               (mapcar #'+agent-shell-pstack-panel-member-state
                                       (+agent-shell-pstack-panel-members
                                        (+agent-shell-arena-remote--panel
                                         remote))))
                         mirrored))))
      (+agent-shell-pstack-arena-remote-acp-test--wait-for
       (lambda () (with-current-buffer lead
                    (save-excursion (goto-char (point-min))
                                    (search-forward "TRIAGED" nil t)))))
      (advice-add '+agent-shell-arena-remote--mirror :after note)
      (unwind-protect
          (let* ((panel (funcall request
                                 (+agent-shell-pstack-arena-remote-acp-test--request)))
                 (followup (funcall delivered panel))
                 (remote (+agent-shell-pstack-panel-remote panel))
                 (id (+agent-shell-arena-remote-id remote))
                 (manifest (+agent-shell-pstack-panel-arena panel))
                 (base (+agent-shell-arena-git-manifest-base manifest))
                 (arena (+agent-shell-arena-git-manifest-directory manifest))
                 (job (+agent-shell-arena-remote--read-json
                       (expand-file-name "job.json" arena) 'job))
                 (host-arena (+agent-shell-arena-remote-arena remote)))
            (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
            (should (eq 'finished (+agent-shell-arena-remote-phase remote)))
            ;; The host's tip is unknown here, so the bundle is cut at
            ;; the merge base with this clone's upstream: only the local
            ;; commit and the snapshot cross.
            (should (equal merge-base
                           (+agent-shell-arena-remote-job-prerequisite job)))
            (should (string-match-p
                     (concat "requires this ref:?\n" merge-base)
                     (with-temp-buffer
                       (let ((default-directory lead-repo))
                         (call-process "git" nil t nil "bundle" "verify"
                                       (expand-file-name "base.bundle" arena)))
                       (buffer-string))))
            (should (equal
                     (list (format "%s refs/pstack-arena/%s/base" base id))
                     (split-string (funcall lead-git "bundle" "list-heads"
                                            (expand-file-name "base.bundle"
                                                              arena))
                                   "\n" t)))
            (should (equal "2" (funcall host-git "rev-list" "--count"
                                        (format "refs/pstack-arena/%s/base" id)
                                        (concat "^" merge-base))))
            (should (equal "Scratch\nAn uncommitted edit"
                           (funcall host-git "show"
                                    (format "refs/pstack-arena/%s/base:README"
                                            id))))
            (should-not (equal host-tip merge-base))
            ;; The panel showed the runner's states while it ran.
            (should (seq-find (lambda (seen)
                                (and (eq 'remote (nth 0 seen))
                                     (memq (nth 1 seen)
                                           '(preparing candidates capturing
                                                       judging))))
                              mirrored))
            (should (seq-every-p (lambda (seen) (equal (nth 2 seen) (nth 3 seen)))
                                 mirrored))
            (should (member '(done done done) (mapcar (lambda (seen) (nth 3 seen))
                                                      mirrored)))
            (dolist (role '("opus" "codex"))
              (let ((ref (format "refs/pstack-arena/%s/%s" id role)))
                (should (equal (concat role ".txt")
                               (funcall lead-git "diff" "--name-only" base ref)))
                (should (equal role (funcall lead-git "show"
                                             (format "%s:%s.txt" ref role))))
                (should (string-search
                         (format "git diff --binary %s %s | git apply --3way"
                                 base (funcall lead-git "rev-parse" ref))
                         followup))))
            (should (equal
                     (+agent-shell-pstack-arena-remote-acp-test--normalize
                      local-followup (list +agent-shell-pstack-arena-directory))
                     (+agent-shell-pstack-arena-remote-acp-test--normalize
                      followup (list +agent-shell-pstack-arena-directory
                                     (expand-file-name "arenas/" host-root)))))
            (should (string-search (format "It ran on local host %s \
(Emacs %s), logins: fixture-claude, 24.0 h left; fixture-codex, 24.0 h \
left.\n"
                                           (abbreviate-file-name
                                            (directory-file-name host-root))
                                           emacs-version)
                                   followup))
            (+agent-shell-pstack-arena-remote-acp-test--wait-for
             (lambda () (with-current-buffer lead
                          (save-excursion
                            (goto-char (point-min))
                            (and (search-forward "TRIAGED" nil t)
                                 (search-forward "TRIAGED" nil t))))))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
              (+agent-shell-pstack-arena--clean manifest))
            (+agent-shell-pstack-arena-remote-acp-test--wait-for
             (lambda () (not (file-exists-p arena))))
            (should (equal "" (funcall lead-git "for-each-ref"
                                       (format "refs/pstack-arena/%s/" id))))
            (should-not (file-exists-p (expand-file-name (concat "jobs/" id)
                                                         host-root)))
            (should-not (file-exists-p (expand-file-name
                                        (concat "arenas/" host-arena)
                                        host-root)))
            (should (equal "" (funcall host-git "for-each-ref"
                                       "refs/pstack-arena/")))
            (should (equal 1 (length (split-string
                                      (funcall host-git "worktree" "list")
                                      "\n" t)))))
        (advice-remove '+agent-shell-arena-remote--mirror note)))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-egress ()
  "Ship the egress switch to the host, which records every member, and
bring each record back beside the laptop's reports."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (let ((+agent-shell-pstack-arena-measure-egress t))
    (+agent-shell-pstack-arena-remote-acp-test--with
      (let* ((panel (funcall request
                             (+agent-shell-pstack-arena-remote-acp-test--request)))
             (followup (funcall delivered panel))
             (id (+agent-shell-pstack-panel-id panel))
             (job (expand-file-name
                   (concat "jobs/" (+agent-shell-arena-remote-id
                                    (+agent-shell-pstack-panel-remote panel)))
                   host-root)))
        (should (string-match-p
                 ;; A member faster than the first sample has none.
                 (format "^Egress ([a-z]+): opus %s, codex %s, judge %s; \
records: %s\\.$"
                         "\\(0 destinations\\|unavailable (no-samples)\\)"
                         "\\(0 destinations\\|unavailable (no-samples)\\)"
                         "\\(0 destinations\\|unavailable (no-samples)\\)"
                         (regexp-quote
                          (expand-file-name
                           (concat id "/*.egress.json")
                           +agent-shell-pstack-panel-directory)))
                 followup))
        (should (equal
                 (cl-list* "report.json" "results.bundle"
                           (mapcar (lambda (role)
                                     (format "reports/%s/%s.egress.json" id
                                             role))
                                   '("opus" "codex" "judge")))
                 (split-string (+agent-shell-pstack-arena-remote-acp-test--read
                                (expand-file-name "results.files" job))
                               "\n" t)))
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (let* ((role (+agent-shell-pstack-panel-member-role member))
                 (host (expand-file-name
                        (format "reports/%s/%s.egress.json" id role) job))
                 (laptop (+agent-shell-pstack-panel-member-egress-file member))
                 (identity (list :panel id :role role
                                 :kind (+agent-shell-pstack-panel-member-kind
                                        member))))
            (should (equal laptop (expand-file-name
                                   (format "%s/%s.egress.json" id role)
                                   +agent-shell-pstack-panel-directory)))
            (should (memq (+agent-shell-egress-record-state
                           (+agent-shell-egress-read laptop identity))
                          '(partial unavailable)))
            (should (equal (plist-get (+agent-shell-egress-record-identity
                                       (+agent-shell-egress-read host identity))
                                      :arena)
                           (plist-get (+agent-shell-egress-record-identity
                                       (+agent-shell-egress-read
                                        laptop identity))
                                      :arena)))))))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-cancel-before-start ()
  "A cancel that reaches a queued runner ends the arena before candidates."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (+agent-shell-pstack-arena-remote-acp-test--with
    (let ((lock (+agent-shell-arena-remote--lock-file host-root host-repo)))
      (make-directory lock t)
      (with-temp-file (expand-file-name "owner" lock)
        (insert (format "%d \"another-job\"\n" (emacs-pid))))
      (let* ((panel (funcall request
                             (+agent-shell-pstack-arena-remote-acp-test--request)))
             (remote (+agent-shell-pstack-panel-remote panel)))
        (+agent-shell-pstack-arena-remote-acp-test--wait-for
         (lambda () (eq 'queued (+agent-shell-arena-remote-runner-phase remote))))
        (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")
        (let ((followup (funcall delivered panel)))
          (should (eq 'sent (+agent-shell-arena-remote-cancel-state remote)))
          (should (string-search "0 of 2 candidates changed the base" followup))
          (should (string-search "## A: opus (fixture-claude): error\nYou \
cancelled the panel.\nNo result." followup))
          (should (string-search "## judge (fixture-claude): error\nYou \
cancelled the panel." followup))
          (let ((arenas (expand-file-name "arenas/" host-root)))
            (should-not (and (file-directory-p arenas)
                             (directory-files arenas nil "\\`[^.]"))))
          (should (file-directory-p lock)))))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-cancel-mid-run ()
  "A cancel mid-run captures the candidates' work and skips the judge."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (+agent-shell-pstack-arena-remote-acp-test--with
    (let* ((panel (funcall request
                           (+agent-shell-pstack-arena-remote-acp-test--request
                            "HANG")))
           (candidates (butlast (+agent-shell-pstack-panel-members panel))))
      (+agent-shell-pstack-arena-remote-acp-test--wait-for
       (lambda () (seq-every-p (lambda (member)
                                 (eq 'running
                                     (+agent-shell-pstack-panel-member-state
                                      member)))
                               candidates)))
      (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")
      (let* ((followup (funcall delivered panel))
             (id (+agent-shell-arena-remote-id
                  (+agent-shell-pstack-panel-remote panel))))
        (should (string-search "2 of 2 candidates changed the base" followup))
        (should (string-search "## A: opus (fixture-claude): error\nYou \
cancelled the panel.\nResult: " followup))
        (should (string-search "## judge (fixture-claude): error\nYou \
cancelled the panel." followup))
        (dolist (role '("opus" "codex"))
          (should (equal role (funcall lead-git "show"
                                       (format "refs/pstack-arena/%s/%s:%s.txt"
                                               id role role)))))))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-killed-runner ()
  "A runner killed mid-run is restarted and reports its candidates' work."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (+agent-shell-pstack-arena-remote-acp-test--with
    (let* ((panel (funcall request
                           (+agent-shell-pstack-arena-remote-acp-test--request
                            "HANG")))
           (remote (+agent-shell-pstack-panel-remote panel))
           (id (+agent-shell-arena-remote-id remote))
           (candidates (butlast (+agent-shell-pstack-panel-members panel))))
      (+agent-shell-pstack-arena-remote-acp-test--wait-for
       (lambda () (seq-every-p (lambda (member)
                                 (eq 'running
                                     (+agent-shell-pstack-panel-member-state
                                      member)))
                               candidates)))
      (let ((pid (string-to-number
                  (+agent-shell-pstack-arena-remote-acp-test--read
                   (expand-file-name (format "jobs/%s/pid" id) host-root)))))
        (should (> pid 0))
        (signal-process pid 'SIGKILL))
      (let ((followup (funcall delivered panel)))
        (should (equal 1 (+agent-shell-arena-remote-restarts remote)))
        (should (string-search "2 of 2 candidates changed the base" followup))
        (should (string-search "## A: opus (fixture-claude): timeout\nThe \
runner stopped while this candidate ran" followup))
        (should (string-search "## judge (fixture-claude): error\nThe runner \
stopped before the judge finished." followup))
        (dolist (role '("opus" "codex"))
          (should (equal role (funcall lead-git "show"
                                       (format "refs/pstack-arena/%s/%s:%s.txt"
                                               id role role)))))))))

(ert-deftest +agent-shell-pstack-arena-remote-acp-test-laptop-restart ()
  "After the laptop forgets its panels, it re-attaches and delivers."
  (+agent-shell-pstack-arena-remote-acp-test--skip)
  (+agent-shell-pstack-arena-remote-acp-test--with
    (let* ((panel (funcall request
                           (+agent-shell-pstack-arena-remote-acp-test--request)))
           (remote (+agent-shell-pstack-panel-remote panel))
           (id (+agent-shell-arena-remote-id remote))
           (panel-id (+agent-shell-pstack-panel-id panel))
           (status (expand-file-name (format "jobs/%s/status.json" id) host-root)))
      (+agent-shell-pstack-arena-remote-acp-test--wait-for
       (lambda () (eq 'remote (+agent-shell-arena-remote-phase remote))))
      (maphash (lambda (_ remote)
                 (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
                   (cancel-timer timer)))
               +agent-shell-arena-remote--arenas)
      (clrhash +agent-shell-arena-remote--arenas)
      (clrhash +agent-shell-arena-remote--lines)
      (clrhash +agent-shell-pstack-panel--panels)
      (+agent-shell-pstack-arena-remote-acp-test--wait-for
       (lambda () (and (file-exists-p status)
                       (eq 'finished
                           (+agent-shell-arena-remote-status-phase
                            (+agent-shell-arena-remote--read-json
                             status 'status))))))
      (should-not (+agent-shell-pstack-panel--find panel-id))
      (+agent-shell-arena-remote-reattach-all)
      (let* ((restored (+agent-shell-pstack-panel--find panel-id))
             (followup (funcall delivered restored)))
        (should-not (eq restored panel))
        (should (eq lead (+agent-shell-pstack-panel-lead restored)))
        (should (string-search "2 of 2 candidates changed the base" followup))
        (should (equal "codex" (funcall lead-git "show"
                                        (format "refs/pstack-arena/%s/codex:codex.txt"
                                                id))))
        (should (+agent-shell-arena-remote-delivered
                 (+agent-shell-arena-remote--read-json
                  (expand-file-name
                   "remote.json"
                   (+agent-shell-arena-git-manifest-directory
                    (+agent-shell-pstack-panel-arena restored)))
                  'record)))
        (+agent-shell-pstack-arena-remote-acp-test--wait-for
         (lambda () (with-current-buffer lead
                      (save-excursion (goto-char (point-min))
                                      (search-forward "TRIAGED" nil t)))))))))

(provide 'agent-shell-pstack-arena-remote-acp-test)
;;; agent-shell-pstack-arena-remote-acp-test.el ends here
