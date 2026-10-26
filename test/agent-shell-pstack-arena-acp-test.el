;;; agent-shell-pstack-arena-acp-test.el --- pstack arenas over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; An arena end to end: a pstack lead in a scratch git repository requests
;; an arena, two fixture candidates run sandboxed in their own worktrees, a
;; fixture judge scores them, and the lead gets the follow-up.  Every agent
;; is test/fixtures/agent-shell-projects-agent.py.  macOS only.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require '+agent-shell-pstack-panel)

(defvar +agent-shell-pstack-arena-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-pstack-arena-acp-test--config (identifier)
  "Return a fixture agent config named IDENTIFIER."
  (agent-shell-make-agent-config
   :identifier identifier :mode-line-name (symbol-name identifier)
   :buffer-name (format "Arena %s" identifier) :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params
                      (list +agent-shell-pstack-arena-acp-test--fixture)))))

(defun +agent-shell-pstack-arena-acp-test--wait-for (predicate)
  "Wait up to 60 seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 60)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should (funcall predicate)))

(defun +agent-shell-pstack-arena-acp-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +agent-shell-pstack-arena-acp-test--read (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defconst +agent-shell-pstack-arena-acp-test--request
  "{\"version\":1,\"shape\":\"arena\",\"brief\":\"Add your file.\",\
\"rubric\":[\"RUBRIC-ONE correctness\",\"RUBRIC-TWO clarity\",\
\"RUBRIC-THREE tests\"],\"timeout\":60,\"judge_timeout\":60,\
\"members\":[{\"role\":\"opus\",\"agent\":\"fixture-claude\"},\
{\"role\":\"codex\",\"agent\":\"fixture-codex\",\"focus\":\"Keep it short\"}]}"
  "The arena the fixture lead requests.")

(ert-deftest +agent-shell-pstack-arena-acp-test-end-to-end ()
  "Run two sandboxed candidates and a judge, hand back the results, clean up."
  (skip-unless (and (eq system-type 'darwin)
                    (file-executable-p "/usr/bin/sandbox-exec")
                    (executable-find "python3")))
  (let* ((scratch (file-name-as-directory
                   (file-truename (make-temp-file "pstack-arena-acp-" t))))
         (repo (expand-file-name "repo/" scratch))
         (gitconfig (expand-file-name "gitconfig" scratch))
         (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                    gitconfig)
                                            "GIT_CONFIG_NOSYSTEM=1")
                                      process-environment))
         (+agent-shell-pstack-arena-directory
          (expand-file-name "arenas/" scratch))
         (+agent-shell-pstack-arena-dependencies nil)
         (+agent-shell-pstack-panel-directory
          (expand-file-name "reports/" scratch))
         (agent-shell-show-welcome-message nil)
         (shell-maker-prompt-before-killing-buffer nil)
         (agent-shell-show-config-icons nil)
         (agent-shell-prefer-viewport-interaction nil)
         (agent-shell-mcp-servers nil)
         (agent-shell-transcript-file-path-function nil)
         (agent-shell-agent-configs
          (list (+agent-shell-pstack-arena-acp-test--config 'fixture-codex)
                (+agent-shell-pstack-arena-acp-test--config 'fixture-claude)))
         (+agent-shell-drive-read-only-modes
          '((fixture-codex . "read-only") (fixture-claude . "plan")))
         (prepare (lambda (sandbox)
                    (list (concat "TMPDIR=" (+agent-shell-sandbox-tmp sandbox))
                          (concat "PA_LEAD=" (expand-file-name "README"
                                                               repo)))))
         (+agent-shell-sandbox-agents
          `((fixture-codex :mode "agent-full-access" :prepare ,prepare)
            (fixture-claude :mode "agent-full-access" :prepare ,prepare)))
         (+agent-shell-pstack-invocations '((fixture-codex . "$poteto-mode")))
         (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
         (+agent-shell-drive--watchers (make-hash-table :test #'equal))
         (+agent-shell-drive--shells (make-hash-table :test #'eq))
         (+agent-shell-pstack-prime-texts nil)
         (+agent-shell-pstack-reminder-texts nil)
         (+agent-shell-pstack-mode-hook nil)
         (+agent-shell-pstack-panel-mode nil)
         (git (lambda (&rest args)
                (apply #'+agent-shell-pstack-arena-acp-test--git repo args)))
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
          (setq lead (+agent-shell-drive-start :config 'fixture-codex
                                               :directory repo))
          (+agent-shell-pstack-arena-acp-test--wait-for
           (lambda () (and (agent-shell-session-id :shell-buffer lead)
                           (eq (agent-shell-status :shell-buffer lead)
                               'ready))))
          (with-current-buffer lead (+agent-shell-pstack-mode 1))
          (should (+agent-shell-drive-send
                   lead (concat "PSTACK-LEAD "
                                +agent-shell-pstack-arena-acp-test--request)))
          (+agent-shell-pstack-arena-acp-test--wait-for
           (lambda ()
             (setq panel (car (+agent-shell-pstack-panel-list)))
             (and panel (+agent-shell-pstack-panel-delivered panel))))
          (let* ((manifest (+agent-shell-pstack-panel-arena panel))
                 (arena (+agent-shell-arena-git-manifest-directory manifest))
                 (base (+agent-shell-arena-git-manifest-base manifest))
                 (members (+agent-shell-pstack-panel-members panel))
                 (candidates (butlast members))
                 (judge (car (last members)))
                 (followup (+agent-shell-pstack-panel-followup panel))
                 (report (+agent-shell-pstack-arena-acp-test--read
                          (+agent-shell-pstack-panel-member-report judge))))
            (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
            (should (equal '(done done done)
                           (mapcar #'+agent-shell-pstack-panel-member-state
                                   members)))
            (dolist (candidate candidates)
              (let* ((role (+agent-shell-pstack-panel-member-role candidate))
                     (result (+agent-shell-pstack-panel-member-result
                              candidate))
                     (ref (format "refs/pstack-arena/%s/%s"
                                  (+agent-shell-arena-git-manifest-id manifest)
                                  role))
                     (sandbox (+agent-shell-pstack-panel-member-sandbox
                               candidate)))
                (should (eq 'captured
                            (+agent-shell-pstack-panel-member-capture
                             candidate)))
                (should (equal ref (plist-get result :ref)))
                (should (equal (concat role ".txt")
                               (funcall git "diff" "--name-only" base ref)))
                (should (equal (concat role "\n")
                               (+agent-shell-pstack-arena-acp-test--read
                                (expand-file-name (concat role ".txt")
                                                  (plist-get result
                                                             :checkout)))))
                (should (string-prefix-p
                         (format "RATIONALE role=%s mode=agent-full-access \
rubric=unseen lead=denied git=ok." role)
                         (+agent-shell-pstack-panel-member-text candidate)))
                (should-not (file-exists-p (+agent-shell-sandbox-tmp sandbox)))
                (should (string-match-p
                         (regexp-quote
                          (format "git diff --binary %s %s | git apply --3way"
                                  base (plist-get result :commit)))
                         followup))))
            (should (string-prefix-p "SCORES mode=plan\n" report))
            (dolist (expected '("## Candidate A\nIt finished its turn."
                                "## Candidate B\nIt finished its turn."
                                "1. RUBRIC-ONE correctness"
                                "3. RUBRIC-THREE tests"
                                "RATIONALE role=opus"
                                "RATIONALE role=codex"))
              (should (string-search expected report)))
            (should (string-search (expand-file-name "checkouts/opus/" arena)
                                   report))
            (should (string-match-p "## judge (fixture-claude): done" followup))
            (should (string-match-p "Pick, graft and verify" followup))
            (+agent-shell-pstack-arena-acp-test--wait-for
             (lambda () (with-current-buffer lead
                          (save-excursion
                            (goto-char (point-min))
                            (search-forward "TRIAGED" nil t)))))
            (should (string-match-p "pstack-arena"
                                    (funcall git "worktree" "list")))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
              (+agent-shell-pstack-panels)
              (with-current-buffer "*pstack panels*"
                (goto-char (point-min))
                (search-forward "Arena:")
                (+agent-shell-pstack-panels-clean)))
            (+agent-shell-pstack-arena-acp-test--wait-for
             (lambda () (not (file-exists-p arena))))
            (should (equal "" (funcall git "for-each-ref" "refs/pstack-arena/")))
            (should (equal 1 (length (split-string
                                      (funcall git "worktree" "list")
                                      "\n" t))))
            (should (equal "" (funcall git "status" "--porcelain")))))
      (+agent-shell-pstack-panel-mode -1)
      (when panel
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (when-let* ((sandbox (+agent-shell-pstack-panel-member-sandbox
                                member)))
            (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
            (delete-directory (+agent-shell-sandbox-config sandbox) t))
          (+agent-shell-drive-kill
           (+agent-shell-pstack-panel-member-shell member))))
      (+agent-shell-drive-kill lead)
      (when (get-buffer "*pstack panels*") (kill-buffer "*pstack panels*"))
      (delete-directory scratch t))))

(provide 'agent-shell-pstack-arena-acp-test)
;;; agent-shell-pstack-arena-acp-test.el ends here
