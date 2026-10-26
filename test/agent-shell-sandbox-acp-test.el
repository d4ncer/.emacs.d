;;; agent-shell-sandbox-acp-test.el --- Sandboxed shells over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; A sandboxed agent shell talks ACP to
;; test/fixtures/agent-shell-projects-agent.py under sandbox-exec, then
;; stops with every process it started.  macOS only.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require '+agent-shell-drive)

(defvar +agent-shell-sandbox-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst +agent-shell-sandbox-acp-test--delay 3
  "Seconds the fixture's late writers wait before writing.")

(defun +agent-shell-sandbox-acp-test--config ()
  "Return the fixture agent config, whose client sets PA_ENV=client."
  (agent-shell-make-agent-config
   :identifier 'fixture-codex :mode-line-name "fixture"
   :buffer-name "Sandbox fixture" :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params
                      (list +agent-shell-sandbox-acp-test--fixture)
                      :environment-variables '("PA_ENV=client")))))

(defun +agent-shell-sandbox-acp-test--wait-for (predicate &optional seconds)
  "Wait up to SECONDS, 15 by default, for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) (or seconds 15))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.03)))
  (should (funcall predicate)))

(ert-deftest +agent-shell-sandbox-acp-test-confines-and-stops ()
  "Confine a full-access agent to its root and stop its late writers.
The fixture writes inside and outside its root, reads a denied folder, and
starts two delayed writers: a child working outside the root and an orphan
working inside it."
  (skip-unless (and (eq system-type 'darwin)
                    (file-executable-p "/usr/bin/sandbox-exec")
                    (executable-find "python3")))
  (let* ((root (file-truename (make-temp-file "pa-root-" t)))
         (outside (make-temp-file "pa-outside-" t))
         (denied (make-temp-file "pa-denied-" t))
         (secret (expand-file-name "secret" denied))
         (sandbox (+agent-shell-sandbox-create root :deny-reads (list denied)))
         (tmp (+agent-shell-sandbox-tmp sandbox))
         (finished nil)
         (+agent-shell-sandbox-agents
          `((fixture-codex
             :mode "agent-full-access"
             :prepare ,(lambda (_) (list "PA_ENV=sandbox"))
             :finish ,(lambda (_) (setq finished t)))))
         (agent-shell-agent-configs
          (list (+agent-shell-sandbox-acp-test--config)))
         (agent-shell-show-welcome-message nil)
         (shell-maker-prompt-before-killing-buffer nil)
         (agent-shell-show-config-icons nil)
         (agent-shell-prefer-viewport-interaction nil)
         (agent-shell-mcp-servers nil)
         (+agent-shell-drive--watchers (make-hash-table :test #'equal))
         (+agent-shell-drive--shells (make-hash-table :test #'eq))
         (shell nil) (adapter nil) (reply nil) (stopped 'pending))
    (with-temp-file secret (insert "hidden"))
    (unwind-protect
        (save-window-excursion
          (setq shell (+agent-shell-drive-start :config 'fixture-codex
                                                :directory root
                                                :policy sandbox))
          (+agent-shell-sandbox-acp-test--wait-for
           (lambda () (and (agent-shell-session-id :shell-buffer shell)
                           (eq (agent-shell-status :shell-buffer shell)
                               'ready))))
          (should (eq sandbox (+agent-shell-drive-policy shell)))
          (should-not (memq shell (agent-shell-buffers)))
          (setq adapter (+agent-shell-drive--adapter shell))
          (+agent-shell-drive-watch
           shell 'sandbox-test 1
           (lambda (event)
             (when (eq (plist-get event :kind) 'turn-complete)
               (setq reply (plist-get event :text)))))
          (should (+agent-shell-drive-send
                   shell
                   (concat "SANDBOX-PROBE "
                           (json-encode
                            (list (cons 'outside (expand-file-name
                                                  "outside.txt" outside))
                                  (cons 'secret secret)
                                  (cons 'delay
                                        +agent-shell-sandbox-acp-test--delay))))))
          (+agent-shell-sandbox-acp-test--wait-for (lambda () reply))
          (let ((probe (json-parse-string
                        (substring reply (string-search "{" reply))
                        :object-type 'alist)))
            (should (equal "agent-full-access" (alist-get 'mode probe)))
            (should (equal "sandbox" (alist-get 'env probe)))
            (should (equal tmp (alist-get 'tmpdir probe)))
            (should (equal "ok" (alist-get 'inside probe)))
            (should (string-prefix-p "denied" (alist-get 'outside probe)))
            (should (string-prefix-p "denied" (alist-get 'secret probe))))
          (should (file-exists-p (expand-file-name "inside.txt" root)))
          (should-not (file-exists-p (expand-file-name "outside.txt" outside)))
          (+agent-shell-drive-stop shell "Stopped by test"
                                   (lambda (survivors)
                                     (setq stopped survivors)))
          (+agent-shell-sandbox-acp-test--wait-for
           (lambda () (not (eq stopped 'pending))))
          (should-not stopped)
          (should finished)
          (should-not (buffer-live-p shell))
          (should-not (process-attributes adapter))
          (sleep-for (+ 2 +agent-shell-sandbox-acp-test--delay))
          (should-not (file-exists-p (expand-file-name "late-child.txt" root)))
          (should-not (file-exists-p (expand-file-name "late-orphan.txt" root)))
          (should-not (file-exists-p (expand-file-name ".agent-shell" root))))
      (+agent-shell-drive-kill shell)
      (dolist (directory (list root outside denied tmp
                               (+agent-shell-sandbox-config sandbox)))
        (delete-directory directory t)))))

(provide 'agent-shell-sandbox-acp-test)
;;; agent-shell-sandbox-acp-test.el ends here
