;;; config-activation-test.el --- Integration activation checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Evaluate real declarations with the configuration's deferral settings.

;;; Code:

(require 'config-test-support)

(ert-deftest +config-activation-hook-names ()
  (let (after-init-hook after-init agent-shell-mode-hook agent-shell-mode)
    (eval (+config-test-declaration "modules/mod-completion.el" 'minibuf-eldef) t)
    (should (memq 'minibuffer-electric-default-mode after-init-hook))
    (should-not after-init)
    (eval (+config-test-declaration "modules/mod-ai.el" 'agent-shell-math-renderer) t)
    (should-not agent-shell-mode)
    ;; :after delays registration until both dependencies have loaded.
    (should (string-match-p "agent-shell-mode-hook"
                            (prin1-to-string
                             (macroexpand
                              (+config-test-declaration
                               "modules/mod-ai.el" 'agent-shell-math-renderer)))))))

(ert-deftest +config-activation-agent-integrations ()
  (require 'agent-shell)
  (dolist (package '(agent-shell-notify +agent-shell-mermaid +agent-shell-perf))
    (eval (+config-test-declaration "modules/mod-ai.el" package) t)
    (should (featurep package)))
  (should agent-shell-notify-mode)
  (should +agent-shell-mermaid-mode)
  (should +agent-shell-perf-mode)
  (agent-shell-notify-mode -1)
  (+agent-shell-mermaid-mode -1)
  (+agent-shell-perf-mode -1))

(ert-deftest +config-activation-environment-order ()
  (let (events)
    (cl-letf (((symbol-function 'exec-path-from-shell-initialize)
               (lambda () (push 'shell events)))
              ((symbol-function 'keychain-refresh-environment)
               (lambda () (push 'keychain events)))
              ((symbol-function 'executable-find) (lambda (&rest _) "/fake/keychain")))
      (require 'exec-path-from-shell)
      (require 'keychain-environment)
      ;; Install stubs after loading the libraries' definitions.
      (cl-letf (((symbol-function 'exec-path-from-shell-initialize)
                 (lambda () (push 'shell events)))
                ((symbol-function 'keychain-refresh-environment)
                 (lambda () (push 'keychain events))))
        (let ((system-type 'darwin))
          (eval (+config-test-declaration "modules/mod-core.el"
                                         'exec-path-from-shell) t)
          (eval (+config-test-declaration "modules/mod-core.el"
                                         'keychain-environment) t)))
      (should (equal (nreverse events) '(shell keychain))))))

(provide 'config-activation-test)
;;; config-activation-test.el ends here
