;;; rk-ledger.el --- Config for ledger  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)

(use-package ledger-mode
  :mode ("\\.ledger$" . ledger-mode)
  :init
  (rk-local-leader-def :keymaps 'ledger-mode-map
    )
  (rk-local-leader-def :keymaps 'ledger-mode-map
    "." '(ledger-mode-clean-buffer :wk "format")
    "r" '(ledger-report :wk "report"))
  :straight t
  :config
  (general-setq ledger-reports
                `(("bal" "ledger -f %(ledger-file) bal")
                  ("reg" "ledger -f %(ledger-file) reg")
                  ("payee" "ledger -f %(ledger-file) reg @%(payee)")
                  ("account" "ledger -f %(ledger-file) reg %(account)"))))

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(provide 'rk-ledger)

;;; rk-ledger.el ends here
