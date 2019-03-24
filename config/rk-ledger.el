;;; rk-ledger.el --- Config for ledger  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(use-package ledger-mode
  :mode ("\\.ledger$" . ledger-mode)
  :straight t)
(use-package cb-ledger-format
  :after ledger-mode
  :init
  (rk-local-leader-def :keymaps 'ledger-mode-map
    "." '(cb-ledger-format-buffer :wk "format")))

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(provide 'rk-ledger)

;;; rk-ledger.el ends here
