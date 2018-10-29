;;; rk-ledger.el --- Config for ledger  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ledger-mode
  :mode ("\\.ledger$" . ledger-mode)
  :straight t)

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(provide 'rk-ledger)

;;; rk-ledger.el ends here
