;;; rk-kotlin.el --- Configuration for kotlin.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package kotlin-mode
  :straight t
  :defer t
  :config
  (progn
    (add-to-list 'exec-path "~/kotlin-language-server/server/build/install/server/bin"))
  :hook
  (kotlin-mode . lsp))

(provide 'rk-kotlin)

;;; rk-kotlin.el ends here
