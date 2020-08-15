;;; rk-envrc.el --- yay direnv  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package envrc
  :straight t
  :demand t
  :commands (envrc-global-mode)
  :if (executable-find "direnv")
  :config
  (envrc-global-mode))

(provide 'rk-envrc)

;;; rk-envrc.el ends here
