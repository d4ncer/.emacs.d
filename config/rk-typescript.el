;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package typescript-mode
  :straight t
  :mode ("\\.tsx?\\'" . typescript-mode)
  :custom
  (typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook #'lsp))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
