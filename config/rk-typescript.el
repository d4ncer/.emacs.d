;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)
(require 'add-node-modules-path)
(autoload 'lsp-diagnostics-flycheck-enable "lsp-diagnostics")

(use-package typescript-mode
  :straight t
  :mode ("\\.tsx?\\'" . typescript-mode)
  :preface
  (defun rk-ts--add-eslint (&rest _)
    (flycheck-add-next-checker 'lsp 'javascript-eslint))
  (defun rk-ts--setup-typescript ()
    (add-node-modules-path)
    (lsp))
  :custom
  (typescript-indent-level 2)
  :init
  (advice-add 'lsp-diagnostics-flycheck-enable :after #'rk-ts--add-eslint)
  (add-hook 'typescript-mode-hook #'rk-ts--setup-typescript))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
