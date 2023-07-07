;;; rk-mermaid.el --- Mermaidy things  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package mermaid-mode
  :straight t
  :preface
  (defun rk-mermaid/setup ()
    (add-hook 'after-save-hook #'mermaid-compile nil 'local))
  :hook
  (mermaid-mode . rk-mermaid/setup))

(use-package smartparens
  :straight t
  :after mermaid-mode
  :config
  (sp-with-modes '(mermaid-mode)
    (sp-local-pair "{" nil :actions nil)))

(provide 'rk-mermaid)

;;; rk-mermaid.el ends here
