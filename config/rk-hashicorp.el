;;; rk-hashicorp.el --- Config for Hashicorp files  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package hcl-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(use-package terraform-mode
  :straight t
  :after eglot
  :hook (terraform-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))

(provide 'rk-hashicorp)

;;; rk-hashicorp.el ends here
