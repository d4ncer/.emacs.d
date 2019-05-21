;;; rk-hashicorp.el --- Config for Hashicorp files  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package hcl-mode
  :straight t
  :mode (("\\.hcl\\'" . hcl-mode)
         ("\\.nomad\\'" . hcl-mode)))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :config
  (add-hook #'terraform-mode-hook #'terraform-format-on-save-mode))

(provide 'rk-hashicorp)

;;; rk-hashicorp.el ends here
