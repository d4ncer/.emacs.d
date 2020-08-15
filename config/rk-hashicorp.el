;;; rk-hashicorp.el --- Config for Hashicorp files  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package hcl-mode
  :straight t
  :mode (("\\.hcl\\'" . hcl-mode)
         ("\\.nomad\\'" . hcl-mode)))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :preface
  (defun rk-hashicorp--terra-ls-server-cmd ()
    (let* ((base '("terraform-ls" "serve" "-tf-exec"))
           (tbin (executable-find "terraform")))
      (-snoc base tbin)))
  (defun rk-hashicorp--register-official-lsp-client ()
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection (lambda () (rk-hashicorp--terra-ls-server-cmd)))
                      :major-modes '(terraform-mode)
                      :priority 1
                      :server-id 'terrals)))
  (defun rk-hashicorp--setup-lsp ()
    (rk-hashicorp--register-official-lsp-client)
    (lsp-deferred))
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-hook 'terraform-mode-hook #'rk-hashicorp--setup-lsp))

(provide 'rk-hashicorp)

;;; rk-hashicorp.el ends here
