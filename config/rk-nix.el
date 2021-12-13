;;; rk-nix.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package nix-mode
  :straight t
  :after lsp
  :hook
  (nix-mode . lsp))

(provide 'rk-nix)

;;; rk-nix.el ends here
