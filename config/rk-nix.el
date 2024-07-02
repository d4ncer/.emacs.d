;;; rk-nix.el --- nix config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package nix-mode
  :straight t
  :after eglot
  :hook
  (nix-mode . eglot-ensure))

(provide 'rk-nix)

;;; rk-nix.el ends here
