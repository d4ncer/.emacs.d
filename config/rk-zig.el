;;; rk-zig.el --- Zig config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package zig-mode
  :straight t
  :after lsp-mode
  :init
  (add-hook 'zig-mode-hook #'lsp-deferred))

(provide 'rk-zig)

;;; rk-zig.el ends here
