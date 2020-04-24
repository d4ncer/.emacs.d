;;; rk-fsharp.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package fsharp-mode
  :straight t
  :hook (fsharp-mode . lsp))

(provide 'rk-fsharp)

;;; rk-fsharp.el ends here
