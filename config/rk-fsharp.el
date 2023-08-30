;;; rk-fsharp.el --- F# config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package fsharp-mode
  :straight t)

(use-package fsharp-mode
  :straight t
  :after eglot
  :hook (fsharp-mode . eglot-ensure))

(provide 'rk-fsharp)

;;; rk-fsharp.el ends here
