;;; rk-csharp.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package csharp-mode
  :straight t
  :custom
  (lsp-csharp-server-path "/usr/local/bin/omnisharp")
  ;; (lsp-csharp-server-path "~/bin/omnisharp")
  :hook (csharp-mode . lsp))

(use-package csproj-mode
  :straight t)

(use-package powershell
  :straight t
  :hook (powershell-mode . lsp))

(provide 'rk-csharp)

;;; rk-csharp.el ends here
