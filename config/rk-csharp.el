;;; rk-csharp.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package csharp-mode
  :straight t
  :custom
  (lsp-csharp-server-path (expand-file-name "~/code/omnisharp-roslyn/artifacts/scripts/OmniSharp.Stdio"))
  :hook (csharp-mode . lsp))

(provide 'rk-csharp)

;;; rk-csharp.el ends here
