;;; rk-csharp.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

;;; TODO Come back to this once I can build razzmatazz's branch
(use-package csharp-mode
  :straight t
  ;; :custom
  ;; (lsp-csharp-server-path (expand-file-name "~/code/omnisharp-roslyn/artifacts/scripts/OmniSharp.Stdio"))
  ;; :hook (csharp-mode . lsp))
  :preface
  (defun rk-csharp--setup ()
    (company-mode)
    (eval-after-load
        'company
      '(add-to-list 'company-backends 'company-omnisharp))
    (flycheck-mode)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)

    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile))
  :hook
  (csharp-mode . rk-csharp--setup))

(use-package omnisharp
  :straight t
  :after csharp-mode
  :hook
  (csharp-mode . omnisharp-mode)
  :general
  (:keymaps 'omnisharp-mode-map :states '(normal motion visual)
            "gd" #'omnisharp-go-to-definition))

(use-package csproj-mode
  :straight t)

(provide 'rk-csharp)

;;; rk-csharp.el ends here
