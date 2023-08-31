;;; rk-lsp.el --- Configuration for LSP using eglot.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'definers)

(use-package eglot
  :preface
  (defun rk-eglot/setup-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil 'local))
  :hook
  (eglot-managed-mode . rk-eglot/setup-hooks)
  :config
  (rk-local-leader-def :keymaps 'eglot-mode-map
    "." '(eglot-format-buffer :wk "format")
    "l" '(:ignore t :wk "lsp")
    "l o" '(eglot-code-action-organize-imports :wk "organize imports")
    "l c" '(eglot-code-actions :wk "code actions")
    "l r" '(eglot-rename :wk "rename")))

(use-package eldoc-box
  :straight t
  :general (:states '(normal motion)
                    "K" #'eldoc-box-help-at-point))

(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-selector 'completing-read)
  (dumb-jump-force-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
