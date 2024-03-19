;;; rk-lsp.el --- Configuration for LSP using eglot.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'definers)

(use-package dumb-jump
  :straight t
  ;; KLUDGE: Need specific bindings because xref-backends can't be easily
  ;; unioned to support both LSP + dumb-jump
  :general
  (:states '(normal motion)
           "gw" #'dumb-jump-go
           "gW" #'dumb-jump-go-other-window)
  :custom
  (dumb-jump-selector 'completing-read)
  (dumb-jump-force-searcher 'rg))

(use-package eglot
  :preface
  ;; (defun rk-eglot/setup-hooks ()
  ;;   (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  (defun rk-eglot/init ()
    ;; Put stuff in for eglot set up here
    ())
  :hook
  (eglot-managed-mode . rk-eglot/init)
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

(provide 'rk-lsp)

;;; rk-lsp.el ends here
