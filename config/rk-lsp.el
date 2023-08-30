;;; rk-lsp.el --- Configuration for LSP using eglot.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'definers)

;; TODO: Add bindings for common eglot commands
(use-package eglot)

(use-package eldoc-box
  :straight t)

(use-package eglot
  :after eldoc-box
  :general (:keymaps 'eglot-mode-map :states '(normal motion)
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
