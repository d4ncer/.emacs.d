;;; rk-search.el --- Configuration for search related utils.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'rk-misc-utils)

(use-package ag
  :straight t
  :commands (ag ag-dired))

(use-package rg
  :straight t
  :commands rg)

(use-package wgrep
  :straight t
  :commands (wgrep-setup)
  :init
  (add-hook 'grep-setup-hook #'wgrep-stup)
  :general
  (:keymaps 'wgrep-mode-map
   :states '(normal motion visual emacs)
   ", c" #'rk-search-wgrep-finish-edit-kill-buffer
   ", k" #'rk-search-wgrep-abort-changes-kill-buffer)
  :config
  (progn
    (setq wgrep-auto-save-buffer t)))

(provide 'rk-search)

;;; rk-search.el ends here
