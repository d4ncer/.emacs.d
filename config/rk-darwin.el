;;; rk-darwin.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package osx-trash
  :straight t
  :config
  (osx-trash-setup))

(use-package emacs
  :custom
  (frame-title-format nil)
  (ns-use-native-fullscreen nil)
  :general
  ("s-q" #'save-buffers-kill-emacs
   "s-v" #'yank
   "s-c" #'copy-region-as-kill
   "s-n" #'make-frame-command
   "s-w" #'delete-frame)
  :config
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))


(provide 'rk-darwin)

;;; rk-darwin.el ends here
