;;; rk-darwin.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)

(general-def "s-q" #'save-buffers-kill-emacs)
(general-def "s-v" #'yank)
(general-def "s-c" #'copy-region-as-kill)
(general-def "s-n" #'new-frame)
(general-def "s-w" #'delete-frame)

(use-package osx-trash
  :straight t
  :config
  (osx-trash-setup)
  :functions
  (osx-trash-setup))

(provide 'rk-darwin)

;;; rk-darwin.el ends here
