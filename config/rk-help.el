;;; rk-help.el --- Configuration for Help+.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package help-fns+
  :straight t
  :commands (describe-buffer
             describe-file
             describe-keymap)
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "h d K" #'describe-keymap
      "h d F" #'describe-file
      "h d B" #'describe-buffer)))

(provide 'rk-help)

;;; rk-help.el ends here
