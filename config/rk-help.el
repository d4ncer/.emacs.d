;;; rk-help.el --- Configuration for Help+.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package help-fns+
  :straight t
  :commands (describe-buffer
             describe-file
             describe-keymap)
  :init
  (progn
    (rk-leader-def
      "h d K" '(describe-keymap :wk "describe keymap")
      "h d F" '(describe-file :wk "describe file")
      "h d B" '(describe-buffer :wk "describe buffer"))))

(provide 'rk-help)

;;; rk-help.el ends here
