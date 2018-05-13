;;; rk-parentheses.el --- Configuration for parens.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package paren-face
  :straight t
  :demand t
  :commands (global-paren-face-mode)
  :config
  (progn
    (add-to-list 'paren-face-modes 'web-mode)
    (setq paren-face-regexp (rx (any "{}();,")))
    (global-paren-face-mode +1)))

(provide 'rk-parentheses)

;;; rk-parentheses.el ends here
