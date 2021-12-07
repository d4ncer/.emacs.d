;;; rk-parentheses.el --- Configuration for parens.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package paren-face
  :straight t
  :custom
  (paren-face-regexp (rx (any "{}();,")))
  :config
  (add-to-list 'paren-face-modes 'web-mode)
  (global-paren-face-mode +1))

(provide 'rk-parentheses)

;;; rk-parentheses.el ends here
