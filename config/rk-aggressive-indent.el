;;; rk-aggressive-indent.el --- Configuration for aggressive-indent-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package aggressive-indent
  :straight t
  :commands (aggressive-indent-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'rk-aggressive-indent)

;;; rk-aggressive-indent.el ends here
