;;; rk-aggressive-indent.el --- Configuration for aggressive-indent-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :defer 1
  :config
  (progn
    (add-to-list 'aggressive-indent-excluded-modes 'diff-auto-refine-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'toml-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'rk-web-js-mode)

    (global-aggressive-indent-mode +1)))

(provide 'rk-aggressive-indent)

;;; rk-aggressive-indent.el ends here
