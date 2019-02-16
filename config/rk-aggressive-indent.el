;;; rk-aggressive-indent.el --- Configuration for aggressive-indent-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst rk-aggressive-indent-exclude-modes
  '(toml-mode
    go-mode
    tuareg-mode
    haskell-mode
    dockerfile-mode
    diff-auto-refine-mode
    sql-mode
    restclient-mode
    rk-web-js-mode
    rk-web-php-mode
    rnc-mode
    scala-mode)
  "List of modes to be excluded from aggressive indent.")

(use-package aggressive-indent
  :straight t
  :commands (global-aggressive-indent-mode)
  :defer 3
  :preface
  (defun rk-extend-aggressive-indent-exclude-modes ()
    "Add select modes to be excluded by aggressive indent."
    (dolist (item rk-aggressive-indent-exclude-modes)
      (add-to-list 'aggressive-indent-excluded-modes item)))
  :config
  (progn
    (rk-extend-aggressive-indent-exclude-modes)
    (global-aggressive-indent-mode +1)))

(provide 'rk-aggressive-indent)

;;; rk-aggressive-indent.el ends here
