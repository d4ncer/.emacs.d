;;; rk-dark-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(deftheme rk-dark)

(require 'rk-theme-base)

(apply #'custom-theme-set-faces 'rk-dark (rk-theme-base-make-theme rk-theme-base-offwhite rk-theme-base-offblack))

(provide-theme 'rk-dark)

;;; rk-dark-theme.el ends here
