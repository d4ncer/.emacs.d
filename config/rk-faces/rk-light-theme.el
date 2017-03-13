;;; rk-light-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(deftheme rk-light)

(require 'rk-theme-common)

(apply #'custom-theme-set-faces 'rk-light (rk-theme-common-make-theme "black" "white"))

(provide-theme 'rk-light)

;;; rk-light-theme.el ends here
