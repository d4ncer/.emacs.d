;;; rk-php.el --- Configuration for PHP mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package php-mode
  :mode "\\.php\\'")

(provide 'rk-php)

;;; rk-php.el ends here
