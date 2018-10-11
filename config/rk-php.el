;;; rk-php.el --- Config for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package php-mode
  :straight t
  :mode ("\\.php\\'" . php-mode)
  :config
  (setq php-template-compatibility nil))

(provide 'rk-php)

;;; rk-php.el ends here
