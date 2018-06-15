;;; rk-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package doom-modeline
  :config
  (+doom-modeline|init))

(provide 'rk-modeline)

;;; rk-modeline.el ends here
