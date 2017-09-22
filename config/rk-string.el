;;; rk-string.el --- String utlities  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package rk-string-fns
  :commands
  (rk-word-or-region-to-camel-lower
   rk-word-or-region-to-camel-upper
   rk-word-or-region-to-snake
   rk-word-or-region-to-dashed)

  :init
  (spacemacs-keys-set-leader-keys
    "t l" #'rk-word-or-region-to-camel-lower
    "t u" #'rk-word-or-region-to-camel-upper
    "t s" #'rk-word-or-region-to-snake
    "t d" #'rk-word-or-region-to-dashed))

(provide 'rk-string)

;;; rk-string.el ends here
