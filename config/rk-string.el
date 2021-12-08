;;; rk-string.el --- String utlities  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package rk-string-fns
  :init
  (rk-leader-def
    "t l" '(rk-word-or-region-to-camel-lower :wk "to lower camel")
    "t u" '(rk-word-or-region-to-camel-upper :wk "to upper camel")
    "t s" '(rk-word-or-region-to-snake :wk "to snake")
    "t d" '(rk-word-or-region-to-dashed :wk "to dashed")))

(provide 'rk-string)

;;; rk-string.el ends here
