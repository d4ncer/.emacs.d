;;; rk-racket.el --- Config for Racket  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package racket-mode
  :straight t)

(provide 'rk-racket)

;;; rk-racket.el ends here
