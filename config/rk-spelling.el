;;; rk-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package jinx
  :straight (:host github :repo "minad/jinx" :branch "main" :files ("*.el" "*.c" "*.h"))
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("C-=" #'jinx-correct))

(provide 'rk-spelling)

;;; rk-spelling.el ends here
