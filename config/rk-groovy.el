;;; rk-groovy.el --- Configuration for Groovy files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package groovy-mode
  :straight t
  :commands (groovy-mode)
  :mode ("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode)
  :interpreter ("groovy" . groovy-mode))

(provide 'rk-groovy)

;;; rk-groovy.el ends here
