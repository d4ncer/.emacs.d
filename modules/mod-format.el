;;; mod-format.el --- Code formatting (apheleia) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains code formatting configuration including:
;; - apheleia for automatic formatting on save

;;; Code:

;;; Apheleia - Code formatting

(use-package apheleia :ensure t
  ;; Apply code formatting on save. Works for a range of languages.
  :after-call +first-file-hook
  :config
  (apheleia-global-mode +1))

;; By default, trim trailing whitespace aggressively.

;; (defvar-local +trim-trailing-whitespace-aggressively t)
;;
;; (add-hook! 'before-save-hook
;;   (when +trim-trailing-whitespace-aggressively
;;     (delete-trailing-whitespace)))

(provide 'mod-format)
;;; mod-format.el ends here
