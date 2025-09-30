;;; mod-eshell.el --- Eshell configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains eshell configuration including:
;; - eshell (Emacs built-in shell)
;; - Custom eshell utilities from +eshell.el

;;; Code:

;;; Eshell - Emacs shell

(use-package eshell
  ;; Emacs' built-in shell combining Emacs Lisp evaluation with Unix shell
  ;; features.
  :config
  (require '+eshell))

(provide 'mod-eshell)
;;; mod-eshell.el ends here
