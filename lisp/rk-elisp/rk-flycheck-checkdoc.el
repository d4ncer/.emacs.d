;;; rk-flycheck-checkdoc.el --- Configure checkdoc checker.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'flycheck)

(defconst rk-flycheck-checkdoc-form
  (flycheck-prepare-emacs-lisp-form
    (setq checkdoc-force-docstrings-flag nil)
    (setq checkdoc-arguments-in-order-flag nil)
    (let ((source (car command-line-args-left))
          ;; Remember the default directory of the process
          (process-default-directory default-directory))
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        ;; And change back to the process default directory to make file-name
        ;; back-substutition work
        (setq default-directory process-default-directory)
        (with-demoted-errors "Error in checkdoc: %S"
          (checkdoc-current-buffer t)
          (with-current-buffer checkdoc-diagnostic-buffer
            (princ (buffer-substring-no-properties (point-min) (point-max)))
            (kill-buffer)))))))

(provide 'rk-flycheck-checkdoc)

;;; rk-flycheck-checkdoc.el ends here
