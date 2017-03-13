;;; rk-header-line-mode.el --- Minor mode for toggling the header line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defconst rk-header-line-function #'ignore
  "0-argument function returning the header line format string.")

;;;###autoload
(define-minor-mode rk-header-line-mode
  "Minor mode to show or hide the header line."
  nil nil nil
  (if rk-header-line-mode
      (setq header-line-format (funcall rk-header-line-function))
    (setq header-line-format nil)))

;;;###autoload
(defun rk-header-line-mode-on ()
  "Explicitly enable `rk-header-line-mode'."
  (interactive)
  (rk-header-line-mode +1))

;;;###autoload
(define-globalized-minor-mode rk-header-line-global-mode rk-header-line-mode
  rk-header-line-mode-on)

(provide 'rk-header-line-mode)

;;; rk-header-line-mode.el ends here
