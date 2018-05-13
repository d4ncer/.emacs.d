;;; rk-yas-utils.el --- Utils for yasnippet  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(require 'subr-x)
(require 's)

(defun rk-yas/bol? ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))

(provide 'rk-yas-utils)

;;; rk-yas-utils.el ends here
