;;; rk-string-fns.el --- String utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)

(defun rk--bounds-of-region-or-symbol-at-pt ()
  "Get bounds of region or symbol at point."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun rk--words-in-region (bounds)
  "Get all words in buffer in BOUNDS."
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(defun rk--bounds-and-symbol ()
  (-if-let* (((beg . end) (rk--bounds-of-region-or-symbol-at-pt))
             (text (buffer-substring-no-properties beg end)))
      (list beg end text)
    (user-error "No symbol at point")))

(defun rk-word-or-region-to-camel-lower (beg end text)
  "Convert TEXT in selected region or symbol in bounds
BEG to END to lower camel case."
  (interactive (rk--bounds-and-symbol))
  (save-excursion
    (delete-region beg end)
    (insert (s-lower-camel-case text))))

(defun rk-word-or-region-to-camel-upper (beg end text)
  "Convert TEXT in selected region or symbol in bounds
BEG to END to upper camel case."
  (interactive (rk--bounds-and-symbol))
  (save-excursion
    (delete-region beg end)
    (insert (s-upper-camel-case text))))

(defun rk-word-or-region-to-snake (beg end text)
  "Convert TEXT in selected region or symbol in bounds
BEG to END to snake case."
  (interactive (rk--bounds-and-symbol))
  (save-excursion
    (delete-region beg end)
    (insert (s-snake-case text))))

(defun rk-word-or-region-to-dashed (beg end text)
  "Convert TEXT in selected region or symbol in bounds
BEG to END to dashed case."
  (interactive (rk--bounds-and-symbol))
  (save-excursion
    (delete-region beg end)
    (insert (s-dashed-words text))))

(provide 'rk-string-fns)

;;; rk-string-fns.el ends here
