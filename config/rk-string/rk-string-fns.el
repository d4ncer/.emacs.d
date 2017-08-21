;;; rk-string-fns.el --- String utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 's)

(defun rk--bounds-of-region-or-symbol-at-pt ()
  "Get bounds of region or symbol at point."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun rk--words-in-region (bounds)
  "Get all words in buffer in BOUNDS."
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(defun rk-word-or-region-to-camel-lower ()
  "Convert selected region or symbol to lower camel case."
  (interactive)
  (let* ((bounds (rk--bounds-of-region-or-symbol-at-pt))
         (text (rk--words-in-region bounds)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-lower-camel-case text)))))

(defun rk-word-or-region-to-camel-upper ()
  "Convert selected region or symbol to upper camel case."
  (interactive)
  (let* ((bounds (rk--bounds-of-region-or-symbol-at-pt))
         (text (rk--words-in-region bounds)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-upper-camel-case text)))))

(defun rk-word-or-region-to-snake ()
  "Convert selected region or symbol to snake case."
  (interactive)
  (let* ((bounds (rk--bounds-of-region-or-symbol-at-pt))
         (text (rk--words-in-region bounds)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-snake-case text)))))

(defun rk-word-or-region-to-dashed ()
  "Convert selected region or symbol to dashed case."
  (interactive)
  (let* ((bounds (rk--bounds-of-region-or-symbol-at-pt))
         (text (rk--words-in-region bounds)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-dashed-words text)))))

(provide 'rk-string-fns)

;;; rk-string-fns.el ends here
