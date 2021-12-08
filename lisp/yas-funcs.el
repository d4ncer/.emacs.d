;;; yas-funcs.el --- Functions for yasnippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 's)
(require 'f)
(require 'thingatpt)

(defvar yas-text nil)

(defun yas-funcs-bolp ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match-p (rx bol (* space) (* word) (* space) eol)
                    line)))

;;; Elisp

(defun yas-funcs-el-custom-group ()
  "Find the first group defined in the current file.
Fall back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun yas-funcs-el-autoload-file (sym)
  (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
      (file-name-sans-extension (file-name-nondirectory file))
    ""))

(defun yas-funcs-el-at-line-above-decl-p ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))

(defun yas-funcs-el-package-prefix ()
  (cond
   ((string-prefix-p "*Org Src" (buffer-name))
    "")
   ((bound-and-true-p nameless-current-name)
    (format "%s-" nameless-current-name))
   (t
    (format "%s-" (f-base (or (buffer-file-name) (buffer-name)))))))

(defun yas-funcs-buttercup-file-p ()
  (string-match-p "^test-" (file-name-nondirectory (buffer-file-name))))

;;; JS

(cl-defun yas-funcs-js-module-name-for-binding (&optional (text yas-text))
  (pcase text
    ('nil      "")
    (""        "")
    ("Promise" "bluebird")
    ("assert"  "power-assert")
    ("VError"  "verror")
    ("styles"  "./styles.css")
    ("styled"  "styled-components")
    ("stampit"   "@stamp/it")
    ("Configure" "@stamp/configure")
    ("Required"  "@stamp/required")
    ("_"       "lodash")

    ((guard (s-contains? "{" text))
     "")
    (s
     (s-downcase (s-dashed-words s)))))

(defun yas-funcs-js-ctor-body (argstring)
  (when argstring
    (thread-last argstring
                 (s-split (rx (or "," ".")))
                 (-map #'s-trim)
                 (-remove #'s-blank?)
                 (--map (format "this.%s = %s;" it it))
                 (s-join "\n"))))

(provide 'yas-funcs)

;;; yas-funcs.el ends here
