;;; +file-templates.el --- File templates -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar auto-insert-directory nil)
(autoload 'define-auto-insert "autoinsert")

(defmacro +define-file-template (mode-or-regexp template-file-name)
  "Use a file template for a particular major-mode or file name pattern.

MODE-OR-REGEXP is a regular expression or a mode name as a symbol.

TEMPLATE-FILE-NAME is the path to a skeleton template, relative to
`auto-insert-directory'."
  (let ((template-file (file-name-concat auto-insert-directory template-file-name)))
    `(define-auto-insert ,mode-or-regexp (lambda ()
                                           (skeleton-insert
                                            (with-temp-buffer
                                              (insert-file-contents ,template-file)
                                              (read (buffer-string))))))))

(defmacro +define-file-template-dispatcher (mode-or-regexp clause &rest clauses)
  "Like `+define-file-template' but dynamically compute the template to use.

MODE-OR-REGEXP is a regular expression or a mode name as a symbol.

CLAUSE & CLAUSES are two-item lists as in `cond', where the result of
each clause is taken as the name of a template."
  (declare (indent 1))
  `(define-auto-insert ,mode-or-regexp (lambda ()
                                         (let* ((choice (cond ,clause ,@clauses)))
                                           (if choice
                                               (let ((template-file (file-name-concat auto-insert-directory choice)))
                                                 (skeleton-insert
                                                  (with-temp-buffer
                                                    (insert-file-contents template-file)
                                                    (read (buffer-string)))))
                                             "")))))

(provide '+file-templates)

;;; +file-templates.el ends here
