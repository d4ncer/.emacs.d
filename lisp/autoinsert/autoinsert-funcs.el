;;; autoinsert-funcs.el --- Auto-insert features for shell scripts.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'f)

(autoload 'yas-expand-snippet "yasnippet")

;;; Elisp

(defconst autoinsert-funcs-el-form
  '((emacs-lisp-mode . "Emacs Lisp")
    nil
    "\;;; " (file-name-nondirectory (buffer-file-name)) " --- <enter description here>  "
    "-*- lexical-binding: t; -*-" '(setq lexical-binding t) \n
    \n
    ";;; Commentary:"                                       \n \n
    ";;; Code:"                                             \n \n
    _                                                       \n \n
    "\(provide '" (file-name-base) ")"                      \n \n
    "\;;; " (file-name-nondirectory (buffer-file-name)) " ends here" \n))

;;; Shell-scripts

(defun autoinsert-funcs--sh-template-string ()
  "Autoinsert string used for sh buffers"
  (let ((program
         (if (f-ext? (buffer-name) "zsh")
             "zsh"
           "bash")))
    (yas-expand-snippet (format "#!/usr/bin/env %s
$0
" program))))

(defconst autoinsert-funcs-sh-form
  '((sh-mode . "Shell Script") . autoinsert-funcs--sh-template-string))

;;; Python

(defconst autoinsert-funcs-py-form
  '((python-mode . "Python")
    nil
    "# -*- coding: utf-8 -*-\n\n"))

;;; HTML

(defun autoinsert-funcs--html-template-string ()
  (yas-expand-snippet (string-trim
                       "
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <title>$0</title>
  </head>
  <body>
  </body>
</html>
")))

(defconst autoinsert-funcs-html-form
  '((web-html-mode . "HTML") . autoinsert-funcs-html-template-string))

(defconst autoinsert-funcs-forms
  (list autoinsert-funcs-sh-form
        autoinsert-funcs-el-form
        autoinsert-funcs-py-form
        autoinsert-funcs-html-form))

(provide 'autoinsert-funcs)

;;; autoinsert-funcs.el ends here
