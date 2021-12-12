;;; rk-auto-insert.el --- Auto-insert  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package autoinsert
  :preface
  (defvar auto-insert-alist nil)
  :hook
  (find-file . auto-insert)
  :custom
  (auto-insert-query nil))

(use-package autoinsert-funcs
  :after autoinsert
  :defines (autoinsert-funcs-forms)
  :config
  (dolist (form autoinsert-funcs-forms)
    (push form auto-insert-alist)))

(provide 'rk-auto-insert)

;;; rk-auto-insert.el ends here
