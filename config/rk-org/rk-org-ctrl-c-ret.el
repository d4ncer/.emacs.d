;;; rk-org-ctrl-c-ret.el --- Context-sensitive Ctrl-C RET keybinding.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'org-at-table-p "org")
(autoload 'org-insert-todo-heading "org")
(autoload 'org-table-hline-and-move "org-table")

(defun rk-org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading'."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-hline-and-move))
   (t
    (call-interactively #'org-insert-todo-heading))))

(provide 'rk-org-ctrl-c-ret)

;;; rk-org-ctrl-c-ret.el ends here
