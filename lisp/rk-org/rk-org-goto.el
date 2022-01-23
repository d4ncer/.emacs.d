;;; rk-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'paths)

(autoload 'org-agenda-filter-apply "org-agenda")

;;;###autoload
(defun rk-org-goto-todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))

;;;###autoload
(defun rk-org-goto-tags-list ()
  "Show all tagged items."
  (interactive)
  (org-tags-view nil))

(provide 'rk-org-goto)

;;; rk-org-goto.el ends here
