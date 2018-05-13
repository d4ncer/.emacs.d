;;; rk-org-clock-cascade.el --- Promote TODOs to NEXTs when clocking in.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-clock)
(require 'dash)

(defun rk-org-clock-cascade--at-todo-parent? ()
  "Non-nil if at a task with any todo subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun rk-org-clock-cascade--at-task? ()
  "Non-nil if at a todo with no subtasks."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun rk-org-clock-cascade-clock-in-to-next-state (&optional _kw)
  "Move the task at point from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO."
  (unless (and (boundp 'org-capture-mode) org-capture-mode)
    (cond
     ((and (-contains? '("TODO") (org-get-todo-state))
           (rk-org-clock-cascade--at-task?))
      "NEXT")
     ((and (-contains? '("NEXT") (org-get-todo-state))
           (rk-org-clock-cascade--at-todo-parent?))
      "TODO"))))

;;;###autoload
(defun rk-org-clock-cascade-init ()
  (setq org-clock-in-switch-to-state #'rk-org-clock-cascade-clock-in-to-next-state))

(provide 'rk-org-clock-cascade)

;;; rk-org-clock-cascade.el ends here
