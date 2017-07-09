;;; rk-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'org)

(autoload 'org-agenda-filter-apply "org-agenda")

;;;###autoload
(defun rk-org-goto-diary ()
  "Switch to the diary file."
  (interactive)
  (find-file org-agenda-diary-file))

;;;###autoload
(defun rk-org-goto-notes ()
  "Switch to the default notes file."
  (interactive)
  (find-file org-default-notes-file))

;;;###autoload
(defun rk-org-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file rk-org-work-file))

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

(defconst rk-org-goto--show-agenda-work-start-hour 8)
(defconst rk-org-goto--show-agenda-work-end-hour 17)

(defun rk-org-goto--is-work-time? (time)
  (-let* (((_s _m h d m y) time)
          (day-of-week (calendar-day-of-week (list m d y))))
    (and (<= rk-org-goto--show-agenda-work-start-hour h)
         (>= rk-org-goto--show-agenda-work-end-hour h)
         (<= 1 day-of-week)
         (>= 5 day-of-week))))

;;;###autoload
(defun rk-org-goto-agenda ()
  "Show the agenda fullscreen."
  (interactive)
  (let ((agenda-key (if (rk-org-goto--is-work-time? (decode-time)) "w" "A")))
    (org-agenda current-prefix-arg agenda-key))
  (delete-other-windows))

(provide 'rk-org-goto)

;;; rk-org-goto.el ends here
