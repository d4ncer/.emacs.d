;;; rk-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'paths)

(autoload 'org-agenda-filter-apply "org-agenda")

;;;###autoload
(defun rk-org-goto-diary ()
  "Switch to the diary file."
  (interactive)
  (find-file rk-org--diary-file))

;;;###autoload
(defun rk-org-goto-inbox ()
  "Switch to the GTD inbox file."
  (interactive)
  (find-file rk-org--inbox-file))

;;;###autoload
(defun rk-org-goto-consume ()
  "Switch to the consume file."
  (interactive)
  (find-file rk-org--consume-file))

;;;###autoload
(defun rk-org-goto-tickler ()
  "Switch to the tickler file."
  (interactive)
  (find-file rk-org--tickler-file))

;;;###autoload
(defun rk-org-goto-reference ()
  "Switch to the GTD reference file."
  (interactive)
  (find-file rk-org--reference-file))

;;;###autoload
(defun rk-org-goto-projects ()
  "Switch to the GTD projects file."
  (interactive)
  (find-file rk-org--projects-file))

;;;###autoload
(defun rk-org-goto-next ()
  "Switch to the next / one-off file."
  (interactive)
  (find-file rk-org--next-file))

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

(defconst rk-org-goto--show-agenda-work-start-hour 7)
(defconst rk-org-goto--show-agenda-work-end-hour 18)

(defun rk-org-goto--is-work-time? (time)
  "Is TIME within work hours or not?"
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
