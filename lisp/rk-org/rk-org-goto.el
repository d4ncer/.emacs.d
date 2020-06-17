;;; rk-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'paths)
(require 'calendar)

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
