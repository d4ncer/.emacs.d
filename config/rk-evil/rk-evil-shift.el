;;; rk-evil-shift.el --- Evil shift commands which keep region active.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'evil)

(defun rk-evil-shift-left (&optional beg end)
  "Shift left, keeping the region active.
BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-left beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun rk-evil-shift-right (&optional beg end)
  "Shift right, keeping the region active.
BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-right beg end)
  (evil-normal-state)
  (evil-visual-restore))

(provide 'rk-evil-shift)

;;; rk-evil-shift.el ends here
