;;; rk-scale-font-transient-state.el --- Microstate for zooming text scale.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'evil-transient-state)

(defun rk-scale-font-transient-state-scale-font-size-up-or-down (direction)
  "Scale the font.
If DIRECTION is positive or zero the font is scaled up, otherwise
it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun rk-scale-font-transient-state-scale-up-font ()
  "Scale up the font."
  (interactive)
  (rk-scale-font-transient-state-scale-font-size-up-or-down 1))

(defun rk-scale-font-transient-state-scale-down-font ()
  "Scale up the font."
  (interactive)
  (rk-scale-font-transient-state-scale-font-size-up-or-down -1))

(defun rk-scale-font-transient-state-reset-font-size ()
  "Reset the font size."
  (interactive)
  (rk-scale-font-transient-state-scale-font-size-up-or-down 0))

(evil-transient-state-define rk-scale-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" rk-scale-font-transient-state-scale-up-font)
  ("=" rk-scale-font-transient-state-scale-up-font)
  ("-" rk-scale-font-transient-state-scale-down-font)
  ("0" rk-scale-font-transient-state-reset-font-size)
  ("q" nil :exit t))


(provide 'rk-scale-font-transient-state)

;;; rk-scale-font-transient-state.el ends here
