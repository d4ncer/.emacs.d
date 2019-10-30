;;; rk-scale-font-transient-state.el --- Microstate for zooming text scale.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)

(defun rk-general--scale-font-up-or-down (direction)
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

(defun rk-general--scale-font-up ()
  "Scale up the font."
  (interactive)
  (rk-general--scale-font-up-or-down 1))

(defun rk-general--scale-font-down ()
  "Scale up the font."
  (interactive)
  (rk-general--scale-font-up-or-down -1))

(defun rk-general--reset-font-size ()
  "Reset the font size."
  (interactive)
  (rk-general--scale-font-up-or-down 0))

(pretty-hydra-define rk-general--scale-font
  (:title "Fonts" :foreign-keys run)
  ("Scale"
   (("+" rk-general--scale-font-up "Up")
    ("-" rk-general--scale-font-down "Down")
    ("0" rk-general--reset-font-size "Reset")
    ("q" nil "quit" :exit t))))

(provide 'rk-scale-font-transient-state)

;;; rk-scale-font-transient-state.el ends here
