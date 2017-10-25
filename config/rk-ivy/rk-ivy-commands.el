;;; rk-ivy-commands.el --- Misc commands for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'counsel)
(require 'swiper)
(require 'subr-x)
(require 'dash)
(require 's)

(autoload 'projectile-project-root "projectile")

(defconst rk-counsel--escape-characters '("$" "*")
  "Characters to escape for input into counsel.")

(defun rk-counsel--escape-character-p (char)
  "Determines if a CHAR should be escaped for input into counsel."
  (-contains-p rk-counsel--escape-characters char))

(defun rk-counsel--escape-character (char)
  "Escape a CHAR for input into counsel."
  (concat "\\" char))

(defun rk-counsel--escape-string (string)
  "Escape STRING for input into counsel."
  (-if-let* ((str-list (split-string string "" t))
             (escaped-str-list (-map-when #'rk-counsel--escape-character-p #'rk-counsel--escape-character str-list)))
      (s-join "" escaped-str-list)
    string))

(defun rk--region-or-symbol-at-pt ()
  "Get symbol at point or text in selected region."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun rk--region-or-symbol ()
  "Get text or symbol at point, or return a user error if neither exist."
  (if-let ((text (rk--region-or-symbol-at-pt)))
      text
    (user-error "No symbol at point")))

(defun rk-swiper-region-or-symbol (input)
  "Run `swiper' with INPUT, which is either the selected region or the symbol at point."
  (interactive (list (rk--region-or-symbol)))
  (swiper input))

(defun rk-counsel-project-region-or-symbol (input)
  "Search project for INPUT, which is either the selected region or the symbol at point."
  (interactive (list (rk--region-or-symbol)))
  (counsel-rg (rk-counsel--escape-string input) (projectile-project-root)))

(defun rk-counsel-region-or-symbol (start-dir input)
  "Search START-DIR for INPUT which is either the selected region or symbol at point."
  (interactive (list (read-directory-name "Start from directory: ")
                     (rk--region-or-symbol)))
  (counsel-rg (rk-counsel--escape-string input) start-dir))

(provide 'rk-ivy-commands)

;;; rk-ivy-commands.el ends here
