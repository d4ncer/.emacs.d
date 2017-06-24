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

(defconst rk-counsel--escape-characters '("$")
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
    (string)))

(defun rk-swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

(defun rk-counsel-project-region-or-symbol ()
  "Search project for region or symbol at point."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (counsel-rg (rk-counsel--escape-string input) (projectile-project-root))))

(defun rk-counsel-region-or-symbol ()
  "Search initial directory for region or symbol at point."
  (interactive)
  (let ((init-dir (read-directory-name "Start from directory: ")))
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (counsel-rg (rk-counsel--escape-string input) init-dir))))

(provide 'rk-ivy-commands)

;;; rk-ivy-commands.el ends here
