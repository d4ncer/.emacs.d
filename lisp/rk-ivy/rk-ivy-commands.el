;;; rk-ivy-commands.el --- Misc commands for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'counsel)
(require 'deadgrep)
(require 'swiper)
(require 'subr-x)
(require 'dash)
(require 'projectile)
(require 'counsel-projectile)
(require 's)
(require 'f)

(autoload 'projectile-project-root "projectile")

(defconst rk-counsel--escape-characters '("$" "*" "+")
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
  "Get text or symbol at point, or an empty string if neither exist."
  (if-let* ((text (rk--region-or-symbol-at-pt)))
      text
    ""))

(defun rk-swiper-region-or-symbol (input)
  "Run `swiper' with INPUT, which is either the selected region or the symbol at point."
  (interactive (list (rk--region-or-symbol)))
  (swiper (rk-counsel--escape-string input)))

(defun rk-counsel-project-region-or-symbol (input)
  "Search project for INPUT, which is either the selected region or the symbol at point."
  (interactive (list (rk--region-or-symbol)))
  (counsel-rg (rk-counsel--escape-string input) (projectile-project-root)))

(defun rk-counsel-i18n-project-region-or-symbol (input)
  "Search project for INPUT, which is either the selected region or the symbol at point."
  (interactive (list (rk--region-or-symbol)))
  (counsel-rg (s-join " " (list "i18n.t" (rk-counsel--escape-string input))) (projectile-project-root)))

(defun rk-counsel-region-or-symbol (start-dir input)
  "Search START-DIR for INPUT which is either the selected region or symbol at point."
  (interactive (list (read-directory-name "Start from directory: ")
                     (rk--region-or-symbol)))
  (counsel-rg (rk-counsel--escape-string input) start-dir))

(defun rk-counsel-deadgrep-from-ivy ()
  "Convert an Ivy QUERY to a Deadgrep search."
  (interactive)
  (ivy-exit-with-action
   (lambda (&rest _)
     (let ((deadgrep--search-type 'regexp))
       (deadgrep (replace-regexp-in-string (rx (+ space)) ".*?" ivy-text))))))

(defun rk-counsel--switch-project ()
  "Switch to projectile project that is a git root."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            (--filter (f-dir-p (f-join it ".git")) projectile-known-projects)
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action counsel-projectile-switch-project-action
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'rk-counsel--switch-project))

(provide 'rk-ivy-commands)

;;; rk-ivy-commands.el ends here
