;;; rk-ivy-commands.el --- Misc commands for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'counsel)
(require 'swiper)
(require 'subr-x)

(autoload 'projectile-project-root "projectile")

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
  (if-let ((sym (symbol-at-point)))
      (counsel-ag (symbol-name sym) (projectile-project-root))
    (counsel-ag nil (projectile-project-root))))

(defun rk-counsel-region-or-symbol ()
  "Search initial directory for region or symbol at point."
  (interactive)
  (let ((init-dir (read-directory-name "Start from directory: ")))
    (if-let ((sym (symbol-at-point)))
        (counsel-ag (symbol-name sym) init-dir)
      (counsel-ag nil init-dir))))

(provide 'rk-ivy-commands)

;;; rk-ivy-commands.el ends here
