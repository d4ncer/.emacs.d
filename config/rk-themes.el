;;; rk-themes.el --- Themes setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'spacemacs-keys)

(defun danc--themes/light-theme ()
  "Load light theme."
  (interactive)
  (load-theme 'rk-light t))

(use-package rk-ligatures
  :if (window-system)
  :preface
  (autoload 'rk-ligatures--set-kwds "rk-ligatures")
  :config
  (rk-ligatures--set-kwds
   '((rk-ligatures--fira-font prog-mode-hook))))

(use-package doom-themes
  :straight t
  :config
  (progn
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-package tao-theme
  :straight t)

(provide 'rk-themes)

;;; rk-themes.el ends here
