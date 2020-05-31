;;; rk-themes.el --- Themes setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)

;; (use-package rk-ligatures
;;   :if (window-system)
;;   :preface
;;   (autoload 'rk-ligatures--set-kwds "rk-ligatures")
;;   :config
;;   (rk-ligatures--set-kwds
;;    '((rk-ligatures--fira-font prog-mode-hook))))

;; (use-package rk-iosevka
;;   :if (window-system))

(defun rk/themes/light-theme ()
  "Load light theme."
  (interactive)
  (load-theme 'rk-light t))

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

(defun config-themes--after-enable-theme (&rest _)
  ;; Delete posframes after changing themes.
  (when (fboundp 'posframe-delete-all)
    (posframe-delete-all))
  ;; Force org buffers to refontify to fix org-bullet properties.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush (point-min) (point-max))))))

(advice-add 'enable-theme :after #'config-themes--after-enable-theme)

(provide 'rk-themes)

;;; rk-themes.el ends here
