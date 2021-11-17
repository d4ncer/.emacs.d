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
(require 'rk-theme-base)

(use-package rk-ligature
  :if (window-system))

(defun rk/themes/light-theme ()
  "Load light theme."
  (interactive)
  (load-theme 'rk-light t))

(defun config-themes--after-enable-theme (&rest _)
  "Delete posframes after changing themes."
  (when (fboundp 'posframe-delete-all)
    (posframe-delete-all))
  ;; Force org buffers to refontify to fix org-bullet properties.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush (point-min) (point-max))))))

(advice-add 'enable-theme :after #'config-themes--after-enable-theme)

(use-package rk-theme-nano)

(use-package tree-sitter
  :disabled t
  :straight t)

(use-package tree-sitter-langs
  :disabled t
  :after tree-sitter)

(provide 'rk-themes)

;;; rk-themes.el ends here
