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

(use-package nano
  :straight (:host github :repo "rougier/nano-emacs")
  :custom
  (nano-font-size 18)
  (nano-font-family-monospaced "Fira Code")
  :config
  (setq pop-up-windows t)
  (setq font-lock-maximum-decoration '((web-mode . t) (rk-web-tsx-mode . t) (rk-web-js-mode . t) (rk-web-css-mode . t) (rk-web-html-mode . t) (t . nil)))

  (set-face-attribute 'default nil
                      :weight 'normal)

  (set-face-attribute 'font-lock-keyword-face nil
                      :weight 'light
                      :inherit 'default)

  (set-face-attribute 'font-lock-builtin-face nil
                      :weight 'light
                      :inherit 'default)

  (set-face-attribute 'font-lock-variable-name-face nil
                      :inherit 'default)

  (set-face-attribute 'font-lock-function-name-face nil
                      :weight 'demibold
                      :inherit 'default)

  (set-face-attribute 'font-lock-constant-face nil
                      :inherit 'default)

  (set-face-attribute 'font-lock-type-face nil
                      :inherit 'default)

  (set-face-attribute 'font-lock-string-face nil
                      :weight 'light
                      :inherit 'default)

  (set-face-attribute 'font-lock-comment-face nil
                      :weight 'demibold
                      :inherit 'default)

  (set-face-attribute 'font-lock-doc-face nil
                      :weight 'demibold
                      :inherit 'default)

  (with-eval-after-load 'magit
    (set-face-attribute 'magit-process-ng nil
                        :foreground rk-theme-base-red)

    (set-face-attribute 'magit-process-ok nil
                        :foreground rk-theme-base-green))

  (with-eval-after-load 'highlight-thing
    (set-face-attribute 'highlight-thing nil
                        :foreground nano-color-salient
                        :background nano-color-background
                        :weight 'normal)))

(advice-add 'enable-theme :after #'config-themes--after-enable-theme)

;; (use-package tree-sitter
;;   :straight t)

;; (use-package tree-sitter-langs
;;   :after tree-sitter)

(provide 'rk-themes)

;;; rk-themes.el ends here
