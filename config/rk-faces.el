;;; rk-faces.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Load internal themes.

(let ((this-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'custom-theme-load-path (concat this-dir "rk-faces/")))

(load-theme 'rk-light t)

;;; Can't use ligatures at the moment due to breakage in non-prog mode buffers

;; (use-package rk-ligatures
;;   :if (window-system)
;;   :preface
;;   (autoload 'rk-setup-fira-code-ligatures "rk-ligatures")
;;   :config
;;   (add-hook 'prog-mode-hook #'rk-setup-fira-code-ligatures))

;;; Add custom themes here

(use-package solarized)
(use-package base16-theme
  :ensure t)
(require 'doom-themes)

(provide 'rk-faces)

;;; rk-faces.el ends here
