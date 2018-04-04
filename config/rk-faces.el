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

(use-package rk-ligatures
  :if (window-system)
  :preface
  (autoload 'rk-ligatures--set-kwds "rk-ligatures")
  :config
  (rk-ligatures--set-kwds
   '((rk-ligatures--fira-font prog-mode-hook))))

;;; Add custom themes here

(use-package solarized)
(use-package base16-theme
  :ensure t)
(require 'doom-themes)

(provide 'rk-faces)

;;; rk-faces.el ends here
