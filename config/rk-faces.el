;;; rk-faces.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Load themes.

(let ((this-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'custom-theme-load-path (concat this-dir "rk-faces/")))

(load-theme 'rk-light t)

(use-package rk-ligatures
  :if (window-system)
  :preface
  (autoload 'rk-setup-fira-code-ligatures "rk-ligatures")
  :config
  (rk-setup-fira-code-ligatures))

(provide 'rk-faces)

;;; rk-faces.el ends here
