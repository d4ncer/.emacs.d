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

(provide 'rk-themes)

;;; rk-themes.el ends here
