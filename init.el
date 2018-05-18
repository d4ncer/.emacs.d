;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(when (version< emacs-version "26")
  (error "This version of Emacs is not supported"))

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Bootstrap straight

(setq package-enable-at-startup nil)

(eval-and-compile
  (defvar bootstrap-version 3)
  (defvar bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(defconst straight-cache-autoloads t)
(defconst straight-check-for-modifications 'live)

(require 'straight bootstrap-file t)

;; Install some basic packages

(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'noflet)
(straight-use-package 'memoize)

;; Set up personal settings

(setq user-full-name "Raghuvir Kasturi")
(setq user-mail-address "raghuvir.kasturi@gmail.com")

(defconst use-package-verbose t)

(straight-use-package 'use-package)
(straight-use-package 'bind-map)

(eval-when-compile
  (require 'recentf)
  (require 'use-package))

;; Setup paths & features

(require 'paths (expand-file-name "paths.el" (concat user-emacs-directory "/config")))
(paths-initialise)
(add-to-list 'custom-theme-load-path paths-themes-directory)

;; Aggressively load in org-plus-contrib to avoid shadowing

(straight-use-package 'org-plus-contrib)

;; Aggressively load themes

(use-package rk-themes
  :config
  (danc--themes/light-theme))

;; Load features.

(use-package rk-emacs)
(use-package rk-basic-settings)
(use-package rk-modeline)
(use-package rk-auto-save)
(use-package rk-leader-keys)
(use-package rk-evil)
(use-package rk-ivy)
(use-package rk-darwin :if (equal system-type 'darwin))
(use-package rk-search)
(use-package rk-help)
(use-package rk-projectile)
(use-package rk-restclient)
(use-package rk-dired)
(use-package rk-hl-todo)
(use-package rk-magit)
(use-package rk-smartparens)
(use-package rk-lsp)
(use-package rk-company)
(use-package rk-undo-tree)
(use-package rk-ws-butler)
(use-package rk-parentheses)
(use-package rk-aggressive-indent)
(use-package rk-flycheck)
(use-package rk-ibuffer)
(use-package rk-treemacs)
(use-package rk-coffeescript)
(use-package rk-haskell)
(use-package rk-elisp)
(use-package rk-web-mode)
(use-package rk-go)
(use-package rk-scala)
(use-package rk-markdown)
(use-package rk-yaml)
(use-package rk-rnc)
(use-package rk-highlight-thing)
(use-package rk-spelling)
(use-package rk-string)
(use-package rk-docker)
(use-package rk-groovy)
(use-package rk-lobsters)
(use-package rk-expand-region)
(use-package rk-python)
(use-package rk-rust)
(use-package rk-org)
(use-package rk-yasnippet)

;;; Post init setup.

(unless (file-directory-p org-directory)
  (when (y-or-n-p (format "`org-directory' does not exist. Create at %s? " org-directory))
    (mkdir org-directory)))

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(provide 'init)

;;; init.el ends here
