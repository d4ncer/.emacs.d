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
  (defvar bootstrap-version 5)
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
(straight-use-package 'general)
(straight-use-package 'el-patch)

;; Set up general to auto unbind keys (override everything)

(general-auto-unbind-keys)

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
  (rk/themes/light-theme))

;; Load features.

;; Base setup

(use-package rk-leader-keys)
(use-package rk-basic-settings)
(use-package rk-darwin :if (equal system-type 'darwin))

;; Editor capabilities

(use-package rk-modeline)
(use-package rk-highlight-thing)
(use-package rk-auto-save)
(use-package rk-evil)
(use-package rk-ivy)
(use-package rk-search)
(use-package rk-help)
(use-package rk-projectile)
(use-package rk-magit)
(use-package rk-parentheses)
(use-package rk-smartparens)
(use-package rk-company)
(use-package rk-restclient)
(use-package rk-dired)
(use-package rk-hl-todo)
(use-package rk-lsp)
(use-package rk-ws-butler)
(use-package rk-aggressive-indent)
(use-package rk-flycheck)
(use-package rk-ibuffer)
(use-package rk-treemacs)
(use-package rk-org)
(use-package rk-spelling)
(use-package rk-string)
(use-package rk-lobsters)
(use-package rk-expand-region)
(use-package rk-yasnippet)
(use-package rk-prodigy)
(use-package rk-ledger)
(use-package rk-eshell)

;; Programming language support

(use-package rk-elisp)
(use-package rk-web-mode)
(use-package rk-typescript)
(use-package rk-coffeescript)
(use-package rk-haskell)
(use-package rk-go)
(use-package rk-scala)
(use-package rk-markdown)
(use-package rk-yaml)
(use-package rk-rnc)
(use-package rk-docker)
(use-package rk-groovy)
(use-package rk-python)
(use-package rk-php)
(use-package rk-rust)
(use-package rk-nim)
(use-package rk-racket)
(use-package rk-janet)
(use-package rk-protobuf)
(use-package rk-puppet)
(use-package rk-sh)
(use-package rk-hashicorp)

(use-package private-config
  :load-path "~/private")

(use-package opam-user-setup
  :load-path "~/.emacs.d/opam-user-setup.el")

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
