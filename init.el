;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(unless (featurep '+bootstrap)
  (load (expand-file-name "lisp/+bootstrap.el" user-emacs-directory) nil t)
  (+bootstrap-setup))

;; Bootstrap elpaca package manager
(load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

;; Setup elpaca use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)
(unless (bound-and-true-p elpaca-use-package-mode)
  (error "Elpaca use-package integration failed to initialize"))

;;; Modules

;; Keybindings come first because later modules bind keys through General.
(require 'mod-keybindings)
(unless (featurep 'general)
  (error "General failed to initialize before feature declarations"))

(require 'mod-core)
(require 'mod-evil)
(require 'mod-completion)
(require 'mod-ui)
(require 'mod-windows)
(require 'mod-editor)
(require 'mod-nav)
(require 'mod-project)
(require 'mod-git)
(require 'mod-lsp)
(require 'mod-format)
(require 'mod-debug)
(require 'mod-dired)
(require 'mod-eshell)
(require 'mod-ai)
(require 'mod-languages)
(require 'mod-org)
(require 'mod-misc)
(require 'mod-docs)
(require 'mod-text)
(require 'mod-core-extras)

;;; Print overall startup time.

(unless noninteractive
  (message "Configuration declarations loaded in %.3fs"
           (float-time (time-subtract (current-time) before-init-time))))

(use-package private-config
  :ensure nil
  :demand t
  :when (and +private-config-directory
             (file-directory-p +private-config-directory))
  :load-path +private-config-directory)

(provide 'init)

;;; init.el ends here
