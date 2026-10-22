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

;;; Leader key bindings
;; Loaded from mod-keybindings.el

;;; Modules

;; Load keybindings first (sets up general and leader key)
(require 'mod-keybindings)
(unless (featurep 'general)
  (error "General failed to initialize before feature declarations"))

;; Load core infrastructure
(require 'mod-core)

;; Load evil mode
(require 'mod-evil)

;; Load completion infrastructure
(require 'mod-completion)

;; Load UI and themes
(require 'mod-ui)

;; Load window management
(require 'mod-windows)

;; Load editor utilities
(require 'mod-editor)

;; Load navigation tools
(require 'mod-nav)

;; Load project management
(require 'mod-project)

;; Load git and version control
(require 'mod-git)

;; Load LSP and tree-sitter
(require 'mod-lsp)

;; Load code formatting
(require 'mod-format)

;; Load debugging tools
(require 'mod-debug)

;; Load dired file manager
(require 'mod-dired)

;; Load eshell
(require 'mod-eshell)

;; Load AI integration
(require 'mod-ai)

;; Load programming languages
(require 'mod-languages)

;; Load org mode (deferred)
(require 'mod-org)

;; Load miscellaneous utilities
(require 'mod-misc)

;; Load documentation systems
(require 'mod-docs)

;; Load text modes and templates
(require 'mod-text)

;; Load additional core utilities
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
