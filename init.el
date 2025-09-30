;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(require 'use-package)

;; Bootstrap elpaca package manager
(load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

;; Setup elpaca use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Add modules directory to load-path
(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path (file-name-concat user-emacs-directory "modules/")))

;;; Leader key bindings
;; Loaded from mod-keybindings.el

;;; Modules

;; Load keybindings first (sets up general and leader key)
(require 'mod-keybindings)

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

;;; Post init setup.

;; Load keychain after everything else to ensure env is setup

(use-package keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed)))

(use-package private-config
  :when (file-directory-p "~/private")
  :load-path "~/private")

(provide 'init)

;;; init.el ends here
