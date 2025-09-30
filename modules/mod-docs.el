;;; mod-docs.el --- Documentation systems -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains documentation and help system configurations:
;; - help (Emacs help system)
;; - help-fns (help function utilities)
;; - help-mode (help buffer major mode)
;; - eldoc (inline documentation)
;; - info (texinfo manual reader)
;; - man (manpage reader)
;; - imenu (buffer navigation)

;;; Code:

;;; Documentation systems

(use-package help
  ;; Defines the main help functionality for Emacs & Emacs-Lisp.
  :hook (help-mode-hook . turn-on-visual-line-mode)
  :custom
  (help-window-select t)
  :general
  (:keymaps 'help-map
            "h" nil ; view-hello-file: never intended, always annoying
            "l" #'find-library
            "c" #'describe-face
            "P" #'describe-text-properties))

(use-package help-fns
  :general
  (:keymaps 'help-map
            "K" #'describe-keymap))

(use-package help-mode
  ;; Major-mode for help buffers.
  :general (:keymaps 'help-mode-map :states 'normal
                     "^" #'help-go-back
                     "M-n" #'forward-button
                     "M-p" #'backward-button
                     "C-n" #'forward-button
                     "C-p" #'backward-button))

(use-package eldoc
  ;; Display help hints in the echo area as you move around.
  :config
  ;; Teach eldoc to re-run after these commands.
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))

(use-package info
  ;; Emacs' built-in system for reading texinfo manuals.
  :general
  (:keymaps 'help-map "s" #'info-apropos)
  (:keymaps 'Info-mode-map :states 'normal
            "^" #'Info-up
            "C-n" #'Info-forward-node
            "C-p" #'Info-backward-node))

(use-package man
  ;; Built-in manpage reader.
  :custom
  ;; Tell man to use pop-to-buffer under the hood, which uses display-buffer and
  ;; selects the window.
  (Man-notify-method 'aggressive))

(use-package imenu
  ;; Emacs' built-in navigator for points of interest in a buffer.
  :general-config
  (:keymaps 'Info-mode-map [remap consult-imenu] #'Info-menu))

(provide 'mod-docs)
;;; mod-docs.el ends here
