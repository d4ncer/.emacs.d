;;; rk-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 's)
(require 'definers)
(require 'paths)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'evil-define-key "evil-core")

;;; TODO: Move this into it's own package
(use-package all-the-icons
  :straight t)

(use-package hydra
  :straight t
  :config
  (setq lv-use-separator t))

(use-package vlf
  :straight t
  :init
  (require 'vlf-setup))

(use-package world-time-mode
  :straight t
  :general
  (:keymaps 'world-time-table-mode-map :states 'normal
            "q" #'quit-window)
  :custom
  (display-time-world-list '(("Pacific/Auckland" "NZT")
                             ("Australia/Melbourne" "AEDT")
                             ("Asia/Kolkata" "India")
                             ("UTC" "UTC")))
  :init
  (rk-leader-def "a m w" '(world-time-list :wk "world time"))
  :config
  (add-hook 'world-time-table-mode-hook 'hl-line-mode))

(use-package popwin
  :straight t
  :config
  ;; Dirty test this regex
  ;; (let* ((trp (rx "*" (or "Cargo" "go" "Racer" "restclient" "lsp" "Ledger" "docker") (zero-or-more anything) "*"))
  ;;        (trp-1? (if (s-matches-p trp "*docker containers*") "YES" "NO")))
  ;;   (message trp-1?))
  (push (list (rx "*" (or "Cargo" "go" "Racer" "restclient" "Ledger" "lsp" "nim" "docker" "Help" "tide" "helpful") (zero-or-more anything) "*") :noselect t :regexp t) popwin:special-display-config)
  (popwin-mode 1))

(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

(use-package smex
  :straight t
  :commands (smex-initialize)
  :defines (smex-save-file)
  :config
  (setq smex-save-file (concat paths-cache-directory "/smex-items"))
  (smex-initialize))

(use-package json-mode
  :straight t
  :init
  (setq json-mode-auto-mode-list '(".babelrc" ".eslintrc" "composer.lock"))
  :preface
  (defun rk-json--disable-python-checker ()
    (with-eval-after-load 'flycheck
      (setq-local flycheck-disabled-checkers '(json-python-json))))
  (defun rk-json--format-region-or-buffer ()
    "Format region or buffer."
    (interactive)
    (if (use-region-p)
        (json-pretty-print (region-beginning) (region-end))
      (json-pretty-print (point-min) (point-max))))
  :init
  (rk-local-leader-def :keymaps 'json-mode-map
    "." '(rk-json--format-region-or-buffer :wk "format"))
  :config
  (add-hook 'json-mode-hook #'rk-json--disable-python-checker)
  (add-hook 'json-mode-hook #'lsp)
  (with-eval-after-load 'js
    (setq js-indent-level 2))
  (with-eval-after-load 'json-reformat
    (setq json-reformat:indent-width 2)))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun rk-csv--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'rk-csv--suppress-final-newline))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :straight t
  :if window-system
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  ;; (exec-path-from-shell-copy-env "LANG")
  ;; (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-initialize))

(use-package request
  :straight t)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(use-package pinentry
  :straight t
  :demand t
  :config
  (pinentry-start))

(use-package plantuml-mode
  :straight t
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-executable-path "plantuml"))

(use-package mini-frame
  :straight t)

(use-package ts
  :straight t)

(provide 'rk-basic-settings)

;;; rk-basic-settings.el ends here
