;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(when (version< emacs-version "28")
  (error "This version of Emacs is not supported"))

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Set frame resize behaviour

(setq frame-resize-pixelwise t)

;; Setup keychain

(defun keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.
Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script."
  (interactive)
  (let* ((ssh (shell-command-to-string "/opt/homebrew/bin/keychain -q --noask --agents ssh --eval"))
         (gpg (shell-command-to-string "/opt/homebrew/bin/keychain -q --noask --agents gpg --eval")))
    (list (and ssh
               (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
               (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
          (and ssh
               (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
               (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
          (and gpg
               (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
               (setenv       "GPG_AGENT_INFO" (match-string 1 gpg))))))

(keychain-refresh-environment)

;; Bootstrap straight

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Aggressively load in org to avoid shadowing

(straight-use-package 'org)

;; Setup cl-lib

(require 'cl-lib)

;; Install & require some basic packages

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'general)
(straight-use-package 'el-patch)
(straight-use-package 'pretty-hydra)
(straight-use-package 'pcache)

(require 'dash)
(require 'f)
(require 's)
(require 'general)
(require 'el-patch)
(require 'pretty-hydra)

;; Set up personal settings

(setq user-full-name "Raghuvir Kasturi")
(setq user-mail-address "raghuvir.kasturi@gmail.com")

(defconst use-package-verbose t)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'recentf)
  (require 'use-package))

;; Set up Vertico

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (let* ((dir (straight--el-get-package-directory 'vertico))
         (ext (f-join dir "extensions/")))
    (add-to-list 'load-path ext))
  :general (:keymaps 'vertico-map
                     "C-j" #'next-line-or-history-element
                     "C-k" #'previous-line-or-history-element
                     "C-<return>" #'vertico-exit-input))

;; Set up general to auto unbind keys (override everything)

(general-auto-unbind-keys)

;; Setup paths & features

(require 'paths (expand-file-name "paths.el" (concat user-emacs-directory "/config")))
(paths-initialise)
(add-to-list 'custom-theme-load-path paths-themes-directory)

;; Up sub-process throughput data size
;; https://github.com/emacs-mirror/emacs/commit/cc78faee7d23dd0433ba537818a68cbd20fa52a3

(setq read-process-output-max (* 1024 1024))


;; Ack org-roam V2

(defvar org-roam-v2-ack)
(setq org-roam-v2-ack t)

;; Aggressively load persist

(use-package persist
  :straight t
  :demand t
  :config
  (setq persist--directory-location (f-join paths-cache-directory "persist")))

;; Aggressively load themes

(use-package rk-themes)

;; Aggressively load flyspell-lazy
;; It has to load before flyspell
(use-package flyspell-lazy
  :straight t
  :demand t
  :commands (flyspell-lazy-mode)
  :custom
  (flyspell-lazy-idle-seconds 1)
  (flyspell-lazy-window-idle-seconds 3)
  :config
  (flyspell-lazy-mode +1))


;; Load features.

;; Base setup

(use-package rk-emacs)
(use-package rk-basic-settings)
(use-package rk-auto-insert)
(use-package rk-leader-keys)
(use-package rk-darwin :if (equal system-type 'darwin))

;; Editor capabilities

(use-package rk-evil)
(use-package rk-tree-sitter)
(use-package rk-completions)
(use-package rk-highlight-thing)
(use-package rk-auto-save)
(use-package rk-search)
(use-package rk-help)
(use-package rk-projectile)
(use-package rk-magit)
(use-package rk-parentheses)
(use-package rk-smartparens)
(use-package rk-restclient)
(use-package rk-dired)
(use-package rk-hl-todo)
(use-package rk-lsp)
(use-package rk-ws-butler)
(use-package rk-aggressive-indent)
(use-package rk-flymake)
(use-package rk-ibuffer)
(use-package rk-org)
(use-package rk-spelling)
(use-package rk-string)
(use-package rk-expand-region)
(use-package rk-yasnippet)
(use-package rk-prodigy)
(use-package rk-ledger)
(use-package rk-pdf)
(use-package rk-sql)
(use-package rk-mermaid)
(use-package rk-bazel)
(use-package rk-fmt)

;; Programming language support

(use-package rk-elisp)
(use-package rk-web-mode)
(use-package rk-typescript)
(use-package rk-graphql)
(use-package rk-haskell)
(use-package rk-go)
(use-package rk-markdown)
(use-package rk-yaml)
(use-package rk-docker)
(use-package rk-python)
(use-package rk-rust)
(use-package rk-nim)
(use-package rk-racket)
(use-package rk-protobuf)
(use-package rk-sh)
(use-package rk-hashicorp)
(use-package rk-nix)
(use-package rk-clojure)
(use-package rk-java)
(use-package rk-zig)
(use-package rk-c)
(use-package rk-elixir)

;; Setup envrc
(use-package rk-envrc)

(use-package private-config
  :when (f-dir-p "~/private")
  :load-path "~/private")

(use-package opam-user-setup
  :when (f-exists-p "~/.emacs.d/opam-user-setup.el")
  :load-path "~/.emacs.d/opam-user-setup.el")

;;; Post init setup.

;; Load keychain after everything else to ensure env is setup

(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

;;; Improve eval-expression

(defvar eval-expression-interactively-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map (kbd "<escape>") #'abort-minibuffers)
    (define-key map (kbd "C-g") #'abort-minibuffers)
    map))

(defun eval-expression-interactively--read (prompt &optional initial-contents)
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((inhibit-message t))
            (emacs-lisp-mode)
            (use-local-map eval-expression-interactively-map)
            (setq font-lock-mode t)
            (funcall font-lock-function 1)))
      (read-from-minibuffer prompt initial-contents
                            eval-expression-interactively-map nil
                            'read-expression-history))))

(autoload 'pp-display-expression "pp")
(autoload 'pp-to-string "pp")

(defun eval-expression-interactively (expression &optional arg)
  "Like `eval-expression' with nicer input handling.

- Use `emacs-lisp-mode' to provide font locking and better
  integration with other packages.

- Use the `pp' library to display the output in a readable form.

EXPRESSION is a Lisp form to evaluate.

With optional prefix ARG, insert the results into the buffer at
point."
  (interactive (list (read (eval-expression-interactively--read "Eval: "))
                     current-prefix-arg))
  (if arg
      (insert (pp-to-string (eval expression lexical-binding)))
    (pp-display-expression (eval expression lexical-binding)
                           "*Pp Eval Output*")))

(general-define-key :keymaps 'override :states '(normal motion visual)
                    "M-:" 'eval-expression-interactively)

;;; Hack

(general-unbind global-map "M-<return>")
(general-unbind org-mode-map "M-<return>")
(general-unbind org-capture-mode-map "M-<return>")

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed)))

(provide 'init)

;;; init.el ends here
