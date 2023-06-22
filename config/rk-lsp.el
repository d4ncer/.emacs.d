;;; rk-lsp.el --- Configuration for LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'f)
(require 'definers)
(require 'rk-colors)

(defvar rk-lsp--ui-menu-colors '("#e99ce8" "#bbbbff" "#ffbbff")
  "Colors to use for imenu.")

(use-package lsp-mode
  :straight t
  :preface
  (defun rk-lsp--maybe-disable-highlight-thing ()
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1)))
  (defun rk-lsp--maybe-setup-organize-imports ()
    (when (and (gethash "codeActionProvider" (lsp--server-capabilities))
               (not (or (eq major-mode 'typescript-ts-mode)
                        (eq major-mode 'tsx-ts-mode)
                        (eq major-mode 'js-ts-mode))))
      (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)))
  (defun rk-lsp--maybe-setup-format-on-save ()
    (when (and (gethash "documentFormattingProvider" (lsp--server-capabilities))
               (not (or (eq major-mode 'rust-mode)
                        (eq major-mode 'json-mode)
                        (eq major-mode 'tsx-ts-mode)
                        (eq major-mode 'js-ts-mode)
                        (eq major-mode 'typescript-ts-mode))))
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
  (defun rk-lsp--setup-local-keybinds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "gd" #'lsp-find-definition
     "K" #'lsp-describe-thing-at-point
     "T" #'lsp-goto-type-definition))
  (defun rk-lsp--setup-lsp ()
    (rk-lsp--maybe-setup-organize-imports)
    (rk-lsp--maybe-disable-highlight-thing)
    (rk-lsp--maybe-setup-format-on-save)
    (rk-lsp--setup-local-keybinds))
  :custom
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-snippet t)
  (lsp-enable-on-type-formatting nil)
  (lsp-session-file (f-join paths-cache-directory "lsp-session-v1"))
  (lsp-server-install-dir (f-join paths-cache-directory "lsp-servers"))
  (lsp-keymap-prefix "C-l")
  ;; (lsp-eslint-server-command '("node" "/home/rk/.local/eslint-server/server/out/eslintServer.js" "--stdio"))
  (lsp-diagnostics-attributes `((unnecessary :foreground ,rk-colors-solarized-b1)
                                (deprecated :strike-through t)))
  :init
  (add-hook 'lsp-after-open-hook #'rk-lsp--setup-lsp)
  (rk-local-leader-def :keymaps 'lsp-mode-map
    "l" '(:ignore t :wk "LSP")
    "l." '(lsp-format-buffer :wk "format")

    "li" '(lsp-ui-imenu :wk "imenu")

    "lo" '(lsp-organize-imports :wk "fix imports")

    "lr" '(lsp-rename :wk "rename")

    "lx" '(lsp-execute-code-action :wk "exec action")

    "ls" '(:ignore t :wk "session / workspace")
    "lsd" '(lsp-describe-session :wk "describe")
    "lsr" '(lsp-restart-workspace :wk "restart")
    "lsa" '(lsp-workspace-folders-add :wk "add folder")
    "lsr" '(lsp-workspace-folders-remove :wk "remove folder")
    "lss" '(lsp-workspace-folders-switch :wk "switch folder")))

(use-package emacs
  :after lsp-mode
  :init
  (advice-add 'json-parse-string :around
              (lambda (orig string &rest rest)
                (apply orig (s-replace "\\u0000" "" string)
                       rest)))
  (advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest))))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :preface
  (autoload 'dumb-jump-go "dump-jump")
  (defun rk-lsp-ui--goto-impl ()
    (interactive)
    (if (or (eq major-mode 'java-mode)
            (gethash "implementationProvider" (lsp--server-capabilities)))
        (call-interactively #'lsp-ui-peek-find-implementation)
      (call-interactively #'dumb-jump-go)))
  (defun rk-lsp-ui--setup-local-keybinds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "R" #'lsp-ui-peek-find-references
     "M" #'rk-lsp-ui--goto-impl))
  :hook
  (lsp-mode . rk-lsp-ui--setup-local-keybinds)
  :general
  (:keymaps 'lsp-ui-imenu-mode-map :states '(normal visual)
            "q" #'lsp-ui-imenu--kill)
  (:keymaps 'lsp-ui-peek-mode-map
            "C-j" #'lsp-ui-peek--select-next
            "C-k" #'lsp-ui-peek--select-prev
            "C-n" #'lsp-ui-peek--select-next-file
            "C-p" #'lsp-ui-peek--select-prev-file
            "<C-return>" #'lsp-ui-peek--goto-xref-other-window)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-fontify 'always))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors 'rk-lsp--ui-menu-colors)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-selector 'completing-read)
  (dumb-jump-force-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package consult-lsp
  :straight (:host github :repo "gagbo/consult-lsp" :branch "main")
  :after lsp-mode
  :config
  (rk-local-leader-def :keymaps 'lsp-mode-map
    "ld" '(consult-lsp-diagnostics :wk "diagnostics")))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
