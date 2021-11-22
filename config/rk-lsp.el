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

(defvar rk-lsp--ui-menu-colors '("#e99ce8" "#bbbbff" "#ffbbff")
  "Colors to use for imenu.")

(use-package lsp-mode
  :straight t
  :preface
  (defun rk-lsp--lsp-company-mode-p ()
    (and (bound-and-true-p lsp-mode)
         (bound-and-true-p company-mode)))
  (defun rk-lsp--setup-company-backend ()
    (when (rk-lsp--lsp-company-mode-p)
      (set (make-local-variable 'company-backends) '(company-files company-capf))))
  (defun rk-lsp--maybe-disable-highlight-thing ()
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1)))
  (defun rk-lsp--maybe-setup-organize-imports ()
    (when (and (gethash "codeActionProvider" (lsp--server-capabilities))
               (not (or (eq major-mode 'typescript-mode)
                        (eq major-mode 'rk-web-tsx-mode))))
      (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)))
  (defun rk-lsp--maybe-setup-format-on-save ()
    (when (and (gethash "documentFormattingProvider" (lsp--server-capabilities))
               (not (or (eq major-mode 'rust-mode)
                        (eq major-mode 'rk-web-tsx-mode)
                        (eq major-mode 'typescript-mode))))
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
  (defun rk-lsp--setup-local-keybinds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "gd" #'lsp-find-definition
     "K" #'lsp-describe-thing-at-point)
    (general-define-key
     :states 'insert
     :keymaps 'local
     "C-." #'company-complete))
  (defun rk-lsp--setup-lsp ()
    (rk-lsp--maybe-setup-organize-imports)
    (rk-lsp--maybe-disable-highlight-thing)
    (rk-lsp--maybe-setup-format-on-save)
    (rk-lsp--setup-local-keybinds))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-snippet t)
  (lsp-enable-on-type-formatting nil)
  (lsp-session-file (f-join paths-cache-directory "lsp-session-v1"))
  (lsp-server-install-dir (f-join paths-cache-directory "lsp-servers"))
  (lsp-keymap-prefix "C-l")
  ;; (lsp-eslint-server-command '("node" "/home/rk/.local/eslint-server/server/out/eslintServer.js" "--stdio"))
  (lsp-diagnostics-attributes `((unnecessary :foreground ,rk-theme-base-solarized-b1)
                                (deprecated :strike-through t)))
  :init
  (add-hook 'lsp-after-open-hook #'rk-lsp--setup-lsp)
  (rk-local-leader-def :keymaps 'lsp-mode-map
    "l" '(:ignore t :wk "LSP")
    "l." '(lsp-format-buffer :wk "format")

    "li" '(lsp-ui-imenu :wk "imenu")

    "lr" '(lsp-rename :wk "rename")

    "ls" '(:ignore t :wk "session / workspace")
    "lsd" '(lsp-describe-session :wk "describe")
    "lsr" '(lsp-restart-workspace :wk "restart")
    "lsa" '(lsp-workspace-folders-add :wk "add folder")
    "lsr" '(lsp-workspace-folders-remove :wk "remove folder")
    "lss" '(lsp-workspace-folders-switch :wk "switch folder"))
  :config
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook #'rk-lsp--setup-company-backend)))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :preface
  (defun rk-lsp-ui--goto-impl ()
    (interactive)
    (if (gethash "implementationProvider" (lsp--server-capabilities))
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
  (lsp-ui-doc-border nano-color-subtle)
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-fontify 'always)
  :config
  (set-face-attribute 'lsp-ui-doc-background nil :background nano-color-background))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors 'rk-lsp--ui-menu-colors)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(use-package lsp-ivy
  :straight t
  :after lsp-mode
  :preface
  (defun rk-lsp-ivy--setup-local-keybinds ()
    (general-define-key
     :states 'normal
     :keybinds 'local
     "C-/" #'lsp-ivy-workspace-symbol))
  :hook
  (lsp-mode . rk-lsp-ivy--setup-local-keybinds))

(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-force-searcher 'rg))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
