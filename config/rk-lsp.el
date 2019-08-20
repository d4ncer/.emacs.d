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
  (defun rk-lsp--maybe-disable-highlight-thing ()
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1)))
  (defun rk-lsp--maybe-setup-organize-imports ()
    (when (and (gethash "codeActionProvider" (lsp--server-capabilities))
               (eq major-mode 'go-mode))
      (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)))
  (defun rk-lsp--maybe-setup-format-on-save ()
    (when (and (gethash "documentFormattingProvider" (lsp--server-capabilities))
               (not (eq major-mode 'rust-mode)))
      (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
  (defun rk-lsp--setup-lsp ()
    (rk-lsp--maybe-setup-organize-imports)
    (rk-lsp--maybe-disable-highlight-thing)
    (rk-lsp--maybe-setup-format-on-save))
  :init
  (progn
    (setq lsp-prefer-flymake nil)
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-session-file (f-join paths-cache-directory "lsp-session-v1")))
  :general
  (:keymaps 'lsp-mode-map :states '(normal motion visual)
            "gd" #'lsp-find-definition
            "K" #'lsp-describe-thing-at-point)
  :config
  (progn
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
      "lss" '(lsp-workspace-folders-switch :wk "switch folder"))))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :general
  (:keymaps 'lsp-ui-imenu-mode-map :states '(normal visual)
            "q" #'lsp-ui-imenu--kill)
  (:keymaps 'lsp-ui-peek-mode-map
            "C-j" #'lsp-ui-peek--select-next
            "C-k" #'lsp-ui-peek--select-prev
            "C-n" #'lsp-ui-peek--select-next-file
            "C-p" #'lsp-ui-peek--select-prev-file
            "<C-return>" #'lsp-ui-peek--goto-xref-other-window)
  (:keymaps 'lsp-mode-map :states '(normal motion visual)
            "R" #'lsp-ui-peek-find-references
            "M" #'lsp-ui-peek-find-implementation)
  :init
  (progn
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-enable nil)))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors 'rk-lsp--ui-menu-colors)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
