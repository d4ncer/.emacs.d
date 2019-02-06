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
  :init
  (progn
    (setq lsp-prefer-flymake nil)
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-session-file (f-join paths-cache-directory "lsp-session-v1")))
  :general
  (:keymaps 'lsp-mode-map :states 'normal
            "K" #'lsp-describe-thing-at-point)
  :config
  (rk-local-leader-def :keymaps 'lsp-mode-map
    "l" '(:ignore t :wk "LSP")
    "l." '(lsp-format-buffer :wk "format")

    "ls" '(:ignore t :wk "session / workspace")
    "lsd" '(lsp-describe-session :wk "describe")
    "lsr" '(lsp-restart-workspace :wk "restart")
    "lsa" '(lsp-workspace-folders-add :wk "add folder")
    "lsr" '(lsp-workspace-folders-remove :wk "remove folder")
    "lss" '(lsp-workspace-folders-switch :wk "switch folder")))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :preface
  (defun rk-lsp-ui--disable-highlight-thing ()
    (highlight-thing-mode -1))
  :init
  (setq lsp-ui-sideline-enable nil)
  :config
  (progn
    (add-hook 'lsp-mode-hook #'rk-lsp-ui--disable-highlight-thing)))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors 'rk-lsp--ui-menu-colors)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
