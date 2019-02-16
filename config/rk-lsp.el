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
            "K" #'lsp-describe-thing-at-point
            "L" #'lsp-ui-peek-find-references)
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
  :general
  (:keymaps 'lsp-ui-peek-mode-map
            "C-j" #'lsp-ui-peek--select-next
            "C-k" #'lsp-ui-peek--select-prev
            "C-n" #'lsp-ui-peek--select-next-file
            "C-p" #'lsp-ui-peek--select-prev-file
            "<C-return>" #'lsp-ui-peek--goto-xref-other-window)
  :preface
  (defun rk-lsp-ui--disable-highlight-thing ()
    (highlight-thing-mode -1))
  :init
  (progn
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-enable nil))
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
