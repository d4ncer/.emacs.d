;;; rk-lsp.el --- Configuration for LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; TODO: Need to add bindings
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :preface
  (defun rk-lsp-ui--disable-highlight-thing ()
    (setq-local lsp-ui-flycheck-live-reporting nil)
    (highlight-thing-mode -1))
  :init
  (setq lsp-ui-doc-fringe-p nil)
  :config
  (progn
    (autoload 'lsp-mode-hook "lsp")
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    (add-hook 'lsp-mode-hook #'rk-lsp-ui--disable-highlight-thing)))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors '("#e99ce8" "#bbbbff" "#ffbbff"))
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
