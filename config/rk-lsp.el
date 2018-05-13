;;; rk-lsp.el --- Configuration for LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :config
  (progn
    (autoload 'lsp-mode-hook "lsp")
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :config
  (progn
    (setq lsp-ui-imenu-colors '("#e99ce8" "#bbbbff" "#ffbbff"))
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(provide 'rk-lsp)

;;; rk-lsp.el ends here
