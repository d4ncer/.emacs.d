;;; rk-c.el --- Basic C/C++ config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package cc-mode
  :config
  (add-hook 'c++-mode-hook #'lsp-deferred))

(provide 'rk-c)

;;; rk-c.el ends here
