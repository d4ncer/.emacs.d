;;; rk-java.el --- Basic Java config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-java
  :straight t
  :after lsp
  :config
  (add-hook 'java-mode-hook #'lsp))

(provide 'rk-java)

;;; rk-java.el ends here
