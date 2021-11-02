;;; rk-ruby.el --- Ruby config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ruby-mode
  :straight t
  :after lsp-mode
  :config
  (add-hook 'ruby-mode-hook #'lsp))

(use-package rvm
  :straight t
  :config
  (rvm-use-default))

(provide 'rk-ruby)

;;; rk-ruby.el ends here
