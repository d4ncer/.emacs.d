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

(use-package inf-ruby
  :straight t
  :after ruby-mode
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(provide 'rk-ruby)

;;; rk-ruby.el ends here
