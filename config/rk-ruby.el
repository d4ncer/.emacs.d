;;; rk-ruby.el --- Ruby config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ruby-mode
  :straight t
  :after lsp-mode
  :preface
  (defun  rk-ruby--setup-lsp ()
    (setq-local lsp-diagnostics-provider :none)
    (setq-local flycheck-disabled-checkers '(lsp ruby))
    (lsp))
  :config
  (add-hook 'ruby-mode-hook #'rk-ruby--setup-lsp))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-solargraph-use-bundler nil))

(use-package rvm
  :straight t
  :if (executable-find "rvm")
  :config
  (rvm-use-default))

(use-package inf-ruby
  :straight t
  :after ruby-mode
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(provide 'rk-ruby)

;;; rk-ruby.el ends here
