;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package python)

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package lsp-pyright
  :after (lsp-mode python)
  :straight t
  :preface
  (defun rk-python--setup-lsp ()
    (lsp-deferred))
  :hook (python-mode . rk-python--setup-lsp))

(provide 'rk-python)

;;; rk-python.el ends here
