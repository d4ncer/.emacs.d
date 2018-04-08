;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)

(use-package python)

(use-package rk-lsp-python
  :after python
  :commands (rk-lsp-python--setup)
  :config
  (progn
    (autoload 'flycheck-mode "flycheck")
    (autoload 'python-mode-hook "python")
    (rk-lsp-python--setup)
    (add-hook 'python-mode-hook #'lsp-python-enable)
    (add-hook 'lsp-python-mode-hook 'flycheck-mode)))

(provide 'rk-python)

;;; rk-python.el ends here
