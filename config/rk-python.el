;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)
(require 'definers)

(use-package python)

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :custom
  (pipenv-with-flycheck nil))

(use-package lsp-pyright
  :after (lsp-mode python)
  :straight t
  :preface
  (defun rk-python--setup-lsp ()
    (lsp-deferred))
  :hook (python-mode . rk-python--setup-lsp))

(use-package python-black
  :straight (:type git :host github :repo "wbolster/emacs-python-black" :branch "main")
  :after python
  :init
  (rk-local-leader-def :keymaps 'python-mode-map
    "." '(python-black-buffer :wk "format buffer")))

(provide 'rk-python)

;;; rk-python.el ends here
