;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package python)

(use-package python
  :after eglot
  :hook
  (python-ts-mode . eglot-ensure))

(use-package pipenv
  :straight t
  :hook (python-ts-mode . pipenv-mode)
  :custom
  (pipenv-with-flycheck nil))

(use-package poetry
  :straight t)

(provide 'rk-python)

;;; rk-python.el ends here
