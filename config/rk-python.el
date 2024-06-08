;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'which-func)
(require 'f)
(require 's)

(use-package python
  :preface
  (defun rk-python/unittest-at-point ()
    "Run unittest at point using the built-in Python test runner."
    (interactive)
    (if-let* ((test-function (which-function))
              (poetry-root (poetry-find-project-root))
              (test-file (file-name-sans-extension (buffer-file-name)))
              (diff-path (f-relative test-file poetry-root))
              (formatted-diff (s-replace "/" "." diff-path))
              (command (format "python -m unittest %s.%s" formatted-diff test-function))
              (default-directory poetry-root))
        (compile command)
      (message "Must be in a Poetry project.")))
  :config
  (rk-local-leader-def :keymaps 'python-ts-mode-map
    "t" '(rk-python/unittest-at-point :wk "test at point")))


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
