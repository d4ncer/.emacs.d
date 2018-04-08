;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)

(use-package lsp-mode
  :commands (lsp-define-stdio-client)
  :preface
  (progn
    (autoload 'projectile-project-root "projectile")

    (defun rk-python--find-python-root ()
      "Return the current Python project root, if any.
This is marked with setup.py or setup.cfg."
      (or (locate-dominating-file default-directory "setup.py")
          (locate-dominating-file default-directory "setup.cfg")))

    (defun rk-python--find-git-root ()
      "Return the current git repository root, if any."
      (locate-dominating-file default-directory ".git"))

    (defun rk-python--find-projectile-root ()
      "Return the current project root according to projectile."
      ;; `ignore-errors' both to avoid an unbound function error as well
      ;; as ignore projectile saying there is no project root here.
      (ignore-errors
        (projectile-project-root)))

    (defun rk-python--find-root ()
      (cond ((rk-python--find-git-root)
             (rk-python--find-git-root)
             (rk-python--find-projectile-root))))

    (defun rk-python--get-lsp-root ()
      (if (rk-python--find-root) (rk-python--find-root) default-directory)))

  :config
  (lsp-define-stdio-client lsp-python-major-mode "python" #'rk-python--get-lsp-root '("pyls")))

(use-package python
  :commands (python-mode-hook)
  :config
  (add-hook 'python-mode-hook #'lsp-python-major-mode-enable))

(provide 'rk-python)

;;; rk-python.el ends here
