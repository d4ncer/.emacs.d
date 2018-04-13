;;; rk-lsp-python.el --- Configuration for Python LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(autoload 'projectile-project-root "projectile")
(autoload 'lsp-define-stdio-client "lsp-mode")

(defvar lsp-python nil "Name of Python LSP Client.")

(defun rk-lsp-python--find-python-root ()
  "Return the current Python project root, if any.
This is marked with setup.py or setup.cfg."
  (or (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory "setup.cfg")))

(defun rk-lsp-python--find-git-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".git"))

(defun rk-lsp-python--find-projectile-root ()
  "Return the current project root according to projectile."
  ;; `ignore-errors' both to avoid an unbound function error as well
  ;; as ignore projectile saying there is no project root here.
  (ignore-errors
    (projectile-project-root)))

(defun rk-lsp-python--find-root ()
  (cond ((rk-lsp-python--find-python-root)
         (rk-lsp-python--find-projectile-root)
         (rk-lsp-python--find-git-root))))

(defun rk-lsp-python--get-lsp-root ()
  (if (rk-lsp-python--find-root) (rk-lsp-python--find-root) default-directory))

(defun rk-lsp-python--setup ()
  "Setup Python LSP client."
  (lsp-define-stdio-client lsp-python "python" #'rk-lsp-python--get-lsp-root '("pyls")))

(provide 'rk-lsp-python)

;;; rk-lsp-python.el ends here
