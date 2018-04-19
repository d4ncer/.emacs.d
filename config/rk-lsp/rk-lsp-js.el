;;; rk-lsp-js.el --- Configuration for JS/Flow LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'projectile-project-root "projectile")
(autoload 'lsp-define-stdio-client "lsp-mode")
(autoload 'lsp-make-traverser "lsp-mode")

(defvar lsp-js-flow nil "Name of JS/Flow LSP Client.")

(defun rk-lsp-js--find-js-root ()
  "Return the current Python project root, if any.
This is marked with setup.py or setup.cfg."
  (locate-dominating-file default-directory "package.json"))

(defun rk-lsp-js--find-git-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".git"))

(defun rk-lsp-js--find-projectile-root ()
  "Return the current project root according to projectile."
  ;; `ignore-errors' both to avoid an unbound function error as well
  ;; as ignore projectile saying there is no project root here.
  (ignore-errors
    (projectile-project-root)))

(defun rk-lsp-js--find-root ()
  (cond ((rk-lsp-js--find-js-root)
         (rk-lsp-js--find-projectile-root)
         (rk-lsp-js--find-git-root))))

;; (defun rk-lsp-js--get-lsp-root ()
;;   (if (rk-lsp-js--find-root) (rk-lsp-js--find-root) default-directory))

(defconst rk-lsp-js--get-root
  (lsp-make-traverser #'(lambda (dir)
                          (directory-files dir nil "package.json"))))
(defun rk-lsp-js--setup ()
  "Setup JavaScript LSP client."
  (lsp-define-stdio-client lsp-js-flow "javascript"
                           rk-lsp-js--get-root '("flow-language-server" "--stdio")
                           :ignore-messages '("\[INFO].*?nuclide")))

(provide 'rk-lsp-js)

;;; rk-lsp-js.el ends here
