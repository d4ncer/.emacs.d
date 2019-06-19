;;; rk-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'definers)
(require 'lsp)
(require 'paths)

(use-package python
  :after lsp-mode
  :general
  (:keymaps 'python-mode-map
            "<backspace>" nil
            "DEL" nil)
  :hook (python-mode . lsp))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package pyvenv
  :straight t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :hook (python-mode . rk-py/pyvenv-activate-if-found)
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")

    (defvar rk-py/venv-names '(".env" "env" ".venv" "venv" ".virtualenv"))

    (defun rk-py/directory-first-ancestor (dir pred)
      "Search up the filesystem for the first DIR satisfying PRED.
Return the first non-nil result of evalutating PRED."
      (let (result)
        (while dir
          (pcase (funcall pred dir)
            (`nil
             (setq dir (f-parent dir)))
            (res
             (setq result res)
             (setq dir nil))))
        result))

    (defun rk-py/find-venv-in-directory (dir)
      (-when-let ((dir) (--keep (let ((dir (f-join dir it)))
                                 (when (f-directory? dir)
                                   dir))
                               rk-py/venv-names))
        (file-truename dir)))

    (defun rk-py/pyvenv-dir ()
      (rk-py/directory-first-ancestor default-directory
                                      #'rk-py/find-venv-in-directory))

    (defun rk-py/pyvenv-activate-if-found ()
      (-when-let (env (rk-py/pyvenv-dir))
        (pyvenv-activate env)
        (message "Using pyvenv at %s" (f-abbrev env))))

    (defun rk-py/pyvenv-init (env)
      (interactive
       (list (or (rk-py/pyvenv-dir)
                 (f-join (read-directory-name "Project root: " nil nil t) ".env"))))
      (when (f-dir? env)
        (user-error "Environment already exists"))
      (let ((reporter (make-progress-reporter "Initializing pyvenv environment...")))
        (pcase (call-process "pyvenv" nil nil nil env)
          (`0
           (progress-reporter-update reporter)
           (pyvenv-activate env)
           (progress-reporter-done reporter))
          (_
           (message "%sFAILED" (aref (cdr reporter) 3)))))))
  :config
  (rk-local-leader-def :keymaps 'python-mode-map
    "e" '(rk-py/pyvenv-init :wk "init pyvenv")))

;; TODO: Add this back once it works, seems to be broken at the moment
;; See https://github.com/emacs-lsp/lsp-python-ms/issues/13
;; (use-package lsp-python-ms
;;   :after (lsp-mode python)
;;   :straight (:host github :repo "emacs-lsp/lsp-python-ms"
;;                    :branch "master")
;;   :preface
;;   (defconst rk-python--lsp-ms-dir (expand-file-name "~/go/src/github.com/Microsoft/python-language-server/"))
;;   :custom
;;   (lsp-python-ms-cache-dir (f-join paths-cache-directory ".mspyls"))
;;   (lsp-python-ms-executable (f-join rk-python--lsp-ms-dir "output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")))

(provide 'rk-python)

;;; rk-python.el ends here
