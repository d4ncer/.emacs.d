;;; +bootstrap.el --- Local configuration infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Establish local paths and declaration conventions before loading modules.

;;; Code:

(defvar +config-root nil)
(defvar +private-config-directory (expand-file-name "~/private/"))
(defvar +lisp-dir nil)
(defvar +ligatures-dir nil)
(defvar +templates-dir nil)
(defvar +site-lisp-directory (expand-file-name "~/life/elisp/"))

(defun +bootstrap-setup (&optional directory)
  "Set up the configuration rooted at DIRECTORY, or `user-emacs-directory'."
  (when (< emacs-major-version 32) (user-error "Emacs 32 required"))
  (setq +config-root (file-name-as-directory
                      (expand-file-name (or directory user-emacs-directory)))
        +lisp-dir (expand-file-name "lisp/" +config-root)
        +ligatures-dir (expand-file-name "ligatures/" +config-root)
        +templates-dir (expand-file-name "templates/" +config-root))
  (dolist (path (list +lisp-dir (expand-file-name "modules/" +config-root)
                      (expand-file-name "project-files/" +lisp-dir)
                      (expand-file-name "compat/" +lisp-dir)))
    (when (file-directory-p path) (add-to-list 'load-path path)))
  ;; Appended, so an external library never shadows one in this repository.
  (when (and +site-lisp-directory (file-directory-p +site-lisp-directory))
    (add-to-list 'load-path +site-lisp-directory t))
  (require 'use-package)
  (require '+corelib)
  (require '+use-package)
  (setq use-package-verbose init-file-debug
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01
        use-package-hook-name-suffix nil)
  (+use-package-setup))

(declare-function elpaca-process-call "elpaca-process")

(declare-function cl-remove-if "cl-seq")

(defun +bootstrap--melpa-clone (path)
  "Clone MELPA's recipes to PATH from its `main' branch.
Overrides `elpaca-menu-melpa--clone', which still pulls `master' after
MELPA renamed it (https://github.com/progfolio/elpaca/issues/585).
Remove this once Elpaca clones `main'."
  (let ((default-directory path))
    (make-directory path t)
    (make-directory (expand-file-name ".git/info/" path) t)
    (message "Downloading MELPA recipes...")
    (let* ((process-environment (append '("GIT_CONFIG_SYSTEM=/dev/null"
                                          "GIT_CONFIG_GLOBAL=/dev/null")
                                        process-environment))
           (processes
            (list
             (elpaca-process-call "git" "init")
             (elpaca-process-call "git" "config" "core.sparseCheckout" "true")
             (with-temp-buffer
               (insert "recipes")
               (append-to-file (point-min) (point-max)
                               (expand-file-name ".git/info/sparse-checkout" path)))
             (elpaca-process-call "git" "remote" "add" "origin"
                                  "https://github.com/melpa/melpa.git")
             (elpaca-process-call "git" "pull" "--depth=1" "origin" "main")
             (elpaca-process-call "git" "checkout" "main")
             (elpaca-process-call "git" "branch" "--set-upstream-to"
                                  "origin/main" "main")))
           (err (car (cl-remove-if #'zerop (delq nil processes) :key #'car))))
      (when err (error "Unable to clone MELPA: %S" err))
      (message "Downloading MELPA recipes...100%%"))))

(with-eval-after-load 'elpaca-menu-melpa
  (advice-add 'elpaca-menu-melpa--clone :override #'+bootstrap--melpa-clone))

(provide '+bootstrap)
;;; +bootstrap.el ends here
