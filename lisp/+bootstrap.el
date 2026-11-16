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

(defun +bootstrap-shared-packages (&optional root)
  "Return why the checkout at ROOT shares packages it does not own, or nil.
It does as a linked git worktree, whose .git is a file, or when its
elpaca directory is a symlink.  ROOT defaults to `+config-root',
or `user-emacs-directory' before setup."
  (let* ((root (file-name-as-directory
                (or root +config-root user-emacs-directory)))
         (elpaca (expand-file-name "elpaca" root)))
    (cond ((file-regular-p (expand-file-name ".git" root))
           (format "%s is a linked git worktree" (abbreviate-file-name root)))
          ((file-symlink-p elpaca)
           (format "%s links to %s" (abbreviate-file-name elpaca)
                   (abbreviate-file-name (file-truename elpaca)))))))

(with-eval-after-load 'elpaca-menu-melpa (require '+elpaca-patch))

(with-eval-after-load 'elpaca (require '+elpaca-guard))

(provide '+bootstrap)
;;; +bootstrap.el ends here
