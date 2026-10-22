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
                      (expand-file-name "agent-shell-projects/" +lisp-dir)
                      (expand-file-name "project-files/" +lisp-dir)
                      (expand-file-name "compat/" +lisp-dir)
                      +site-lisp-directory))
    (when (and path (file-directory-p path)) (add-to-list 'load-path path)))
  (require 'use-package)
  (require '+corelib)
  (require '+use-package)
  (setq use-package-verbose init-file-debug
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01
        use-package-hook-name-suffix nil)
  (+use-package-setup))

(provide '+bootstrap)
;;; +bootstrap.el ends here
