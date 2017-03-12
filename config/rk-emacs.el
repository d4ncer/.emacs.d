;;; rk-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Base variables & config

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'magit-anything-modified-p "magit-git")
(autoload 'magit-list-remotes "magit-git")
(autoload 'magit-process-buffer "magit-process")
(autoload 'magit-read-string-ns "magit-utils")
(autoload 'magit-read-url "magit-remote")
(autoload 'magit-run-git "magit-process")

(defvar magit-process-raise-error)

;; Config Paths

(defconst rk-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst rk-emacs-autosave-directory
  (concat user-emacs-directory "autosave"))

(defconst rk-emacs-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst rk-emacs-config-directory
  (concat user-emacs-directory "config"))

;; Commands for working with config subtrees

(defun rk-emacs--find-subtree-remote (subtree)
  (--find (equal (-last-item (s-split "/" it)) subtree)
          (magit-list-remotes)))

(defmacro rk-emacs--with-signal-handlers (step &rest body)
  (declare (indent 1))
  `(condition-case _
       (let ((magit-process-raise-error t))
         (message "%s" ,step)
         ,@body)
     (magit-git-error
      (error "%sfailed.  See %s" ,step (magit-process-buffer t)))
     (error
      (error "%sfailed" ,step ))))

(defun rk-emacs--read-new-remote ()
  (let* ((name (magit-read-string-ns "Remote name"))
         (url (magit-read-url "Remote url" (format "https://github.com/%s.git" name))))
    (rk-emacs--with-signal-handlers "Adding remote..."
      (magit-run-git "remote" "add" name url)
      name)))

(defun rk-emacs--assert-tree-not-dirty ()
  (require 'magit)
  (when (magit-anything-modified-p)
    (user-error "`%s' has uncommitted changes.  Aborting" default-directory)))

(defun rk-emacs-add-subtree (subtree remote)
  "Add a new SUBTREE at REMOTE."
  (interactive  (let ((default-directory user-emacs-directory))
                  (rk-emacs--assert-tree-not-dirty)
                  (let* ((remote (rk-emacs--read-new-remote))
                         (subtree (file-name-nondirectory remote)))
                    (list subtree remote))))
  (let ((default-directory user-emacs-directory))
    (rk-emacs--assert-tree-not-dirty)
    (run-hooks 'magit-credential-hook)

    (rk-emacs--with-signal-handlers "Fetching remote..."
      (magit-run-git "fetch" "-q" remote))

    (let* ((prefix (format "lisp/%s" subtree))
           (fullpath (f-join rk-emacs-lisp-directory subtree))
           (commit-message (format "'Add %s@master to %s'" remote prefix)))

      (rk-emacs--with-signal-handlers "Importing subtree..."
        (magit-run-git "subtree" "-q" "add" "--prefix" prefix remote "master" "--squash" "-m" commit-message))

      (rk-emacs--with-signal-handlers "Compiling..."
        (byte-recompile-directory fullpath 0))

      (message "Subtree `%s' added successfully." prefix))))

(defun rk-emacs-update-subtree (subtree &optional remote)
  "Update SUBTREE at REMOTE.
When called interactively, prompt for the subtree, then only
prompt for REMOTE if it cannot be determined."
  (interactive  (let ((default-directory user-emacs-directory))
                  (rk-emacs--assert-tree-not-dirty)
                  (let ((subtree (completing-read
                                  "Select subtree to update: "
                                  (-map #'f-filename (f-directories rk-emacs-lisp-directory))
                                  t)))
                    (list subtree
                          (or (rk-emacs--find-subtree-remote subtree)
                              (rk-emacs--read-new-remote))))))

  (let ((default-directory user-emacs-directory))
    (rk-emacs--assert-tree-not-dirty)
    (run-hooks 'magit-credential-hook)

    (rk-emacs--with-signal-handlers "Fetching remote..."
      (magit-run-git "fetch" "-q" remote))

    (let* ((prefix (format "lisp/%s" subtree))
           (fullpath (f-join rk-emacs-lisp-directory subtree))
           (commit-message (format "'Merge %s@master into %s'" remote prefix)))

      (rk-emacs--with-signal-handlers "Importing subtree..."
        (magit-run-git "subtree" "-q" "pull" "--prefix" prefix remote "master" "--squash" "-m" commit-message))

      (rk-emacs--with-signal-handlers "Compiling..."
        (byte-recompile-directory fullpath 0))

      (message "Subtree `%s' updated successfully." prefix))))

(defun rk-emacs-compile-subtree (subtree)
  "Force the byte compilation of SUBTREE."
  (interactive (list
                (completing-read "Select subtree to byte-recompile: "
                                 (-map #'f-filename (f-directories rk-emacs-lisp-directory))
                                 t)))
  (byte-recompile-directory (f-join rk-emacs-lisp-directory subtree) 0 t))

(provide 'rk-emacs)

;;; rk-emacs.el ends here
