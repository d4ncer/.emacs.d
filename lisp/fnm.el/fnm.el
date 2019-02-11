;;; fnm.el --- Managing Node versions via fnm  -*- lexical-binding: t; -*-

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>
;; Maintainer: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>
;; Version: 0.0.1
;; Keywords: node, fnm
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0") (dash-functional "2.4.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; This is HEAVILY copied from nvm.el by Johan Andersson

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'dash-functional)

(defgroup fnm nil
  "Manage Node versions within Emacs via fnm"
  :prefix "fnm-"
  :group 'tools)

(defconst fnm-version-re
  "v[0-9]+\.[0-9]+\.[0-9]+"
  "Regex matching a Node version.")

(defconst fnm-runtime-re
  "\\(?:versions/node/\\|versions/io.js/\\)?")

(defcustom fnm-dir (f-full "~/.fnm")
  "Full path to fnm installation directory."
  :group 'fnm
  :type 'directory)

(defvar fnm-current-version nil
  "Current active version.")

(defun fnm--installed-versions ()
  (let ((match-fn (lambda (directory)
                    (s-matches? (concat fnm-version-re "$") (f-filename directory)))))
    (fnm--version-directories match-fn)))

(defun fnm--version-directories (match-fn)
  (--map (list (f-filename it) it) (f-directories (f-join fnm-dir "node-versions") match-fn)))

(defun fnm--version-installed? (version)
  "Return non-nil if VERSION is installed."
  (--any? (string= (car it) version) (fnm--installed-versions)))

(defun fnm--find-exact-version-for (short)
  "Find most suitable version for SHORT.
SHORT is a string containing major and minor version.  This
function will return the most recent patch version."
  (when (s-matches? "v?[0-9]+\.[0-9]+\\(\.[0-9]+\\)?$" short)
    (unless (or (s-starts-with? "v" short)
                (s-starts-with? "node" short)
                (s-starts-with? "iojs" short))
      (setq short (concat "v" short)))
    (let* ((versions (fnm--installed-versions))
           (first-version
            (--first (string= (car it) short) versions)))
      (if first-version
          first-version
        (let ((possible-versions
               (-filter
                (lambda (version)
                  (s-contains? short (car version)))
                versions)))
          (-min-by (-on 'string< (lambda (version) (car version)))
                   possible-versions))))))

;;;###autoload
(defun fnm-use (version &optional callback)
  "Activate Node VERSION.
If CALLBACK is specified, active in that scope and then reset to
previously used version."
  (setq version (fnm--find-exact-version-for version))
  (let ((version-path (-last-item version)))
    (if (fnm--version-installed? (car version))
        (let ((prev-version fnm-current-version)
              (prev-exec-path exec-path))
          (let* ((path-re (concat "^" (f-join fnm-dir "node-versions/") fnm-version-re "/installation/bin/?$"))
                 (new-bin-path (f-full (f-join version-path "installation/bin")))
                 (paths
                  (cons
                   new-bin-path
                   (-reject
                    (lambda (path)
                      (s-matches? path-re path))
                    (parse-colon-path (getenv "PATH"))))))
            (setenv "PATH" (s-join path-separator paths))
            (setq exec-path (cons new-bin-path (--remove (s-matches? path-re it) exec-path))))
          (setq fnm-current-version version)
          (when callback
            (unwind-protect
                (funcall callback)
              (when prev-version (fnm-use (car prev-version)))
              (setq exec-path prev-exec-path))))
      (user-error "No such version %s" version))))

;;;###autoload
(defun fnm-use-for (&optional path callback)
  "Activate Node using fnm for PATH or `default-directory'.
This function will look for a .nvmrc file in that path and
activate the version specified in that file.
If CALLBACK is specified, active in that scope and then reset to
previously used version."
  (unless path
    (setq path default-directory))
  (-if-let (fnmrc-path
            (f-traverse-upwards
             (lambda (dir)
               (f-file? (f-expand ".nvmrc" dir)))
             path))
      (fnm-use (s-trim (f-read (f-expand ".nvmrc" fnmrc-path))) callback)
    (error "No .nvmrc found for %s" path)))

;;;###autoload
(defun fnm-use-for-buffer ()
  "Activate Node based on an .nvmrc using fnm for the current file.
If buffer is not visiting a file, do nothing."
  (when buffer-file-name
    (condition-case err
        (fnm-use-for buffer-file-name)
      (error (message "%s" err)))))

(provide 'fnm)

;;; fnm.el ends here
