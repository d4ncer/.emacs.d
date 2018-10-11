;;; rk-mode-line-format.el --- Functions for constructing the mode line format string.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'subr-x)
(require 'rk-theme-base)

(autoload 'all-the-icons-icon-for-mode "all-the-icons")
(autoload 'all-the-icons-icon-for-file "all-the-icons")
(autoload 'all-the-icons-octicon "all-the-icons")
(autoload 'all-the-icons-fileicon "all-the-icons")
(autoload 'magit-get-current-branch "magit-git")
(autoload 'projectile-project-p "projectile")

(defconst rk-mode-line-format--derived-modes
  '(rk-web-js-mode rk-web-css-mode rk-web-html-mode rk-web-json-mode gfm-mode)
  "Derived modes.")

;;; Faces

(defgroup rk-mode-line-format nil
  "Utilities for constructing the header line."
  :group 'themes
  :prefix "rk-mode-line-format-")

(defface rk-mode-line-format-nonemphased-element
  '((t
     (:inherit mode-line-text)))
  "Face for non-emphasised elements in the header line."
  :group 'rk-mode-line-format)

(defface rk-mode-line-format-accent-element
  `((t
     (:inhert mode-line :foreground ,rk-theme-base-yellow)))
  "Face for accented elements in the header line."
  :group 'rk-mode-line-format)

(defface rk-mode-line-format-project-name
  '((t
     (:inherit mode-line-text)))
  "Face for project name in header line."
  :group 'rk-mode-line-format)

(defface rk-mode-line-format-branch-name
  '((t
     (:inherit mode-line-text)))
  "Face for git branch in header line."
  :group 'rk-mode-line-format)

(defface rk-mode-line-format-host-name
  '((t
     (:inherit mode-line-text)))
  "Face for host-name in header line."
  :group 'rk-mode-line-format)

;;; Helper for testing if window selected.

(defvar rk-mode-line-format--window-for-redisplay nil
  "The window currently being redisplayed.")

(defun rk-mode-line-format--set-window-for-redisplay (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq rk-mode-line-format--window-for-redisplay (selected-window))))

(add-function :before pre-redisplay-function #'rk-mode-line-format--set-window-for-redisplay)

(defun rk-mode-line-format--window-selected? ()
  (eq rk-mode-line-format--window-for-redisplay (get-buffer-window)))

;;; Cache variable lookups to improve speed

(defconst rk-mode-line-format--cache-duration-seconds 10)

(defun rk-mode-line-format--make-cache-key ()
  (cons (current-time) default-directory))

(defun rk-mode-line-format--cache-expired? (key)
  (-let* (((time . key-directory) key)
          (expiry-time (time-add time rk-mode-line-format--cache-duration-seconds)))

    (or (time-less-p expiry-time (current-time))
        (not (equal default-directory key-directory)))))

;; Cache the git branch.

(defvar-local rk-mode-line-format--branch nil
  "A cons of (cache-key . branch-name) or nil")

(defun rk-mode-line-format--update-branch ()
  (let ((key (rk-mode-line-format--make-cache-key))
        (branch (magit-get-current-branch)))
    (setq rk-mode-line-format--branch (cons key branch))
    branch))

(defun rk-mode-line-format--current-branch ()
  (require 'magit)
  (-if-let ((key . branch) rk-mode-line-format--branch)
      (cond
       ((rk-mode-line-format--cache-expired? key)
        (rk-mode-line-format--update-branch))
       (t
        branch))
    (rk-mode-line-format--update-branch)))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defvar-local rk-mode-line-format--project nil
  "A cons of (cache-key . project-name) or nil")

(defun rk-mode-line-format--update-project ()
  (let ((key (rk-mode-line-format--make-cache-key))
        (project (projectile-project-p)))
    (setq rk-mode-line-format--project (cons key project))
    project))

(defun rk-mode-line-format--current-project ()
  (-if-let ((key . project) rk-mode-line-format--project)
      (cond
       ((rk-mode-line-format--cache-expired? key)
        (rk-mode-line-format--update-project))
       (t
        project))
    (rk-mode-line-format--update-project)))

;;; Helper functions

(defun rk-mode-line-format--shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;;; Construction functions

(defun rk-mode-line-format--nonemphasised (str)
  (propertize str 'face 'rk-mode-line-format-nonemphased-element))

(defun rk-mode-line-format--access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "*" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'rk-mode-line-format-accent-element)))

(defun rk-mode-line-format--project-info ()
  (let* ((project (rk-mode-line-format--current-project))
         (project (when project (directory-file-name project)))
         (project-root-name (when project (file-name-nondirectory project)))
         (branch (when project (rk-mode-line-format--current-branch))))
    (cond
     ((and project branch)
      (concat (rk-mode-line-format--nonemphasised " (")
              (propertize project-root-name 'face 'rk-mode-line-format-project-name)
              (rk-mode-line-format--nonemphasised " on ")
              (all-the-icons-octicon "git-branch" :v-adjust 0.1 :height 0.9)
              " "
              (propertize branch 'face 'rk-mode-line-format-branch-name)
              (rk-mode-line-format--nonemphasised ") ")))
     (project
      (concat (rk-mode-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'rk-mode-line-format-project-name)
              (rk-mode-line-format--nonemphasised ") ")))
     (t
      ""))))

(defun rk-mode-line-format--host-info ()
  (concat
   (rk-mode-line-format--nonemphasised " (at ")
   (propertize (and (boundp 'tramp-current-host) tramp-current-host) 'face 'rk-mode-line-format-host-name)
   (rk-mode-line-format--nonemphasised ") ")))

(defun rk-mode-line-format--context-info ()
  (cond
   ((not (rk-mode-line-format--window-selected?))
    "")
   ((file-remote-p default-directory)
    "")
   (t
    (rk-mode-line-format--project-info))))

(defun rk-mode-line-format--major-mode-icon ()
  (-let* ((mode major-mode)
          (is-derived-mode-p (-contains-p rk-mode-line-format--derived-modes mode))
          (fn (if is-derived-mode-p #'all-the-icons-icon-for-file #'all-the-icons-icon-for-mode))
          (fn-args (if is-derived-mode-p (buffer-name) mode))
          (height 0.8)
          (v-adjust (if is-derived-mode-p 0.05 (if (eq mode 'emacs-lisp-mode) -0.1 0.05))))
    (concat (funcall fn fn-args :height height :v-adjust v-adjust) " ")))

(defun rk-mode-line-format--major-mode-info ()
  (cond
   ((eq major-mode 'turn-on-evil-mode)
    "")
   (t
    (rk-mode-line-format--major-mode-icon))))

(defun rk-mode-line-format--buffer-name ()
  (if (rk-mode-line-format--window-selected?)
      (buffer-name)
    (propertize (buffer-name) 'face 'rk-mode-line-format-nonemphased-element)))

(defun rk-mode-line-format--line-info ()
  (let ((str "%2l"))
    (if (rk-mode-line-format--window-selected?)
        str
      (propertize str 'face 'rk-mode-line-format-nonemphased-element))))

(defconst rk-mode-line-format
  '(
    ;; Print error on low memory
    "%e"
    " "

    ;; Emacsclient info
    mode-line-client

    ;; Major mode icon
    (:eval (rk-mode-line-format--major-mode-info))

    ;; Current line, padded
    (:eval (rk-mode-line-format--line-info))
    "  "
    (:propertize "%6p " face rk-mode-line-format-nonemphased-element)

    ;; Modification indicator.
    (:eval (rk-mode-line-format--access-mode-info))

    ;; Buffer name, with braces on recursive edit
    "  %[" (:eval (rk-mode-line-format--buffer-name)) "%] "

    (:eval (rk-mode-line-format--context-info))))

(provide 'rk-mode-line-format)

;;; rk-mode-line-format.el ends here
