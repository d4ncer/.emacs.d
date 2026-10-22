;;; +agent-shell-projects-lifecycle.el --- Package activation -*- lexical-binding: t; -*-

;;; Commentary:
;; Own global registrations and scheduled work. Loading libraries only registers
;; their integrations; setup installs them, and teardown removes them.

;;; Code:

(require 'cl-lib)
(defvar +agent-shell-projects-mode nil)
(defvar +agent-shell-projects--enabled nil)
(defvar +agent-shell-projects--hooks nil)
(defvar +agent-shell-projects--timers (make-hash-table :test #'eq))
(defvar +agent-shell-projects--embark-maps nil)
(defvar +agent-shell-projects--embark-previous nil)
(defvar embark-keymap-alist)
(defvar +agent-shell-projects--ui-timer)
(defvar +agent-shell-projects--ui-pending)
(defvar +agent-shell-projects--code-source-p)
(defvar +agent-shell-projects--source-project)
(defvar +agent-shell-projects--project-id)
(defvar +agent-shell-projects--artefact-id)
(declare-function +agent-shell-projects--start-agent "+agent-shell-projects-agent")
(declare-function +agent-shell-projects--stop-agent "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-stop-workers "+agent-shell-projects-jobs")
(declare-function +agent-shell-projects--source-saved "+agent-shell-projects-core")
(declare-function +agent-shell-projects--code-source-saved "+agent-shell-projects-code")
(declare-function +agent-shell-projects-embark-target "+agent-shell-projects-annotations")

(defun +agent-shell-projects-register-hook (hook function &optional depth)
  "Register FUNCTION on HOOK at DEPTH while the package is enabled."
  (cl-pushnew (list hook function depth) +agent-shell-projects--hooks :test #'equal)
  (when +agent-shell-projects--enabled (add-hook hook function depth)))

(defun +agent-shell-projects-run-at-time (time repeat function &rest args)
  "Schedule owned FUNCTION with ARGS at TIME, optionally with REPEAT."
  (let (timer)
    (setq timer
          (run-at-time time repeat
                       (lambda ()
                         (unless repeat (remhash timer +agent-shell-projects--timers))
                         (apply function args))))
    (puthash timer t +agent-shell-projects--timers)
    timer))

(defun +agent-shell-projects-cancel-timer (timer)
  "Cancel owned TIMER and release its registry entry."
  (cancel-timer timer)
  (remhash timer +agent-shell-projects--timers))

(defun +agent-shell-projects--install-embark ()
  "Install contextual maps when both packages are enabled."
  (when (and +agent-shell-projects--enabled (featurep 'embark)
             +agent-shell-projects--embark-maps)
    (add-hook 'embark-target-finders #'+agent-shell-projects-embark-target)
    (dolist (entry +agent-shell-projects--embark-maps)
      (unless (assq (car entry) +agent-shell-projects--embark-previous)
        (push (cons (car entry) (copy-tree (assq (car entry) embark-keymap-alist)))
              +agent-shell-projects--embark-previous))
      (setf (alist-get (car entry) embark-keymap-alist) (list (cdr entry))))))

(with-eval-after-load 'embark (+agent-shell-projects--install-embark))

(defun +agent-shell-projects-setup ()
  "Install the package's registered integrations idempotently."
  (setq +agent-shell-projects--enabled t)
  (dolist (entry +agent-shell-projects--hooks)
    (add-hook (nth 0 entry) (nth 1 entry) (nth 2 entry)))
  (+agent-shell-projects--install-embark)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p +agent-shell-projects--source-project)
        (add-hook 'after-save-hook #'+agent-shell-projects--source-saved nil t))
      (when (and (featurep '+agent-shell-projects-code)
                 (bound-and-true-p +agent-shell-projects--project-id)
                 (bound-and-true-p +agent-shell-projects--artefact-id)
                 (bound-and-true-p +agent-shell-projects--code-source-p))
        (add-hook 'after-save-hook #'+agent-shell-projects--code-source-saved nil t)))))

(defun +agent-shell-projects-teardown ()
  "Remove integrations and cancel owned asynchronous work."
  (setq +agent-shell-projects--enabled nil
        +agent-shell-projects-mode nil)
  (dolist (entry +agent-shell-projects--hooks)
    (remove-hook (nth 0 entry) (nth 1 entry)))
  (when (fboundp '+agent-shell-projects--stop-agent)
    (+agent-shell-projects--stop-agent))
  (when (fboundp '+agent-shell-projects-stop-workers)
    (+agent-shell-projects-stop-workers))
  (maphash (lambda (timer _) (cancel-timer timer)) +agent-shell-projects--timers)
  (clrhash +agent-shell-projects--timers)
  (when (boundp '+agent-shell-projects--ui-timer)
    (setq +agent-shell-projects--ui-timer nil))
  (when (boundp '+agent-shell-projects--ui-pending)
    (clrhash +agent-shell-projects--ui-pending))
  (when (featurep 'embark)
    (remove-hook 'embark-target-finders #'+agent-shell-projects-embark-target)
    (dolist (entry +agent-shell-projects--embark-previous)
      (when (equal (alist-get (car entry) embark-keymap-alist)
                   (list (cdr (assq (car entry) +agent-shell-projects--embark-maps))))
        (setq embark-keymap-alist (assq-delete-all (car entry) embark-keymap-alist))
        (when (cdr entry) (push (cdr entry) embark-keymap-alist)))))
  (setq +agent-shell-projects--embark-previous nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (remove-hook 'after-save-hook #'+agent-shell-projects--source-saved t)
      (remove-hook 'after-save-hook #'+agent-shell-projects--code-source-saved t))))

(define-minor-mode +agent-shell-projects-mode
  "Maintain project integrations, agent subscriptions and the inbox."
  :global t :group '+agent-shell-projects
  (if +agent-shell-projects-mode
      (progn
        (+agent-shell-projects-setup)
        (require '+agent-shell-projects-agent)
        (+agent-shell-projects--start-agent))
    (+agent-shell-projects-teardown)))

(provide '+agent-shell-projects-lifecycle)
;;; +agent-shell-projects-lifecycle.el ends here
