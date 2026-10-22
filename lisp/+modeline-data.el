;;; +modeline-data.el --- Event driven mode line data -*- lexical-binding: t; -*-

;;; Commentary:
;; Refresh filesystem and note data outside mode line evaluation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(declare-function dired-get-filename "dired")
(declare-function vulpea-note-title "vulpea-note")
(declare-function vulpea-note-tags "vulpea-note")
(declare-function vulpea-note-id "vulpea-note")
(declare-function vulpea-db-query-by-links-some "vulpea-db")
(defvar vulpea-ui--current-note)
(defvar +modeline-data-mode)
(defvar-local +modeline-dired--directory "")
(defvar-local +modeline-dired--count "0 items")
(defvar-local +modeline-dired--space nil)
(defvar-local +modeline-data--timer nil)
(defvar-local +modeline-vulpea-data nil)

(defun +modeline-data--cancel ()
  "Cancel the current buffer's pending update."
  (when (timerp +modeline-data--timer) (cancel-timer +modeline-data--timer))
  (setq +modeline-data--timer nil))

(defun +modeline-data--schedule (function)
  "Debounce FUNCTION in the current buffer."
  (+modeline-data--cancel)
  (let ((buffer (current-buffer)))
    (setq +modeline-data--timer
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq +modeline-data--timer nil)
                 (when +modeline-data-mode (funcall function))))))))
  (add-hook 'kill-buffer-hook #'+modeline-data--cancel nil t))

(defun +modeline-dired--update-space ()
  "Cache local free space; remote listings omit this field."
  (setq +modeline-dired--space
        (unless (file-remote-p default-directory)
          (when-let* ((info (ignore-errors (file-system-info default-directory)))
                      (free (nth 2 info)))
            (format "%.1f GB free" (/ free 1073741824.0)))))
  (force-mode-line-update))

(defun +modeline-dired--changed (&rest _)
  "Refresh counts after Dired inserts or removes listing entries."
  (+modeline-data--schedule #'+modeline-dired-refresh))

(defun +modeline-dired-refresh ()
  "Cache displayed directory entries after a listing changes."
  (when (derived-mode-p 'dired-mode)
    (add-hook 'after-change-functions #'+modeline-dired--changed 90 t)
    (setq +modeline-dired--directory (abbreviate-file-name default-directory)
          +modeline-dired--space nil)
    (let ((count 0))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (when-let* ((file (dired-get-filename 'local t)))
              (unless (member (file-name-nondirectory (directory-file-name file))
                              '("." ".."))
                (cl-incf count)))
            (forward-line 1))))
      (setq +modeline-dired--count (format "%d items" count)))
    (+modeline-data--schedule #'+modeline-dired--update-space)))

(defun +modeline-dired-directory () "Return cached directory." +modeline-dired--directory)
(defun +modeline-dired-file-count () "Return cached entry count." +modeline-dired--count)
(defun +modeline-dired-free-space () "Return cached free space." +modeline-dired--space)

(defun +modeline-vulpea-refresh ()
  "Cache the current sidebar note's title, tags and backlink count."
  (when (derived-mode-p 'vulpea-ui-sidebar-mode)
    (setq +modeline-vulpea-data
          (when-let* ((note (bound-and-true-p vulpea-ui--current-note)))
            (list :title (or (vulpea-note-title note) "No note")
                  :tags (vulpea-note-tags note)
                  :backlinks
                  (length (vulpea-db-query-by-links-some
                           (list (cons "id" (vulpea-note-id note))))))))
    (force-mode-line-update)))

(defun +modeline-vulpea--note-changed (_symbol _value operation where)
  "Schedule updated sidebar data after a note assignment in WHERE."
  (when (and +modeline-data-mode (eq operation 'set) (bufferp where)
             (buffer-live-p where))
    (with-current-buffer where
      (when (derived-mode-p 'vulpea-ui-sidebar-mode)
        (+modeline-data--schedule #'+modeline-vulpea-refresh)))))

(defun +modeline-vulpea--db-changed (&rest _)
  "Coalesce database changes into updates of existing sidebar buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'vulpea-ui-sidebar-mode)
        (+modeline-data--schedule #'+modeline-vulpea-refresh)))))

(define-minor-mode +modeline-data-mode
  "Maintain cached data used by custom mode line segments."
  :global t :group 'mode-line
  (dolist (hook '(dired-mode-hook dired-after-readin-hook wdired-finish-edit-hook))
    (if +modeline-data-mode
        (add-hook hook #'+modeline-dired-refresh)
      (remove-hook hook #'+modeline-dired-refresh)))
  (if +modeline-data-mode
      (progn
        (add-variable-watcher 'vulpea-ui--current-note
                              #'+modeline-vulpea--note-changed)
        (add-hook 'vulpea-db-updated-functions #'+modeline-vulpea--db-changed))
    (remove-variable-watcher 'vulpea-ui--current-note
                             #'+modeline-vulpea--note-changed)
    (remove-hook 'vulpea-db-updated-functions #'+modeline-vulpea--db-changed))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if +modeline-data-mode
          (cond ((derived-mode-p 'dired-mode) (+modeline-dired-refresh))
                ((derived-mode-p 'vulpea-ui-sidebar-mode)
                 (+modeline-data--schedule #'+modeline-vulpea-refresh)))
        (+modeline-data--cancel)
        (remove-hook 'kill-buffer-hook #'+modeline-data--cancel t)
        (remove-hook 'after-change-functions #'+modeline-dired--changed t)))))

(provide '+modeline-data)
;;; +modeline-data.el ends here
