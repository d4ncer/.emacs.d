;;; +grep-edit.el --- Save files changed through native Grep Edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Retain automatic saving when finishing native editable search results.

;;; Code:

(require 'cl-lib)
(require 'grep)
(defvar-local +grep-edit--sources nil)
(declare-function evil-insert-state "evil")
(declare-function evil-normal-state "evil")

(defun +grep-edit--remember-target (beg _end _length)
  "Remember the source buffer edited at BEG."
  (when (derived-mode-p 'grep-edit-mode)
    (let* ((targets (get-text-property
                     (save-excursion (goto-char beg) (line-beginning-position))
                     'occur-target))
           (marker (if (markerp targets) targets (car-safe (car-safe targets)))))
      (when (and (markerp marker) (marker-buffer marker))
        (cl-pushnew (marker-buffer marker) +grep-edit--sources)))))

(defun +grep-edit--start ()
  "Track edits after the native source update handler."
  (setq +grep-edit--sources nil)
  (add-hook 'after-change-functions #'+grep-edit--remember-target 90 t)
  (when (featurep 'evil) (evil-insert-state)))

(defun +grep-edit--save ()
  "Save only source buffers changed in this Grep editing session."
  (when (derived-mode-p 'grep-edit-mode)
    (dolist (buffer +grep-edit--sources)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))))))

(defun +grep-edit--finish (&rest _)
  "Remove edit tracking after native Grep Edit finishes."
  (remove-hook 'after-change-functions #'+grep-edit--remember-target t)
  (setq +grep-edit--sources nil)
  (when (featurep 'evil) (evil-normal-state)))

(define-minor-mode +grep-edit-mode
  "Automatically save edited search results when finishing Grep Edit."
  :global t :group 'grep
  (if +grep-edit-mode
      (progn
        (add-hook 'grep-edit-mode-hook #'+grep-edit--start)
        (advice-add 'grep-edit-save-changes :before #'+grep-edit--save)
        (advice-add 'grep-edit-save-changes :after #'+grep-edit--finish))
    (remove-hook 'grep-edit-mode-hook #'+grep-edit--start)
    (advice-remove 'grep-edit-save-changes #'+grep-edit--save)
    (advice-remove 'grep-edit-save-changes #'+grep-edit--finish)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (remove-hook 'after-change-functions #'+grep-edit--remember-target t)
        (setq +grep-edit--sources nil)))))

(provide '+grep-edit)
;;; +grep-edit.el ends here
