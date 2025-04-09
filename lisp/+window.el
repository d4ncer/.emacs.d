;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun +sibling-file-or-other-buffer ()
  (let ((sibling (when-let* ((file (buffer-file-name)))
                   (save-excursion
                     (ignore-errors
                       (find-sibling-file file))))))
    (cond
     ;; Don't show sibling again if it's already visible
     ((get-buffer-window sibling)
      (other-buffer))
     (sibling
      sibling)
     (t
      ;; Show some buffer that's not already visible.
      (or (seq-find (lambda (it)
                      (null (get-buffer-window it)))
                    (buffer-list))
          (other-buffer))))))

(defun +split-window-horizontally-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (+sibling-file-or-other-buffer))
    (unless arg
      (select-window target-window)))

  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (balance-windows (window-parent)))

(defun +split-window-vertically-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (+sibling-file-or-other-buffer))
    (unless arg
      (select-window target-window)))

  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (balance-windows (window-parent)))

(defun +toggle-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(defun +delete-nondedicated-windows ()
  "Delete windows that are not dedicated."
  (interactive)
  (let ((windows (window-list))
        (selected (selected-window)))
    (dolist (win windows)
      (unless (or (equal win selected)
                  (window-dedicated-p win))
        (delete-window win)))))

(defun +clone-indirect-buffer-of-region (beg end &optional display)
  (interactive (list (region-beginning) (region-end) t))
  (deactivate-mark)
  (let ((old-buf (current-buffer))
        (new-buf))
    (narrow-to-region beg end)
    (setq new-buf (clone-indirect-buffer nil display))
    (with-current-buffer old-buf
      (widen))
    new-buf))

(provide '+window)
