;;; rk-misc-utils.el --- Random fns that I use -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'paths)

(autoload 'wgrep-finish-edit "wgrep")
(autoload 'wgrep-abort-changes "wgrep")
(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'recentf-cleanup "recentf")
(autoload 'org-insert-link "org")

(defvar rk-delete-current-buffer-and-file-file-deleted-functions nil
  "Hook called after a file is deleted by `rk-delete-current-buffer-and-file'.
Each function is passed the path of the file that was deleted.")

(defun rk-search-wgrep-finish-edit-kill-buffer ()
  "Finish the current wgrep edit and kill the wgrep buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (prog1 (wgrep-finish-edit)
      (kill-buffer buf))))

(defun rk-search-wgrep-abort-changes-kill-buffer ()
  "Abort the current wgrep edit and kill the wgrep buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (prog1 (wgrep-abort-changes)
      (kill-buffer buf))))

(defun rk-insert-iso-timestamp ()
  "Insert current ISO timestamp at position."
  (interactive)
  (insert (format-time-string "%FT%T.%3NZ")))

(defun rk-get-face-at-point  (pos)
  "Get the font face at POS."
  (interactive "d")
  (let ((face (or (get-text-property (point) 'read-face-name)
                  (get-text-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun rk-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun rk-copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun rk-reload-file ()
  "Revisit the current file."
  (interactive)
  (when-let (path (buffer-file-name))
    (find-alternate-file path)))

(defun rk-alternate-buffer (&optional window)
  "Toggle back and forth between two buffers.
WINDOW sets the window in which to toggle, and defaults to the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer)
                            (and (not (eq buffer current-buffer))
                                 (or (null buffer-predicate) (funcall buffer-predicate buffer))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun rk-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let ((path (or (buffer-file-name) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

(defun rk-delete-current-buffer-and-file ()
  "Remove the file associated with the current buffer, then kill it."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((null file)
      (kill-buffer))
     ((not (file-exists-p file))
      (kill-buffer))
     ((yes-or-no-p "Delete this file? ")
      (delete-file file t)
      (kill-buffer)
      (run-hook-with-args rk-delete-current-buffer-and-file-file-deleted-functions file)
      (message "File deleted: %s" file)))))

(defun rk-goto-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))

(defun rk-goto--base-ledger ()
  "Open base ledger file."
  (interactive)
  (find-file rk-accounts--ledger-file))

(defun rk-rename-file-and-buffer--assert-file-exists-for-buffer (&optional buf)
  (let ((cur (buffer-file-name buf)))
    (if (not (and cur (f-exists? cur)))
        (error "Buffer is not visiting a file!")
      cur)))

(defun rk-rename-file-and-buffer--try-move-file-with-vc (src dest)
  (condition-case err
      (when (vc-backend src)
        (vc-rename-file src dest)
        t)
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((s-matches? "New file already exists" msg) nil)
        ((s-matches? "Please update files" msg)
         (unless (y-or-n-p "VC cannot track this change automatically.  Continue? ")
           (error msg)))
        (t
         (error msg)))))))

(defun rk-rename-file-and-buffer--try-rename-file (src dest)
  (when (and (f-exists? dest) (not (y-or-n-p "File exists.  Overwrite? ")))
    (user-error "Aborted"))
  (rename-file src dest t)
  (-when-let (buf (get-file-buffer src))
    (with-current-buffer buf
      (rename-buffer dest)
      (set-visited-file-name dest)
      (set-buffer-modified-p nil))

    (recentf-cleanup)
    (when (projectile-project-p)
      (projectile-invalidate-cache nil))))

(defun rk-rename-file-and-buffer (buffer dest-dir dest-filename)
  "Rename the current buffer and file it is visiting.
Performs basic VC cleanup.
BUFFER is the buffer to rename.
DEST-DIR is the directory to move the underlying file to.
DEST-FILENAME is the new filename for the underlying file."
  (interactive (let ((cur (rk-rename-file-and-buffer--assert-file-exists-for-buffer)))
                 (list (current-buffer)
                       (read-directory-name "Move to directory: " (f-dirname cur))
                       (read-string "New name: " (f-filename cur)))))
  (let ((src (rk-rename-file-and-buffer--assert-file-exists-for-buffer buffer))
        (dest-path (f-join dest-dir dest-filename)))
    (or (rk-rename-file-and-buffer--try-move-file-with-vc src dest-path)
        (rk-rename-file-and-buffer--try-rename-file src dest-path))
    (when (and (fboundp 'projectile-project-p) (projectile-project-p))
      (call-interactively #'projectile-invalidate-cache))
    (message "File '%s' moved to '%s'" (f-short (f-filename src)) (f-short dest-path))))

(defun rk-rename-file-and-buffer-mv (buffer to-dir)
  "Move BUFFER's corresponding file to TO-DIR."
  (interactive (list (current-buffer) (read-directory-name "Move to: ")))
  (let ((current-file-name (f-filename (rk-rename-file-and-buffer--assert-file-exists-for-buffer buffer))))
    (rk-rename-file-and-buffer buffer to-dir current-file-name)))

(defun rk-sudo-edit (&optional arg)
  "Reopen the current file as sudo for editing.
With prefix argument ARG, prompt for a file."
  (interactive "p")
  (let* ((fname (if (or arg (not buffer-file-name))
                    (read-file-name "File: ")
                  buffer-file-name))
         (target (cond ((string-match-p "^/ssh:" fname)
                        (with-temp-buffer
                          (insert fname)
                          (search-backward ":")
                          (let ((last-match-end nil)
                                (last-ssh-hostname nil))
                            (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                              (setq last-ssh-hostname (or (match-string 1 fname)
                                                          last-ssh-hostname))
                              (setq last-match-end (match-end 0)))
                            (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
                          (buffer-string)))
                       (t (concat "/sudo:root@localhost:" fname)))))
    (find-file target)))

(defun rk-toggle-window-split ()
  "Toggle between vertical and horizontal split."
  (interactive)
  (cond
   ((one-window-p)
    (user-error "Only one window"))
   ((> (count-windows) 2)
    (user-error "Too many windows to toggle split"))
   (t
    (let* ((b1 (window-buffer))
           (b2 (window-buffer (next-window)))
           (w1-edges (window-edges (selected-window)))
           (w2-edges (window-edges (next-window)))
           (w2 (not (and (<= (car w1-edges) (car w2-edges))
                         (<= (cadr w1-edges) (cadr w2-edges)))))
           (split-fn
            (if (= (car w1-edges)
                   (car (window-edges (next-window))))
                #'split-window-horizontally
              #'split-window-vertically)))

      (delete-other-windows)
      (let ((w1 (selected-window)))
        (funcall split-fn)
        (when w2 (other-window 1))
        (set-window-buffer (selected-window) b1)
        (set-window-buffer (next-window) b2)
        (select-window w1)
        (when w2 (other-window 1)))))))

(defun rk-elisp-eval-buffer ()
  "Evaluate the current buffer as Elisp code, within a straight transaction."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (if (null buffer-file-name)
      (eval-buffer)
    (load buffer-file-name nil 'nomessage))
  (message "Evaluating %s... done." (buffer-name)))

;; Generate random passwords.
(defun rk-generate-password ()
  "Generate a random password and copy it to the kill ring."
  (interactive)
  (kill-new (s-trim (shell-command-to-string "gpg --gen-random --armor 1 30")))
  (message "Password copied to kill-ring."))

;; Work stuff
(defconst rkmooven--jira-url-template
  "https://mooven.atlassian.net/browse/%s")

(defun rkmooven-insert-JIRA-link (ticket)
  "Generate a link to a MPB JIRA TICKET."
  (interactive (let ((ticket (read-string "Ticket number: ")))
                 (list ticket)))
  (org-insert-link nil (format rkmooven--jira-url-template ticket) (format "MPB-%s" ticket)))


(provide 'rk-misc-utils)

;;; rk-misc-utils.el ends here
