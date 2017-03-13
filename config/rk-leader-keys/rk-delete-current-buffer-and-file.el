;;; rk-delete-current-buffer-and-file.el --- Command to delete the current buffer and file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defvar rk-delete-current-buffer-and-file-file-deleted-functions nil
  "Hook called after a file is deleted by `rk-delete-current-buffer-and-file'.
Each function is passed the path of the file that was deleted.")

;;;###autoload
(defun rk/delete-current-buffer-and-file ()
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


(provide 'rk-delete-current-buffer-and-file)

;;; rk-delete-current-buffer-and-file.el ends here
