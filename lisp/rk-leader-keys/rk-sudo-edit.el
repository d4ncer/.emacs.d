;;; rk-sudo-edit.el --- Command for editing the current file as sudo.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defun rk/sudo-edit (&optional arg)
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

(provide 'rk-sudo-edit)

;;; rk-sudo-edit.el ends here
