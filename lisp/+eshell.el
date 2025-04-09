;;; +eshell.el --- Extra commands for eshell -*- lexical-binding: t; -*-

;;; Commentary:

;; Defines extra commands that should be available in Eshell sessions. I use
;; these to more closely align eshell with my regular shell.

;;; Code:

(autoload 'project-root "project")

(defun eshell/j (&rest query)
  "Jump to a directory with fasd QUERY."
  (let* ((command `("fasd" "-ld" ,@(mapcar #'shell-quote-argument query)))
         (output (shell-command-to-string (string-join command " ")))
         (matches (nreverse (split-string output "\n" t))))
    (if-let* ((dir (car matches)))
        (eshell/cd dir)
      (let ((message-log-max))
        (message "No fasd match")))))

(defun eshell/g ()
  "Navigate to the Git root."
  (let (message-log-max)
    (if-let* ((dir (locate-dominating-file default-directory ".git")))
        (progn
          (message "Moving to git repository root")
          (eshell/cd dir))
      (if-let* ((dir (project-root)))
          (progn
            (message "Moving to project root")
            (eshell/cd dir))
        (user-error "Not in a project or git repo")))))

(provide '+eshell)

;;; +eshell.el ends here
