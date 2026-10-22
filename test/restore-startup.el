;;; restore-startup.el --- Restored init verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Simulate normal startup in the disposable directory used by restore-config.

;;; Code:

(defvar +private-config-directory nil)
(setq use-package-expand-minimally t)
(advice-add 'exec-path-from-shell-initialize :override #'ignore)
(advice-add 'keychain-refresh-environment :override #'ignore)
(with-eval-after-load 'elpaca
  (advice-add 'elpaca--fail :before
              (lambda (e reason &rest _)
                (message "Restore package failure: %S: %s\n%S"
                         (elpaca<-id e) reason
                         (elpaca-event-log (elpaca<-id e))))))
(let* ((directory (or (getenv "CONFIG_RESTORE_DIRECTORY")
                      (error "CONFIG_RESTORE_DIRECTORY is required")))
       (after-init-time nil)
       (deadline (+ (float-time) 600))
       (monitor
        (run-with-timer
         2 5
         (lambda ()
           (when (featurep 'elpaca)
             (with-temp-file (expand-file-name "restore-progress.eld" directory)
               (pp (mapcar (lambda (entry)
                             (let ((e (cdr entry)))
                               (list (car entry) (elpaca<-status e)
                                     (elpaca<-current-step e) (elpaca<-conditions e))))
                           (elpaca--queued))
                   (current-buffer))))
           (when (> (float-time) deadline) (throw '+restore-timeout t))))))
  (unwind-protect
      (progn
        (when (catch '+restore-timeout
                (setq user-emacs-directory (file-name-as-directory directory))
                (load (expand-file-name "early-init.el" directory) nil t)
                (load (expand-file-name "init.el" directory) nil t)
                (setq after-init-time (current-time))
                (run-hooks 'after-init-hook)
                (elpaca-wait)
                nil)
          (error "Restore timed out; progress: %s"
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "restore-progress.eld" directory))
                   (buffer-string))))
        (dolist (feature '(org org-agenda vulpea))
          (when (featurep feature) (error "Restored startup eagerly loaded %s" feature)))
        (dolist (entry (elpaca--queued))
          (when (eq (elpaca<-status (cdr entry)) 'failed)
            (error "Restored package failed: %s" (car entry))))
        (message "Restored init and package builds: PASS"))
    (cancel-timer monitor)))

(provide 'restore-startup)
;;; restore-startup.el ends here
