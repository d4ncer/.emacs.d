;;; restore-gui.el --- GUI activation smoke check -*- lexical-binding: t; -*-

;;; Commentary:
;; Run only in the disposable restored configuration, never a running server.

;;; Code:

(condition-case err
    (progn
      (load (expand-file-name "restore-startup.el" (file-name-directory load-file-name)) nil t)
      (unless (display-graphic-p) (error "A graphical Emacs frame is required"))
      (run-hooks 'pre-command-hook)
      (unless (and vertico-mode global-corfu-mode)
        (error "GUI completion did not activate"))
      (let ((unread-command-events (list ?a ?l ?p ?h ?a ?\r)))
        (unless (equal (completing-read "Smoke completion: " '("alpha" "beta") nil t)
                       "alpha")
          (error "First graphical completion failed")))
      (let* ((directory (make-temp-file "config-gui-fixture-" t))
             (source (expand-file-name "source.el" directory))
             (note (expand-file-name "note.org" directory)))
        (unwind-protect
            (progn
              (with-temp-file source (insert ";;; -*- lexical-binding: t; -*-\n(message \"ok\")\n"))
              (find-file source)
              (unless (and electric-pair-mode winner-mode)
                (error "GUI editor services did not activate"))
              (when (featurep 'org) (error "GUI source file eagerly loaded Org"))
              (kill-buffer)
              (with-temp-file note (insert "* Fixture\n"))
              (find-file note)
              (unless (derived-mode-p 'org-mode) (error "GUI Org activation failed"))
              (kill-buffer)
              (dired directory)
              (sit-for 0.5)
              (unless (equal (+modeline-dired-file-count) "2 items")
                (error "GUI Dired cache is incorrect"))
              (unless (stringp (format-mode-line mode-line-format))
                (error "GUI mode line did not render"))
              (kill-buffer))
          (delete-directory directory t)))
      (message "Graphical completion, editor activation and mode lines: PASS")
      (kill-emacs 0))
  ((error quit)
   (message "GUI smoke failure: %S" err)
   (kill-emacs 1)))

(provide 'restore-gui)
;;; restore-gui.el ends here
