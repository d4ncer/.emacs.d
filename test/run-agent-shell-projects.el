;;; run-agent-shell-projects.el --- Project verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-projects.el from this repo.

;;; Code:

(setq load-prefer-newer t)

(require 'bytecomp)
(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (add-to-list 'load-path (expand-file-name "lisp/agent-shell-projects/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                     t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-projects-test.el" root) nil t)
  (load (expand-file-name "test/agent-shell-projects-async-test.el" root) nil t)
  (load (expand-file-name "test/agent-shell-projects-workflow-test.el" root) nil t)
  (load (expand-file-name "test/agent-shell-projects-onboarding-test.el" root) nil t)
  (load (expand-file-name "test/agent-shell-projects-manage-test.el" root) nil t)
  (let* ((temporary (make-temp-file "projects-bytecode-" t))
         (byte-compile-error-on-warn t)
         (byte-compile-dest-file-function
          (lambda (file) (expand-file-name (concat (file-name-base file) ".elc")
                                           temporary)))
         (stats (ert-run-tests-batch "^+agent-shell-projects-test-")))
    (unwind-protect
        (progn
          (dolist (file (directory-files (expand-file-name "lisp/agent-shell-projects/" root)
                                         t "^+agent-shell-projects.*\\.el$"))
            (with-temp-buffer
              (insert-file-contents file) (emacs-lisp-mode) (check-parens))
            (unless (byte-compile-file file)
              (error "Compilation failed: %s" file))))
      (delete-directory temporary t))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-projects)
;;; run-agent-shell-projects.el ends here
