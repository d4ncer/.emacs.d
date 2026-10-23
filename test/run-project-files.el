;;; run-project-files.el --- File finder verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l test/run-project-files.el

;;; Code:

(setq load-prefer-newer t)

(require 'bytecomp)
(let* ((root (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (temporary (make-temp-file "project-files-bytecode-" t))
       (stats nil)
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (file)
          (expand-file-name (concat (file-name-base file) ".elc") temporary))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (add-to-list 'load-path (expand-file-name "lisp/project-files/" root))
  (dolist (package '("projectile" "compat" "consult" "orderless"))
    (add-to-list 'load-path (expand-file-name
                            (concat "elpaca/builds/" package) root)))
  (unwind-protect
      (progn
        (dolist (file '("lisp/project-files/+project-files.el" "lisp/project-files/+project-files-worker.el"
                        "test/project-files-test.el" "test/project-files-search-test.el"
                        "modules/mod-project.el"
                        "modules/mod-keybindings.el"))
          (with-temp-buffer
            (insert-file-contents (expand-file-name file root))
            (emacs-lisp-mode)
            (check-parens)))
        (dolist (file '("lisp/project-files/+project-files.el" "lisp/project-files/+project-files-worker.el"))
          (unless (byte-compile-file (expand-file-name file root))
            (error "Compilation failed: %s" file)))
        (load (expand-file-name "test/project-files-test.el" root) nil t)
        (load (expand-file-name "test/project-files-search-test.el" root) nil t)
        (setq stats (ert-run-tests-batch "^+project-files-test-")))
    (delete-directory temporary t))
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

(provide 'run-project-files)
;;; run-project-files.el ends here
