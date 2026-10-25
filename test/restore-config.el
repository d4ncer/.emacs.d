;;; restore-config.el --- Fresh install check -*- lexical-binding: t; -*-

;;; Commentary:
;; Install this configuration into a disposable directory from the latest
;; upstream package sources, then run the gates against it.  Needs network.
;; Run: emacs -Q --batch -L test -l test/restore-config.el

;;; Code:

(require 'config-test-support)
(let ((temporary (make-temp-file "config-restore-" t)))
  (unwind-protect
      (progn
        (dolist (name '("early-init.el" "init.el" "elpaca-bootstrap.el"
                         "emacs-build.eld" "treesit-grammars.eld" "lisp" "modules"
                         "templates" "file-templates" "ligatures" "test"))
          (let ((source (expand-file-name name +config-test-root))
                (target (expand-file-name name temporary)))
            (if (file-directory-p source)
                (copy-directory source target nil t t)
              (copy-file source target))))
        (dolist (file (directory-files-recursively
                       (expand-file-name "lisp" temporary) "\\.elc\\'"))
          (delete-file file))
        (let ((process-environment (copy-sequence process-environment)))
          (setenv "CONFIG_RESTORE_DIRECTORY" temporary)
          (with-temp-buffer
            (let ((status (call-process
                           (expand-file-name invocation-name invocation-directory) nil t nil
                           "-Q" "--batch" "-l"
                           (expand-file-name "test/restore-startup.el" temporary))))
              (princ (buffer-string))
              (unless (zerop status) (error "Restored init failed (%s)" status)))))
        (let ((default-directory temporary))
          (with-temp-buffer
            (let ((status (call-process
                           (expand-file-name invocation-name invocation-directory) nil t nil
                           "-Q" "--batch" "-l" "test/run-config.el")))
              (princ (buffer-string))
              (unless (zerop status) (error "Restored verification failed (%s)" status)))))
        (when (eq system-type 'darwin)
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "CONFIG_RESTORE_DIRECTORY" temporary)
            (with-temp-buffer
              (let ((status (call-process
                             (expand-file-name invocation-name invocation-directory) nil t nil
                             "-Q" "-l"
                             (expand-file-name "test/restore-gui.el" temporary))))
                (princ (buffer-string))
                (unless (zerop status) (error "Restored GUI failed (%s)" status))))))
        (princ "Fresh install, package builds and verification: PASS\n"))
    (delete-directory temporary t)))

(provide 'restore-config)
;;; restore-config.el ends here
