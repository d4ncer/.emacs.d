;;; restore-config.el --- Offline locked restore check -*- lexical-binding: t; -*-

;;; Commentary:
;; Rebuild a disposable configuration from local Git objects at locked commits.
;; Run: emacs -Q --batch -L test -l test/restore-config.el
;; Local repositories must already contain every recorded package revision.

;;; Code:

(require 'config-test-support)
(require 'elpaca)
(require 'elpaca-git)
(let* ((temporary (make-temp-file "config-restore-" t))
       (sources (expand-file-name "mirrors/" temporary))
       (lock (with-temp-buffer
               (insert-file-contents (expand-file-name "elpaca-lock.eld" +config-test-root))
               (read (current-buffer))))
       (seen (make-hash-table :test #'equal))
       (rewrites (make-hash-table :test #'equal))
       (process-environment (copy-sequence process-environment)))
  (unwind-protect
      (progn
        (make-directory sources t)
        (dolist (name '("early-init.el" "init.el" "elpaca-bootstrap.el"
                         "elpaca-lock.eld" "emacs-build.eld" "treesit-grammars.eld" "lisp" "modules"
                         "templates" "file-templates" "ligatures" "test"))
          (let ((source (expand-file-name name +config-test-root))
                (target (expand-file-name name temporary)))
            (if (file-directory-p source)
                (copy-directory source target nil t t)
              (copy-file source target))))
        (dolist (file (directory-files-recursively
                       (expand-file-name "lisp" temporary) "\\.elc\\'"))
          (delete-file file))
        (dolist (entry lock)
          (let* ((recipe (plist-get (cdr entry) :recipe))
                 (original (elpaca-git-repo-dir recipe))
                 (target (expand-file-name (file-name-nondirectory
                                            (directory-file-name original)) sources))
                 (revision (plist-get recipe :ref)))
            (unless (gethash target seen)
              (with-temp-buffer
                (unless (zerop (call-process "git" nil t nil "clone" "--shared" "--quiet"
                                            "--no-checkout" original target))
                  (error "Clone failed: %s" (buffer-string)))
                (unless (zerop (call-process "git" nil t nil "-C" target "checkout"
                                            "--quiet" "--detach" revision))
                  (error "Checkout failed: %s" (buffer-string))))
              (puthash target revision seen))
            (puthash (elpaca-git--repo-uri recipe) target rewrites)
            (unless (equal revision (gethash target seen))
              (error "Shared repository has conflicting lock revisions: %s" target))))
        (let ((index 0))
          (maphash (lambda (remote mirror)
                     (setenv (format "GIT_CONFIG_KEY_%d" index)
                             (concat "url." mirror ".insteadOf"))
                     (setenv (format "GIT_CONFIG_VALUE_%d" index) remote)
                     (cl-incf index))
                   rewrites)
          (setenv (format "GIT_CONFIG_KEY_%d" index) "protocol.file.allow")
          (setenv (format "GIT_CONFIG_VALUE_%d" index) "always")
          (setenv "GIT_CONFIG_COUNT" (number-to-string (1+ index))))
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
        (princ "Locked restore, package builds and verification: PASS\n"))
    (delete-directory temporary t)))

(provide 'restore-config)
;;; restore-config.el ends here
