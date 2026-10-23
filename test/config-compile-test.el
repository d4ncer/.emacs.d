;;; config-compile-test.el --- Helper compilation gate -*- lexical-binding: t; -*-

;;; Commentary:
;; Compile infrastructure and adapters with warnings treated as errors.

;;; Code:

(require 'config-test-support)
(require 'bytecomp)
(let* ((temporary (make-temp-file "config-bytecode-" t))
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (file) (expand-file-name (concat (file-name-base file) ".elc") temporary))))
  (unwind-protect
      (dolist (file '("lisp/+bootstrap.el" "lisp/+corelib.el"
                       "lisp/+keybindings.el" "lisp/+use-package.el"
                       "lisp/+edit-cmds.el" "lisp/+profile.el"
                       "lisp/+modeline-data.el" "lisp/+grep-edit.el"
                       "lisp/+agent-shell-drive.el"
                       "lisp/+agent-shell-pstack.el"
                       "lisp/+agent-shell-pstack-panel.el"
                       "lisp/+agent-shell-perf.el" "lisp/+compile-local.el"
                       "lisp/compat/+expert-compat.el"
                       "lisp/compat/+eglot-tempel-compat.el"))
        (unless (byte-compile-file (expand-file-name file +config-test-root))
          (error "Compilation failed: %s" file)))
    (delete-directory temporary t)))

(provide 'config-compile-test)
;;; config-compile-test.el ends here
