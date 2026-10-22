;;; config-test-support.el --- Isolated configuration checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Load installed dependencies without running the package manager or init file.

;;; Code:

(require 'cl-lib)
(require 'ert)
(defconst +config-test-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "lisp" +config-test-root))
(dolist (directory (directory-files (expand-file-name "elpaca/builds" +config-test-root)
                                  t "^[^. ]"))
  (when (file-directory-p directory) (add-to-list 'load-path directory)))
(require '+bootstrap)
(+bootstrap-setup +config-test-root)
(require 'use-package-ensure)
(require 'general)
(setq use-package-ensure-function #'ignore)

(defun +config-test-declaration (file package)
  "Read PACKAGE's top-level use-package declaration from FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file +config-test-root))
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) package))
                (when-let* ((ensure (memq :ensure form)))
                  (setcar (cdr ensure) nil))
                (throw 'found form))))
        (end-of-file (error "Declaration not found: %s" package))))))

(defun +config-test-load-module (file)
  "Evaluate FILE with installed dependencies and package installation disabled."
  (require 'exec-path-from-shell)
  (require 'keychain-environment)
  (cl-letf (((symbol-function 'use-package-normalize/:ensure)
             (lambda (&rest _) '(nil)))
            ((symbol-function 'exec-path-from-shell-initialize) #'ignore)
            ((symbol-function 'keychain-refresh-environment) #'ignore))
    (load (expand-file-name file +config-test-root) nil t)))

(provide 'config-test-support)
;;; config-test-support.el ends here
