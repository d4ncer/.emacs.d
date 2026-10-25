;;; config-verify-packages-test.el --- Package build verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Run scripts/verify-packages.el against disposable builds directories.

;;; Code:

(require 'config-test-support)

(defun +config-verify-packages-run (builds)
  "Run the verify script on BUILDS; return (STATUS . OUTPUT)."
  (with-temp-buffer
    (cons (call-process (expand-file-name invocation-name invocation-directory)
                        nil t nil "-Q" "--batch" "-l"
                        (expand-file-name "scripts/verify-packages.el"
                                          +config-test-root)
                        builds)
          (buffer-string))))

(defun +config-verify-packages-write (file &rest forms)
  "Write FORMS to FILE as a lexical-binding library."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))

(ert-deftest +config-verify-packages-compiles-stale-builds ()
  (let* ((root (make-temp-file "verify-packages-" t))
         (sources (expand-file-name "sources/" root))
         (builds (expand-file-name "builds/" root)))
    (unwind-protect
        (progn
          (+config-verify-packages-write
           (expand-file-name "base/base.el" sources)
           '(defmacro base-double (x) `(* 2 ,x)) '(provide 'base))
          (+config-verify-packages-write
           (expand-file-name "user/user.el" sources)
           '(require 'base) '(defun user-four () (base-double 2))
           '(provide 'user))
          (+config-verify-packages-write
           (expand-file-name "user/data/user-data.el" sources)
           '(provide 'user-data))
          (+config-verify-packages-write
           (expand-file-name "skip/skip.el" sources)
           '(provide 'skip))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "skip/skip.el" sources))
            (insert ";; no-byte-compile: t\n")
            (write-region nil nil (expand-file-name "skip/skip.el" sources)))
          (dolist (package '("base" "user" "skip"))
            (make-directory (expand-file-name package builds) t)
            (make-symbolic-link
             (expand-file-name (concat package "/" package ".el") sources)
             (expand-file-name (concat package "/" package ".el") builds)))
          (make-symbolic-link (expand-file-name "user/data/" sources)
                              (expand-file-name "user/data" builds))
          (let ((result (+config-verify-packages-run builds)))
            (should (eql (car result) 0))
            (should (string-match-p "Stale bytecode in 2 packages, 2 files"
                                    (cdr result))))
          (should (file-exists-p (expand-file-name "user/user.elc" builds)))
          (should (file-exists-p (expand-file-name "base/base.elc" builds)))
          (should-not (file-exists-p (expand-file-name "skip/skip.elc" builds)))
          (should-not (file-exists-p
                       (expand-file-name "user/data/user-data.elc" sources)))
          (let ((result (+config-verify-packages-run builds)))
            (should (eql (car result) 0))
            (should (string-match-p "All bytecode is up to date" (cdr result))))
          (+config-verify-packages-write
           (expand-file-name "broken/broken.el" builds)
           '(require 'missing-library))
          (let ((result (+config-verify-packages-run builds)))
            (should (eql (car result) 1))
            (should (string-match-p "broken: FAILED 1 of 1" (cdr result)))
            (should (string-match-p "missing-library" (cdr result)))))
      (delete-directory root t))))

(provide 'config-verify-packages-test)
;;; config-verify-packages-test.el ends here
