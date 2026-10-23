;;; config-compile-local-test.el --- Background local compilation -*- lexical-binding: t; -*-

;;; Commentary:
;; Compile scratch libraries the way startup compiles lisp/, and check that
;; the test gate loads an edited source over its older bytecode.

;;; Code:

(require 'config-test-support)
(require 'bytecomp)
(require '+compile-local)

(defmacro +compile-local-test--with-dir (&rest body)
  "Run BODY with `dir', a scratch directory on `load-path'."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory
                (file-truename (make-temp-file "compile-local-" t))))
          (load-path (cons dir load-path)))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun +compile-local-test--write (file text &optional age)
  "Write TEXT to FILE and date it AGE seconds in the past."
  (with-temp-file file (insert text))
  (when age
    (set-file-times file (time-subtract nil age))))

(defun +compile-local-test--library (feature value)
  "Return the source of FEATURE, whose function returns VALUE."
  (format ";;; -*- lexical-binding: t; -*-\n(defun %s () %S)\n(provide '%s)\n"
          feature value feature))

(ert-deftest +compile-local-test-stale-files ()
  "Missing and older bytecode is stale; current and unloadable files are not."
  (+compile-local-test--with-dir
    (let ((other (expand-file-name "off-path/" dir)))
      (make-directory other)
      (+compile-local-test--write (expand-file-name "missing.el" dir) "")
      (+compile-local-test--write (expand-file-name "older.el" dir) "")
      (+compile-local-test--write (expand-file-name "older.elc" dir) "" 60)
      (+compile-local-test--write (expand-file-name "current.el" dir) "" 60)
      (+compile-local-test--write (expand-file-name "current.elc" dir) "")
      (+compile-local-test--write (expand-file-name ".#lock.el" dir) "")
      (+compile-local-test--write (expand-file-name "skipped.el" other) "")
      (should (equal (sort (mapcar #'file-name-nondirectory
                                   (+compile-local-stale-files dir))
                           #'string<)
                     '("missing.el" "older.el"))))))

(ert-deftest +compile-local-test-compiles-in-background-and-logs-failures ()
  "Good libraries get bytecode; a failing one is named in the log buffer."
  (+compile-local-test--with-dir
    (let ((+compile-local-buffer (generate-new-buffer-name " *compile-local*"))
          (+compile-local--process nil))
      (+compile-local-test--write (expand-file-name "+clt-good.el" dir)
                                  (+compile-local-test--library '+clt-good 1))
      (+compile-local-test--write
       (expand-file-name "+clt-bad.el" dir)
       ";;; -*- lexical-binding: t; -*-\n(require '+clt-missing-feature)\n")
      (let ((process (+compile-local-start dir)))
        (should (processp process))
        (should-not (+compile-local-start dir))
        (let ((deadline (+ (float-time) 60)))
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.1)))
        (should-not (process-live-p process))
        (should (= (process-exit-status process) 1))
        (should (file-exists-p (expand-file-name "+clt-good.elc" dir)))
        (should-not (file-exists-p (expand-file-name "+clt-bad.elc" dir)))
        (with-current-buffer +compile-local-buffer
          (should (string-search "+compile-local failed: " (buffer-string)))
          (should (string-search "+clt-bad.el" (buffer-string))))
        (kill-buffer +compile-local-buffer)
        (should (equal (mapcar #'file-name-nondirectory
                               (+compile-local-stale-files dir))
                       '("+clt-bad.el")))))))

(ert-deftest +compile-local-test-gate-loads-newer-source ()
  "The gate loads a source edited after compilation, not its old bytecode."
  (+compile-local-test--with-dir
    (let ((source (expand-file-name "+clt-edited.el" dir)))
      (+compile-local-test--write source
                                  (+compile-local-test--library '+clt-edited 1)
                                  60)
      (should (byte-compile-file source))
      (set-file-times (concat source "c") (time-subtract nil 30))
      (+compile-local-test--write source
                                  (+compile-local-test--library '+clt-edited 2))
      (load "+clt-edited" nil t)
      (should (= (funcall '+clt-edited) 2)))))

(provide 'config-compile-local-test)
;;; config-compile-local-test.el ends here
