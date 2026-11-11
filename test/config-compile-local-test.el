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
      (+compile-local-test--write (expand-file-name "pkg-autoloads.el" dir)
                                  ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n")
      (+compile-local-test--write (expand-file-name "skipped.el" other) "")
      (should (equal (sort (mapcar #'file-name-nondirectory
                                   (+compile-local-stale-files dir))
                           #'string<)
                     '("missing.el" "older.el"))))))

(ert-deftest +compile-local-test-struct-users-go-stale ()
  "Bytecode older than the source of a struct it uses is stale."
  (+compile-local-test--with-dir
    (let ((write (lambda (name text age)
                   (+compile-local-test--write (expand-file-name name dir)
                                               text age))))
      (funcall write "owner.el" "(cl-defstruct (+clt-rec (:copier nil)) a b)\n" 20)
      (funcall write "owner.elc" "" 0)
      (funcall write "user.el" "(+clt-rec-b r)\n" 60)
      (funcall write "user.elc" "" 30)
      (funcall write "rebuilt.el" "(+clt-rec-b r)\n" 60)
      (funcall write "rebuilt.elc" "" 10)
      (funcall write "other.el" "(other)\n" 60)
      (funcall write "other.elc" "" 30)
      (should (equal (mapcar #'file-name-nondirectory
                             (+compile-local-stale-files dir))
                     '("user.el"))))))

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
      (let ((process (+compile-local-start (list dir))))
        (should (processp process))
        (should-not (+compile-local-start (list dir)))
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

(ert-deftest +compile-local-test-covers-every-directory ()
  "One compilation covers stale files across all the given directories."
  (+compile-local-test--with-dir
    (let* ((second (file-name-as-directory (expand-file-name "builds/" dir)))
           (load-path (cons second load-path))
           (+compile-local-buffer (generate-new-buffer-name " *compile-local*"))
           (+compile-local--process nil))
      (make-directory second)
      (+compile-local-test--write (expand-file-name "+clt-one.el" dir)
                                  (+compile-local-test--library '+clt-one 1))
      (+compile-local-test--write (expand-file-name "+clt-two.el" second)
                                  (+compile-local-test--library '+clt-two 2))
      (let ((process (+compile-local-start (list dir second)))
            (deadline (+ (float-time) 60)))
        (while (and (process-live-p process) (< (float-time) deadline))
          (accept-process-output process 0.1))
        (should (= (process-exit-status process) 0))
        (should (file-exists-p (expand-file-name "+clt-one.elc" dir)))
        (should (file-exists-p (expand-file-name "+clt-two.elc" second)))
        (kill-buffer +compile-local-buffer)))))

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

(defvar elpaca-directory)

(defvar elpaca-builds-directory)

(defvar elpaca-sources-directory)

(ert-deftest +compile-local-test-elpaca-real-directory ()
  "Resolve shared Elpaca directories before deriving build paths."
  (let* ((root (make-temp-file "elpaca-link-" t))
         (user-emacs-directory (expand-file-name "config/" root))
         (real (file-name-as-directory
                (file-truename (expand-file-name "shared/" root))))
         (elpaca-directory nil)
         (elpaca-builds-directory nil)
         (elpaca-sources-directory nil))
    (unwind-protect
        (progn
          (make-directory user-emacs-directory)
          (make-directory real)
          (make-symbolic-link real (expand-file-name "elpaca" user-emacs-directory))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "elpaca-bootstrap.el"
                                                    +config-test-root))
            (let (form)
              (while (setq form (condition-case nil (read (current-buffer))
                                  (end-of-file nil)))
                (when (and (eq (car-safe form) 'defvar)
                           (memq (cadr form) '(elpaca-directory
                                               elpaca-builds-directory
                                               elpaca-sources-directory)))
                  (set (cadr form) (eval (caddr form) t))))))
          (should (equal elpaca-directory real))
          (should (equal elpaca-builds-directory (concat real "builds/")))
          (should (equal elpaca-sources-directory (concat real "sources/")))
          (make-directory (expand-file-name "builds/pkg" real) t)
          (should (file-in-directory-p
                   (expand-file-name "elpaca/builds/pkg" user-emacs-directory)
                   elpaca-builds-directory)))
      (delete-directory root t))))

(provide 'config-compile-local-test)
;;; config-compile-local-test.el ends here
