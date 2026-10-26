;;; verify-packages.el --- Recompile package builds after an update -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after updating packages, before the gate:
;;
;;   emacs -Q --batch -l scripts/verify-packages.el [BUILDS-DIRECTORY]
;;
;; Elpaca compiles a package only when it builds it.  It treats an existing
;; build directory as complete, so bytecode deleted afterwards never comes
;; back.  This lists every .el file under the builds directory whose .elc
;; is missing or older than its source, compiles each package's stale files
;; in its own batch Emacs, and exits non-zero if any file fails.  Each
;; compiler sees every build directory, so a require of a package that the
;; Package-Requires header omits still resolves.  BUILDS-DIRECTORY defaults
;; to this configuration's elpaca/builds.

;;; Code:

(require 'cl-lib)

;; The shared finder may have older bytecode beside an edited source.
(setq load-prefer-newer t)
(add-to-list 'load-path
             (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require '+compile-local)

(defun +verify-packages-stale (package)
  "Return PACKAGE directory's compilable .el files lacking an up-to-date .elc."
  (seq-filter #'+compile-local-stale-p (+compile-local--sources package)))

(defun +verify-packages-form (packages files)
  "Return a batch form compiling FILES with every directory in PACKAGES."
  `(progn
     (dolist (dir ',packages)
       (let ((default-directory dir))
         (add-to-list 'load-path dir)
         (normal-top-level-add-subdirs-to-load-path)))
     (setq load-prefer-newer t)
     (let ((failed nil))
       (dolist (file ',files)
         (unless (condition-case err (byte-compile-file file)
                   (error (message "Error: %s" (error-message-string err))
                          nil))
           (push file failed)))
       (dolist (file failed) (message "+verify-packages failed: %s" file))
       (kill-emacs (if failed 1 0)))))

(defun +verify-packages-compile (packages package files)
  "Compile PACKAGE's FILES in a batch Emacs; return the failed files."
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "-Q" "--batch" "--eval"
                   (prin1-to-string (+verify-packages-form packages files)))))
      (goto-char (point-min))
      (let (failed errors)
        (while (re-search-forward
                "^\\(?:+verify-packages failed: \\(.*\\)\\|.*Error: .*\\)$"
                nil t)
          (if (match-string 1)
              (push (match-string 1) failed)
            (push (match-string 0) errors)))
        (when (and (null failed) (not (eql status 0)))
          (setq failed files))
        (when failed
          (princ (format "%s: FAILED %d of %d\n"
                         (file-name-nondirectory package)
                         (length failed) (length files)))
          (dolist (line (nreverse errors)) (princ (format "  %s\n" line))))
        failed))))

(let* ((builds (file-name-as-directory
                (expand-file-name
                 (or (pop command-line-args-left)
                     (expand-file-name
                      "../elpaca/builds"
                      (file-name-directory load-file-name))))))
       (packages (cl-remove-if-not #'file-directory-p
                                   (directory-files builds t "\\`[^.]")))
       (stale (cl-loop for package in packages
                       for files = (+verify-packages-stale package)
                       when files collect (cons package files)))
       (failed 0))
  (princ (format "%d packages in %s\n" (length packages) builds))
  (if (null stale)
      (princ "All bytecode is up to date.\n")
    (princ (format "Stale bytecode in %d packages, %d files:\n" (length stale)
                   (apply #'+ (mapcar (lambda (entry) (length (cdr entry)))
                                      stale))))
    (pcase-dolist (`(,package . ,files) stale)
      (princ (format "  %s: %d\n" (file-name-nondirectory package)
                     (length files))))
    (pcase-dolist (`(,package . ,files) stale)
      (cl-incf failed (length (+verify-packages-compile
                               packages package files))))
    (princ (format "Compiled %d packages; %d files failed.\n"
                   (length stale) failed)))
  (kill-emacs (if (zerop failed) 0 1)))

;;; verify-packages.el ends here
