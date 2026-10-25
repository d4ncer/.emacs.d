;;; +compile-local.el --- Byte-compile local libraries in the background -*- lexical-binding: t; -*-

;;; Commentary:
;; Local libraries live as source under `+lisp-dir', and package builds under
;; `elpaca-builds-directory'.  `+compile-local-start' byte-compiles the stale
;; ones in a batch Emacs that shares this session's
;; `load-path', so the session never waits on the compiler.  With
;; `load-prefer-newer', an edited source still wins over its old .elc, and
;; native compilation follows from loading the .elc.

;;; Code:

(require 'cl-lib)

(defvar +lisp-dir)

(defvar +compile-local-buffer "*+compile-local*"
  "Name of the buffer that logs the last background compilation.")

(defvar +compile-local--process nil
  "The running compilation process, or nil.")

(defun +compile-local--sources (directory)
  "Return the .el files under DIRECTORY, skipping dot files.
Symlinked directories are not followed.  Avoid
`directory-files-recursively': on Emacs 32.0.50 it sometimes drops a
symlinked file, and every file in a package build is a symlink."
  (cl-loop for name in (directory-files directory nil "\\`[^.]")
           for file = (expand-file-name name directory)
           nconc (cond ((file-directory-p file)
                        (unless (file-symlink-p file)
                          (+compile-local--sources file)))
                       ((string-suffix-p ".el" name) (list file)))))

(defun +compile-local-stale-p (file)
  "Return non-nil if FILE's .elc is missing or older and FILE compiles.
Files that declare `no-byte-compile' never count."
  (and (file-newer-than-file-p file (concat file "c"))
       (with-temp-buffer
         (insert-file-contents file)
         (not (re-search-forward "no-byte-compile: *t" nil t)))))

(defun +compile-local-stale-files (directory)
  "Return the stale .el files under DIRECTORY; see `+compile-local-stale-p'.
Only files in a `load-path' directory count, since nothing loads the rest."
  (let ((loadable (mapcar (lambda (dir) (file-name-as-directory
                                         (expand-file-name dir)))
                          load-path)))
    (seq-filter (lambda (file)
                  (and (member (file-name-directory file) loadable)
                       (+compile-local-stale-p file)))
                (+compile-local--sources (expand-file-name directory)))))

(defun +compile-local--form (files)
  "Return the batch form that byte-compiles FILES with this `load-path'."
  `(progn
     (setq load-path ',load-path
           load-prefer-newer t
           native-comp-jit-compilation nil)
     (require 'bytecomp)
     (let ((failed 0))
       (dolist (file ',files)
         (unless (condition-case err (byte-compile-file file)
                   (error (message "%s: %s" file (error-message-string err))
                          nil))
           (setq failed (1+ failed))
           (message "+compile-local failed: %s" file)))
       (kill-emacs (min failed 100)))))

(defun +compile-local--sentinel (process _event)
  "Report PROCESS's failures once it exits."
  (unless (process-live-p process)
    (let ((failed (process-exit-status process)))
      (unless (zerop failed)
        (message "%s local librar%s failed to byte-compile; see %s"
                 failed (if (= failed 1) "y" "ies") +compile-local-buffer)))))

(defun +compile-local-start (&optional directories)
  "Byte-compile stale libraries under DIRECTORIES in a batch Emacs.
DIRECTORIES defaults to `+lisp-dir' and `elpaca-builds-directory'.
Return the process, or nil when nothing is stale or a compilation is
already running.  Output, including warnings and failures, goes to
`+compile-local-buffer'."
  (interactive)
  (let ((files (mapcan #'+compile-local-stale-files
                       (or directories
                           (list +lisp-dir
                                 (bound-and-true-p elpaca-builds-directory))))))
    (when (and files (not (process-live-p +compile-local--process)))
      (message "Byte-compiling %d stale librar%s in the background"
               (length files) (if (cdr files) "ies" "y"))
      (let ((buffer (get-buffer-create +compile-local-buffer)))
        (with-current-buffer buffer (erase-buffer))
        (setq +compile-local--process
              (make-process
               :name "compile-local" :buffer buffer :noquery t
               :connection-type 'pipe :sentinel #'+compile-local--sentinel
               :command (list (expand-file-name invocation-name
                                                invocation-directory)
                              "-Q" "--batch" "--eval"
                              (prin1-to-string
                               (+compile-local--form files)))))))))

(provide '+compile-local)
;;; +compile-local.el ends here
