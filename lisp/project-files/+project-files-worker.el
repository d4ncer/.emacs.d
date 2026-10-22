;;; +project-files-worker.el --- Isolated project indexing -*- lexical-binding: t; -*-

;;; Commentary:
;; Run Projectile's complete file discovery in a child Emacs.  Publish a
;; NUL-separated manifest atomically, without loading the user's init file.

;;; Code:

(require 'cl-lib)

(declare-function projectile-project-files "projectile" (root))
(defvar projectile-enable-caching)
(defvar projectile-auto-update-cache-with-watches)

(defun +project-files-worker-main (request-file)
  "Build the manifest described by REQUEST-FILE."
  (let* ((request (with-temp-buffer
                    (insert-file-contents request-file)
                    (read (current-buffer))))
         (root (plist-get request :root))
         (output (plist-get request :output))
         (temporary (or (plist-get request :temporary)
                        (make-temp-file (concat output ".pending-"))))
         (default-directory root))
    (unwind-protect
        (progn
          (dolist (directory (plist-get request :load-path))
            (add-to-list 'load-path directory))
          (require 'projectile)
          (cl-progv (mapcar #'car (plist-get request :settings))
              (mapcar #'cdr (plist-get request :settings))
            (let* ((projectile-enable-caching nil)
                   (projectile-auto-update-cache-with-watches nil)
                   (files (projectile-project-files root))
                   (coding-system-for-write 'no-conversion))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (dolist (file files)
                  (insert (encode-coding-string file 'utf-8-unix) "\0"))
                (write-region (point-min) (point-max) temporary nil 'silent))
              (rename-file temporary output t)
              (with-temp-file (concat output ".meta")
                (let ((print-length nil) (print-level nil))
                  (prin1 (list :root root :count (length files)
                               :signature (plist-get request :signature))
                         (current-buffer)))))))
      (when (file-exists-p temporary) (delete-file temporary)))))

(provide '+project-files-worker)
;;; +project-files-worker.el ends here
