;;; config-denylist.el --- Check tracked files against a private denylist -*- lexical-binding: t; -*-

;;; Commentary:
;; The denylist lives outside the repository and holds case-insensitive
;; extended regexes, one per line, that no tracked text file may match.
;; Results name files and line numbers only, so the patterns stay private.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun +config-denylist-patterns (file)
  "Return the patterns in denylist FILE, without comments and blank lines."
  (with-temp-buffer
    (insert-file-contents file)
    (seq-remove (lambda (line) (string-match-p "\\`\\(#\\|[ \t]*\\'\\)" line))
                (split-string (buffer-string) "\n"))))

(defun +config-denylist-check (root file)
  "Check the tracked files of the repository at ROOT against denylist FILE.
Return (skipped) when FILE does not exist, (pass) when nothing matches,
and (fail LOCATION...) with one \"PATH:LINE\" string per matching line."
  (let ((patterns (and (file-readable-p file) (+config-denylist-patterns file))))
    (cond
     ((not (file-readable-p file)) '(skipped))
     ((null patterns) '(pass))
     (t
      ;; Patterns go through a file, so they never appear in a command line.
      (let ((pattern-file (make-temp-file "config-denylist-")))
        (unwind-protect
            (with-temp-buffer
              (write-region (string-join patterns "\n") nil pattern-file nil 'silent)
              ;; Standard error is dropped, since git quotes a bad pattern.
              (let ((status (call-process "git" nil '(t nil) nil
                                          "-C" (expand-file-name root)
                                          "grep" "-I" "-n" "-i" "-E" "-z"
                                          "--no-color" "-f" pattern-file)))
                (pcase status
                  (1 '(pass))
                  (0 (cons 'fail
                           (mapcar (lambda (line)
                                     (pcase-let ((`(,path ,number . ,_)
                                                  (split-string line "\0")))
                                       (concat path ":" number)))
                                   (split-string (buffer-string) "\n" t))))
                  (_ (error "git grep failed (%s)" status)))))
          (delete-file pattern-file)))))))

(provide 'config-denylist)
;;; config-denylist.el ends here
