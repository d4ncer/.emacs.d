;;; config-denylist-test.el --- Private denylist gate check -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the denylist checker against a scratch repository and denylist.

;;; Code:

(require 'ert)
(require 'config-denylist)

(defmacro +config-denylist-test--with-repo (denylist &rest body)
  "Run BODY with `root' a scratch repository and `file' holding DENYLIST.
A nil DENYLIST leaves `file' absent."
  (declare (indent 1))
  `(let* ((root (make-temp-file "config-denylist-repo-" t))
          (file (expand-file-name "denylist" (make-temp-file "config-denylist-" t)))
          (default-directory root))
     (unwind-protect
         (progn
           (write-region "first line\nhas a SecretWord here\n# keep\n" nil "a.txt")
           (write-region "nothing to see\n" nil "b.txt")
           (write-region "secretword\0binary\n" nil "c.bin")
           (dolist (arguments '(("init" "-q")
                                ("add" "a.txt" "b.txt" "c.bin")
                                ("-c" "user.name=t" "-c" "user.email=t@example.invalid"
                                 "-c" "commit.gpgsign=false"
                                 "commit" "-q" "-m" "fixture")))
             (should (zerop (apply #'call-process "git" nil nil nil arguments))))
           (write-region "secretword in an untracked file\n" nil "d.txt")
           (when ,denylist (write-region ,denylist nil file nil 'silent))
           ,@body)
       (delete-directory root t)
       (delete-directory (file-name-directory file) t))))

(ert-deftest +config-denylist-test-match-fails ()
  "A match in a tracked text file fails with its path and line only."
  (+config-denylist-test--with-repo "SECRETWORD\n"
    (should (equal (+config-denylist-check root file) '(fail "a.txt:2")))))

(ert-deftest +config-denylist-test-no-match-passes ()
  "Patterns matching nothing tracked pass."
  (+config-denylist-test--with-repo "absent-term\n"
    (should (equal (+config-denylist-check root file) '(pass)))))

(ert-deftest +config-denylist-test-ignores-comments-and-blank-lines ()
  "Comment and blank lines are not patterns."
  (+config-denylist-test--with-repo "# keep\n\n   \nabsent-term\n"
    (should (equal (+config-denylist-check root file) '(pass))))
  (+config-denylist-test--with-repo "# keep\n\n"
    (should (equal (+config-denylist-check root file) '(pass)))))

(ert-deftest +config-denylist-test-missing-denylist-skips ()
  "Without a denylist the check is skipped."
  (+config-denylist-test--with-repo nil
    (should (equal (+config-denylist-check root file) '(skipped)))))

(provide 'config-denylist-test)
;;; config-denylist-test.el ends here
