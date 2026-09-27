;;; agent-shell-review-git-test.el --- Snapshot tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise review snapshots in disposable Git repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-review-git nil t)

(defun agent-shell-review-git-test--git (root &rest args)
  "Run Git ARGS in ROOT, signaling on failure."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (zerop status)
          (error "Git failed: %S: %s" args (buffer-string)))
        (string-trim (buffer-string))))))

(defmacro agent-shell-review-git-test--with-repo (&rest body)
  "Create a Git repository and run BODY with `root' bound."
  (declare (indent 0))
  `(let ((root (make-temp-file "agent-shell-review-git-" t)))
     (unwind-protect
         (progn
           (agent-shell-review-git-test--git root "init" "-b" "main")
           (agent-shell-review-git-test--git root "config" "user.email"
                                             "test@example.invalid")
           (agent-shell-review-git-test--git root "config" "user.name"
                                             "Review Test")
           (with-temp-file (expand-file-name "app.txt" root)
             (insert "base\n"))
           (agent-shell-review-git-test--git root "add" "app.txt")
           (agent-shell-review-git-test--git root "commit" "-m" "base")
           ,@body)
       (delete-directory root t))))

(ert-deftest agent-shell-review-git-test-pushed-branch ()
  "A feature pushed to its upstream still includes its commit."
  (agent-shell-review-git-test--with-repo
    (let ((main (agent-shell-review-git-test--git root "rev-parse" "HEAD")))
      (agent-shell-review-git-test--git root "remote" "add" "origin" root)
      (agent-shell-review-git-test--git root "update-ref"
                                        "refs/remotes/origin/main" main)
      (agent-shell-review-git-test--git root "symbolic-ref"
                                        "refs/remotes/origin/HEAD"
                                        "refs/remotes/origin/main")
      (agent-shell-review-git-test--git root "switch" "-c" "feature")
      (with-temp-file (expand-file-name "app.txt" root)
        (insert "committed-feature\n"))
      (agent-shell-review-git-test--git root "commit" "-am" "feature")
      (agent-shell-review-git-test--git
       root "update-ref" "refs/remotes/origin/feature"
       (agent-shell-review-git-test--git root "rev-parse" "HEAD"))
      (agent-shell-review-git-test--git root "config" "branch.feature.remote"
                                        "origin")
      (agent-shell-review-git-test--git root "config" "branch.feature.merge"
                                        "refs/heads/feature")
      (let ((snapshot (agent-shell-review-git-snapshot root)))
        (should (equal (plist-get snapshot :base) "refs/remotes/origin/main"))
        (should (string-match-p "committed-feature"
                                (plist-get snapshot :diff)))))))

(ert-deftest agent-shell-review-git-test-working-tree ()
  "Include staged, unstaged, and untracked changes and detect staleness."
  (agent-shell-review-git-test--with-repo
    (with-temp-file (expand-file-name "app.txt" root)
      (insert "staged\n"))
    (agent-shell-review-git-test--git root "add" "app.txt")
    (with-temp-file (expand-file-name "app.txt" root)
      (insert "staged\nunstaged\n"))
    (with-temp-file (expand-file-name "new file with spaces.txt" root)
      (insert "new content\n"))
    (let ((snapshot (agent-shell-review-git-snapshot root)))
      (should (string-match-p "staged" (plist-get snapshot :diff)))
      (should (string-match-p "unstaged" (plist-get snapshot :diff)))
      (should (string-match-p "new file with spaces"
                              (plist-get snapshot :diff)))
      (with-temp-file (expand-file-name "app.txt" root)
        (insert "changed again\n"))
      (should-not (equal (plist-get snapshot :fingerprint)
                         (agent-shell-review-git-current-fingerprint
                          snapshot))))))

(ert-deftest agent-shell-review-git-test-untracked-binary-size ()
  "List binary changes and reject empty or oversized snapshots."
  (agent-shell-review-git-test--with-repo
    (should-error (agent-shell-review-git-snapshot root) :type 'user-error)
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file (expand-file-name "binary file.bin" root)
        (insert (unibyte-string 0 1 2 3 255))))
    (let ((snapshot (agent-shell-review-git-snapshot root)))
      (should (string-match-p "binary file.bin"
                              (plist-get snapshot :diff)))
      (should (string-match-p "Binary"
                              (plist-get snapshot :diff))))
    (let ((agent-shell-review-git-max-bytes 8))
      (should-error (agent-shell-review-git-snapshot root) :type 'user-error)))
  (let ((empty-root (make-temp-file "review-no-git-" t)))
    (unwind-protect
        (should-error (agent-shell-review-git-snapshot empty-root)
                      :type 'user-error)
      (delete-directory empty-root t))))

(provide 'agent-shell-review-git-test)
;;; agent-shell-review-git-test.el ends here
