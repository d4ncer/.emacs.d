;;; agent-shell-pstack-arena-sandbox-test.el --- Arena candidates under sandbox-exec -*- lexical-binding: t; -*-

;;; Commentary:
;; Provision real candidate worktrees in scratch repositories and run
;; commands under the sandbox profile an arena candidate gets.  Tests that
;; run sandbox-exec skip off macOS.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-arena-git-test)
(require '+agent-shell-pstack-panel)

(defmacro +arena-sandbox-test--on-macos (&rest body)
  "Run BODY only where sandbox-exec exists."
  (declare (indent 0))
  `(progn
     (skip-unless (and (eq system-type 'darwin)
                       (file-executable-p "/usr/bin/sandbox-exec")))
     ,@body))

(defun +arena-sandbox-test--candidate (repo name)
  "Provision candidate NAME over the lead at REPO.
Return (MANIFEST WORKTREE SANDBOX)."
  (let* ((manifest (+arena-git-test--arena (+arena-git-test--base repo)))
         (worktree (+arena-git-test--provision manifest name))
         (+agent-shell-pstack-panel-directory
          (expand-file-name "reports/" +arena-git-test--scratch)))
    (list manifest worktree (+agent-shell-pstack-arena-sandbox manifest name))))

(defun +arena-sandbox-test--run (sandbox directory &rest command)
  "Run COMMAND in DIRECTORY confined to SANDBOX with its environment.
Return (STATUS . OUTPUT)."
  (let ((profile (make-temp-file "pa-profile-" nil ".sb"
                                 (+agent-shell-sandbox-profile sandbox)))
        (default-directory (file-name-as-directory directory))
        (process-environment (append (+agent-shell-sandbox-environment
                                      sandbox)
                                     process-environment)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process "/usr/bin/sandbox-exec" nil t nil
                               "-f" profile command)))
            (cons status (buffer-string))))
      (delete-file profile))))

(defun +arena-sandbox-test--discard (sandbox)
  "Delete SANDBOX's private folders."
  (delete-directory (+agent-shell-sandbox-tmp sandbox) t)
  (delete-directory (+agent-shell-sandbox-config sandbox) t))

(ert-deftest +arena-sandbox-test-read-only-git ()
  "Run git status and git diff in a candidate, never the lead's files.
The lead's git directory stays unwritable."
  (+arena-sandbox-test--on-macos
    (+arena-git-test--with-scratch
      (let ((repo (+arena-git-test--repo "a.txt" "a\n")))
        (pcase-let ((`(,manifest ,worktree ,sandbox)
                     (+arena-sandbox-test--candidate repo "opus")))
          (unwind-protect
              (let ((run (lambda (&rest command)
                           (apply #'+arena-sandbox-test--run sandbox worktree
                                  command)))
                    (common (+agent-shell-arena-git-common-dir manifest)))
                (+arena-git-test--write (expand-file-name "a.txt" worktree)
                                        "changed\n")
                (should (equal '(0 . " M a.txt\n")
                               (funcall run "git" "status" "--porcelain")))
                (let ((diff (funcall run "git" "diff")))
                  (should (eql 0 (car diff)))
                  (should (string-search "+changed" (cdr diff))))
                (should-not (eql 0 (car (funcall run "/bin/cat"
                                                 (expand-file-name "a.txt"
                                                                   repo)))))
                (should-not (eql 0 (car (funcall run "/usr/bin/touch"
                                                 (expand-file-name "probe"
                                                                   common)))))
                (should-not (file-exists-p (expand-file-name "probe" common)))
                (should-not (eql 0 (car (funcall run "git" "commit" "-qam"
                                                 "escape")))))
            (+arena-sandbox-test--discard sandbox)))))))

(provide 'agent-shell-pstack-arena-sandbox-test)
;;; agent-shell-pstack-arena-sandbox-test.el ends here
