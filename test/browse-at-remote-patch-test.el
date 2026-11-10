;;; browse-at-remote-patch-test.el --- browse-at-remote el-patch checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Link the revision a git-timemachine buffer shows.

;;; Code:

(require 'config-test-support)
(require '+browse-at-remote-patch)

(defvar git-timemachine-mode)

(ert-deftest +browse-at-remote-patch-test-timemachine-revision ()
  (let* ((directory (file-name-as-directory
                     (file-truename (make-temp-file "browse-at-remote-" t))))
         (default-directory directory)
         (process-environment
          (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                    "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                    "GIT_COMMITTER_NAME=Test"
                    "GIT_COMMITTER_EMAIL=test@example.com")
                  process-environment))
         (file (expand-file-name "a.txt" directory)))
    (unwind-protect
        (progn
          (dolist (args '(("init" "-q" "-b" "main")
                          ("remote" "add" "origin"
                           "https://github.com/someone/repo.git")))
            (should (eql 0 (apply #'call-process "git" nil nil nil args))))
          (write-region "one\ntwo\nthree\n" nil file nil 'silent)
          (should (eql 0 (call-process "git" nil nil nil "add" ".")))
          (should (eql 0 (call-process "git" nil nil nil "commit" "-qm" "x")))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (let ((transient-mark-mode t))
                  (goto-char (point-min))
                  (push-mark (point) t t)
                  (forward-line 2)
                  (setq-local git-timemachine-mode t
                              git-timemachine-revision '("abc1234" "x"))
                  (should (equal "https://github.com/someone/repo/blob/abc1234/a.txt#L1-L2"
                                 (browse-at-remote-get-url)))
                  (setq-local git-timemachine-mode nil)
                  (should-not (string-search "abc1234"
                                             (browse-at-remote-get-url))))
              (kill-buffer))))
      (delete-directory directory t))))

(provide 'browse-at-remote-patch-test)
;;; browse-at-remote-patch-test.el ends here
