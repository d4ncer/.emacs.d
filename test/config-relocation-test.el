;;; config-relocation-test.el --- Legacy worker path checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the paths cached by Emacs sessions started before the package move.

;;; Code:

(require 'config-test-support)
(require 'project-files-test)
(require 'project-files-search-test)
(let ((+project-files--library-directory
       (expand-file-name "lisp/" +config-test-root)))
  (let ((stats (ert-run-tests-batch
                '(member
                  +project-files-test-worker-inherits-discovery-and-preserves-paths
                  +project-files-test-search-worker-reuses-and-supersedes))))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "Legacy finder worker paths failed"))))
(with-temp-buffer
  (unless (zerop
           (call-process
            (expand-file-name invocation-name invocation-directory) nil t nil
            "-Q" "--batch" "-L" (expand-file-name "lisp/" +config-test-root)
            "--eval"
            "(progn (require '+agent-shell-projects-worker)
                    (when (featurep '+agent-shell-projects)
                      (error \"Worker loaded project UI\")))"))
    (error "Legacy agent worker path failed: %s" (buffer-string))))

(provide 'config-relocation-test)
;;; config-relocation-test.el ends here
