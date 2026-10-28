;;; run-agent-shell-pstack-delegate.el --- pstack delegate verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-pstack-delegate.el from
;; this repo.  Drives a lead and a full-access delegate against the ACP
;; fixture in a scratch git repository, in each isolation.

;;; Code:

(setq load-prefer-newer t)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                      t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-pstack-delegate-acp-test.el" root)
        nil t)
  (let ((stats (ert-run-tests-batch "^+agent-shell-pstack-delegate-acp-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-pstack-delegate)
;;; run-agent-shell-pstack-delegate.el ends here
