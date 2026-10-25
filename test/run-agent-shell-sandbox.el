;;; run-agent-shell-sandbox.el --- Sandboxed shell verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-sandbox.el from this repo.
;; Drives a sandboxed agent shell against the ACP fixture.

;;; Code:

(setq load-prefer-newer t)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                      t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-sandbox-acp-test.el" root) nil t)
  (let ((stats (ert-run-tests-batch "^+agent-shell-sandbox-acp-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-sandbox)
;;; run-agent-shell-sandbox.el ends here
