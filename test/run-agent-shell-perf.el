;;; run-agent-shell-perf.el --- agent-shell workaround verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-perf.el from this repo.
;; Drives real agent shells against the ACP fixture.

;;; Code:

(setq load-prefer-newer t)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                      t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-perf-acp-test.el" root)
        nil t)
  (let ((stats (ert-run-tests-batch "^+agent-shell-perf-acp-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-perf)
;;; run-agent-shell-perf.el ends here
