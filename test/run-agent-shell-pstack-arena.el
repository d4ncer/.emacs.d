;;; run-agent-shell-pstack-arena.el --- pstack arena verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-pstack-arena.el from
;; this repo.  Drives a lead, sandboxed candidates and a judge against the
;; ACP fixture in a scratch git repository.

;;; Code:

(setq load-prefer-newer t)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                      t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-pstack-arena-acp-test.el" root)
        nil t)
  (let ((stats (ert-run-tests-batch "^+agent-shell-pstack-arena-acp-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-pstack-arena)
;;; run-agent-shell-pstack-arena.el ends here
