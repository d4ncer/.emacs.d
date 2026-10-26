;;; run-agent-shell-pstack-arena-remote.el --- Remote pstack arena verification runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-agent-shell-pstack-arena-remote.el from
;; this repo.  Runs remote arenas on a local host whose runner drives
;; sandboxed fixture candidates and a judge in scratch git clones.

;;; Code:

(setq load-prefer-newer t)

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                      t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (load (expand-file-name "test/agent-shell-pstack-arena-remote-acp-test.el" root)
        nil t)
  (let ((stats (ert-run-tests-batch "^+agent-shell-pstack-arena-remote-acp-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'run-agent-shell-pstack-arena-remote)
;;; run-agent-shell-pstack-arena-remote.el ends here
