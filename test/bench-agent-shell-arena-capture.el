;;; bench-agent-shell-arena-capture.el --- Time arena candidate capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -L lisp -l test/bench-agent-shell-arena-capture.el
;;
;; Generates a scratch repository of ARENA_BENCH_FILES files (default
;; 50000), provisions one candidate, edits one file and times
;; `+agent-shell-arena-git-capture'.  Put another copy of
;; +agent-shell-arena-git.el first on the load path to time it instead.
;;
;; "fresh" captures straight after provisioning, when files the checkout
;; wrote in the index's final second are racily clean and get hashed.
;; "refreshed" first waits a second and refreshes the candidate's index,
;; as any git command the candidate runs would.

;;; Code:

(setq load-prefer-newer t)
(require 'cl-lib)
(require '+agent-shell-arena-git)

(defun +arena-bench--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return its output, failing on error."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (buffer-string))))

(defun +arena-bench--wait (start)
  "Call START with a callback and return its result, failing on error."
  (let ((outcome nil))
    (funcall start (lambda (result error) (setq outcome (list result error))))
    (while (not outcome) (accept-process-output nil 0.01))
    (when (cadr outcome) (error "Arena call failed: %s" (cadr outcome)))
    (car outcome)))

(defun +arena-bench--time (function)
  "Return the seconds FUNCTION takes."
  (let ((start (float-time)))
    (funcall function)
    (- (float-time) start)))

(let* ((files (string-to-number (or (getenv "ARENA_BENCH_FILES") "50000")))
       (scratch (file-name-as-directory
                 (file-truename (make-temp-file "arena-bench-" t))))
       (repo (expand-file-name "repo/" scratch))
       (config (expand-file-name "gitconfig" scratch))
       (process-environment (append (list (concat "GIT_CONFIG_GLOBAL=" config)
                                          "GIT_CONFIG_NOSYSTEM=1")
                                    process-environment))
       (+agent-shell-pstack-arena-directory (expand-file-name "arenas/" scratch))
       (+agent-shell-pstack-arena-profiles nil))
  (unwind-protect
      (progn
        (with-temp-file config
          (insert "[user]\n\tname = Bench\n\temail = bench@example.com\n"))
        (dotimes (i files)
          (let ((file (expand-file-name (format "d%03d/f%05d.txt" (/ i 1000) i)
                                        repo)))
            (when (zerop (% i 1000)) (make-directory (file-name-directory file) t))
            (write-region (format "file %d\n%s\n" i (make-string 64 ?x))
                          nil file nil 'silent)))
        (+arena-bench--git repo "init" "-q")
        (+arena-bench--git repo "add" "-A")
        (+arena-bench--git repo "commit" "-q" "-m" "initial")
        (let* ((base (+arena-bench--wait
                      (lambda (done)
                        (+agent-shell-arena-git-capture-base repo done))))
               (manifest (+agent-shell-arena-git-create base "bench")))
          (dolist (scenario '("fresh" "refreshed"))
            (let* ((worktree (+arena-bench--wait
                              (lambda (done)
                                (+agent-shell-arena-git-provision
                                 manifest scenario done)))))
              (when (equal scenario "refreshed")
                (sleep-for 1.1)
                (+arena-bench--git worktree "update-index" "-q" "--refresh"))
              (write-region "edited\n" nil
                            (expand-file-name "d000/f00000.txt" worktree)
                            nil 'silent)
              (let* ((commit nil)
                     (seconds (+arena-bench--time
                               (lambda ()
                                 (setq commit
                                       (+arena-bench--wait
                                        (lambda (done)
                                          (+agent-shell-arena-git-capture
                                           manifest scenario done)))))))
                     (changed (+arena-bench--git
                               repo "diff" "--name-only"
                               (+agent-shell-arena-git-base-commit base)
                               commit)))
                (unless (equal changed "d000/f00000.txt\n")
                  (error "Capture recorded the wrong change: %S" changed))
                (princ (format "%s %s files, %s capture: %.2fs\n"
                               (file-name-directory
                                (locate-library "+agent-shell-arena-git"))
                               files scenario seconds)))))
          (+arena-bench--wait
           (lambda (done) (+agent-shell-arena-git-clean manifest done)))))
    (delete-directory scratch t)))

(provide 'bench-agent-shell-arena-capture)
;;; bench-agent-shell-arena-capture.el ends here
