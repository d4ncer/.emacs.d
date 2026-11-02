;;; agent-shell-pstack-arena-sandbox-test.el --- Arena candidates under sandbox-exec -*- lexical-binding: t; -*-

;;; Commentary:
;; Provision real candidate worktrees in scratch repositories and run
;; commands under the sandbox profile an arena candidate gets.  Tests that
;; run sandbox-exec skip off macOS.

;;; Code:

(require 'ert)
(require 'config-test-support)
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

(defun +arena-sandbox-test--candidate (repo name &optional deadline)
  "Provision candidate NAME over the lead at REPO with explicit DEADLINE.
Return (MANIFEST WORKTREE SANDBOX)."
  (let* ((manifest (+arena-git-test--arena (+arena-git-test--base repo)))
         (worktree (+arena-git-test--provision manifest name))
         (+agent-shell-pstack-panel-directory
          (expand-file-name "reports/" +arena-git-test--scratch)))
    (list manifest worktree
          (+agent-shell-pstack-arena-sandbox manifest name deadline))))

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

(ert-deftest +arena-sandbox-test-deadline ()
  "Each candidate's and the judge's sandbox carries the deadline it was
given, so arenas running at once never share one."
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (now (float-time))
           (made
            (mapcar (lambda (deadline)
                      (pcase-let ((`(,manifest ,_ ,sandbox)
                                   (+arena-sandbox-test--candidate
                                    repo (format "c%d" (round (- deadline now)))
                                    deadline)))
                        (list deadline sandbox
                              (+agent-shell-pstack-arena-judge-sandbox
                               manifest deadline))))
                    (list (+ now 3600) (+ now 7200)))))
      (unwind-protect
          (pcase-dolist (`(,deadline . ,sandboxes) made)
            (dolist (sandbox sandboxes)
              (should (equal deadline
                             (+agent-shell-sandbox-deadline sandbox)))))
        (dolist (sandbox (mapcan #'cdr made))
          (+arena-sandbox-test--discard sandbox))))))

(ert-deftest +arena-sandbox-test-read-only-git ()
  "Run git status and git diff in a candidate, never the lead's files.
The lead's git directory stays unwritable."
  (+arena-sandbox-test--on-macos
    (+config-test-require 'sandbox-exec)
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

(ert-deftest +arena-sandbox-test-profile ()
  "Apply a repository's profile: generated files, allowances and
environment, with the generated files left out of the result."
  (+arena-sandbox-test--on-macos
    (+config-test-require 'sandbox-exec)
    (+arena-git-test--with-scratch
      (let* ((repo (+arena-git-test--repo "a.txt" "a\n" "tools/t" "tool\n"))
             (cache (expand-file-name "cache" +arena-git-test--scratch))
             (other (expand-file-name "other" +arena-git-test--scratch))
             (+agent-shell-pstack-arena-profiles
              `((,repo
                 :read (,(expand-file-name "tools" repo))
                 :write (,(lambda (_) cache))
                 :connect ("/tmp/pa-build-tunnel.sock")
                 :env ,(lambda (sandbox)
                         (list "GOTMPDIR=/profile"
                               (concat "PA_PROFILE="
                                       (+agent-shell-sandbox-tmp sandbox))))
                 :files (("conf/tool.cfg"
                          . ,(lambda (sandbox)
                               (concat "cache="
                                       (+agent-shell-sandbox-tmp sandbox)
                                       "\n"))))))))
        (make-directory cache)
        (make-directory other)
        (pcase-let ((`(,manifest ,worktree ,sandbox)
                     (+arena-sandbox-test--candidate repo "opus")))
          (unwind-protect
              (let ((run (lambda (&rest command)
                           (apply #'+arena-sandbox-test--run sandbox worktree
                                  command)))
                    (tmp (+agent-shell-sandbox-tmp sandbox)))
                (should (equal '("/private/tmp/pa-build-tunnel.sock")
                               (+agent-shell-sandbox-allow-connects sandbox)))
                (should (equal (concat "cache=" tmp "\n")
                               (+arena-git-test--read
                                (expand-file-name "conf/tool.cfg" worktree))))
                (should (equal (cons 0 (concat "/profile " tmp "\n"))
                               (funcall run "/bin/sh" "-c"
                                        "echo $GOTMPDIR $PA_PROFILE")))
                (should (eql 0 (car (funcall run "/usr/bin/touch"
                                             (expand-file-name "x" cache)))))
                (should-not (eql 0 (car (funcall run "/usr/bin/touch"
                                                 (expand-file-name "x"
                                                                   other)))))
                (should (equal '(0 . "tool\n")
                               (funcall run "/bin/cat"
                                        (expand-file-name "tools/t" repo))))
                (should-not (eql 0 (car (funcall run "/bin/cat"
                                                 (expand-file-name "a.txt"
                                                                   repo)))))
                (should (eql 0 (car (funcall run "/bin/sh" "-c"
                                             "echo b > b.txt"))))
                (should (equal "b.txt"
                               (+arena-git-test--git
                                repo "diff" "--name-only"
                                (+agent-shell-arena-git-manifest-base manifest)
                                (+arena-git-test--capture manifest "opus")))))
            (+arena-sandbox-test--discard sandbox)))))))

(defun +arena-sandbox-test--preflight (repo command &optional cancel)
  "Provision candidate opus of REPO and run COMMAND as its preflight.
With CANCEL, stop it at once.  Return (REASON SANDBOX-CONFIG WORKTREE)."
  (let* ((+agent-shell-pstack-arena-profiles `((,repo :preflight ,command)))
         (+agent-shell-pstack-panel-directory
          (expand-file-name "reports/" +arena-git-test--scratch))
         (manifest (+arena-git-test--arena (+arena-git-test--base repo)))
         (worktree (+arena-git-test--provision manifest "opus"))
         (panel (+agent-shell-pstack-panel--make
                 :id "panel1" :shape 'arena :timeout 60 :arena manifest
                 :members (list (+agent-shell-pstack-panel--make-member
                                 :role "opus" :agent 'codex :kind 'candidate
                                 :state 'pending))))
         (config nil) (outcome nil) (deadline (+ (float-time) 60)))
    (+agent-shell-pstack-panel--fix-deadline panel 600)
    (cl-letf* ((create (symbol-function '+agent-shell-pstack-arena-sandbox))
               ((symbol-function '+agent-shell-pstack-arena-sandbox)
                (lambda (&rest args)
                  (let ((sandbox (apply create args)))
                    (setq config (+agent-shell-sandbox-config sandbox))
                    sandbox))))
      (let ((stop (+agent-shell-pstack-panel--preflight
                   panel (lambda (reason) (setq outcome (list reason))))))
        (when cancel (funcall stop))))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (should outcome)
    (list (car outcome) config worktree)))

(ert-deftest +arena-sandbox-test-preflight ()
  "Run a profile's preflight in a candidate's worktree under its sandbox,
refusing the arena when it fails, cannot read the lead, changes the
worktree, runs too long or is cancelled.  Its sandbox always goes."
  (+arena-sandbox-test--on-macos
    (+config-test-require 'sandbox-exec)
    (+arena-git-test--with-scratch
      (let ((repo (+arena-git-test--repo "a.txt" "a\n" "deps/d" "dep\n")))
        (pcase-dolist (`(,command ,expected)
                       `(("test -f a.txt && test \"$PWD\" != /" nil)
                         ("echo before; echo boom; exit 3"
                          "failed with status 3 .*:\nbefore\nboom\\'")
                         (,(concat "cat " (expand-file-name "a.txt" repo))
                          "failed with status 1")
                         ("echo b > b.txt" "changed the candidate's worktree.*:\n.*b\\.txt")))
          (pcase-let ((`(,reason ,config ,_)
                       (+arena-sandbox-test--preflight repo command)))
            (if expected
                (should (string-match-p expected reason))
              (should-not reason))
            (should-not (file-exists-p config))))
        (let ((+agent-shell-pstack-panel-preflight-timeout 1))
          (should (string-match-p
                   "ran past 1 seconds"
                   (car (+arena-sandbox-test--preflight repo "sleep 30")))))
        (should (string-match-p
                 "cancelled during its preflight"
                 (car (+arena-sandbox-test--preflight repo "sleep 30" t))))))))

(ert-deftest +arena-sandbox-test-profile-results-checked ()
  "Reject what a profile's functions return when it is malformed, and
leave no private folders behind."
  (+arena-git-test--with-scratch
    (let ((repo (+arena-git-test--repo "a.txt" "a\n"))
          (made nil))
      (advice-add '+agent-shell-sandbox--make-tmp :filter-return
                  (lambda (tmp) (push tmp made) tmp)
                  '((name . pa-test-record)))
      (unwind-protect
          (dolist (profile `((:write (,(lambda (_) "relative")))
                             (:env ,(lambda (_) '("no equals sign")))
                             (:files (("x.cfg" . ,(lambda (_) 42))))))
            (let ((+agent-shell-pstack-arena-profiles
                   (list (cons repo profile))))
              (should-error (+arena-sandbox-test--candidate repo "opus")
                            :type 'user-error)))
        (advice-remove '+agent-shell-sandbox--make-tmp 'pa-test-record))
      (should (= 3 (length made)))
      (+agent-shell-sandbox-test-wait
       (lambda () (not (seq-some #'file-exists-p made)))))))

(provide 'agent-shell-pstack-arena-sandbox-test)
;;; agent-shell-pstack-arena-sandbox-test.el ends here
