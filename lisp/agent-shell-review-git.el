;;; agent-shell-review-git.el --- Git snapshots for agent review -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Capture branch commits and working tree changes for one review pass.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup agent-shell-review-git nil
  "Git snapshot capture for agent reviews."
  :group 'tools)

(defcustom agent-shell-review-git-max-bytes 524288
  "Largest review diff in bytes.  Larger changesets are rejected."
  :type 'integer)

(defun agent-shell-review-git--call (root &rest args)
  "Return (STATUS . OUTPUT) for Git ARGS run in ROOT."
  (let ((default-directory root)
        (coding-system-for-read 'utf-8-unix))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (cons status (buffer-string))))))

(defun agent-shell-review-git--ok (root &rest args)
  "Return trimmed output of successful Git ARGS in ROOT, or nil."
  (pcase-let ((`(,status . ,output)
               (apply #'agent-shell-review-git--call root args)))
    (when (zerop status) (string-trim output))))

(defun agent-shell-review-git--ref-exists-p (root ref)
  "Return non-nil if REF names a commit in ROOT."
  (agent-shell-review-git--ok root "rev-parse" "--verify" "--quiet"
                              (concat ref "^{commit}")))

(defun agent-shell-review-git--choose-base (root explicit)
  "Choose a review base for ROOT, honoring EXPLICIT first."
  (when (and explicit
             (not (agent-shell-review-git--ref-exists-p root explicit)))
    (user-error "Review base does not exist: %s" explicit))
  (or explicit
      (let* ((branch (agent-shell-review-git--ok
                      root "symbolic-ref" "--quiet" "--short" "HEAD"))
             (upstream (and (member branch '("main" "master"))
                            (agent-shell-review-git--ok
                             root "rev-parse" "--symbolic-full-name"
                             "@{upstream}")))
             (matching-upstream
              (and upstream
                   (equal (file-name-nondirectory upstream) branch)
                   upstream)))
        (or matching-upstream
            (let* ((remotes (agent-shell-review-git--ok root "remote"))
                   (defaults
                    (delq nil
                          (mapcar
                           (lambda (remote)
                             (let ((ref (format "refs/remotes/%s/HEAD"
                                                remote)))
                               (agent-shell-review-git--ok
                                root "symbolic-ref" "--quiet" ref)))
                           (split-string (or remotes "") "\n" t)))))
              (cond
               ((= (length defaults) 1) (car defaults))
               ((> (length defaults) 1)
                (completing-read "Review base: " defaults nil t))
               (t
                (let ((locals
                       (cl-remove-if-not
                        (lambda (ref)
                          (agent-shell-review-git--ref-exists-p root ref))
                        '("main" "master"))))
                  (cond
                   ((= (length locals) 1) (car locals))
                   ((> (length locals) 1)
                    (completing-read "Review base: " locals nil t)))))))))))

(defun agent-shell-review-git--binary-p (path)
  "Return non-nil when PATH has a NUL in its first 8 KiB."
  (with-temp-buffer
    (insert-file-contents-literally path nil 0
                                    (min 8192 (file-attribute-size
                                               (file-attributes path))))
    (goto-char (point-min))
    (search-forward "\0" nil t)))

(defun agent-shell-review-git--untracked (root)
  "Return review diff text for nonignored untracked files in ROOT."
  (pcase-let ((`(,status . ,paths)
               (agent-shell-review-git--call
                root "ls-files" "--others" "--exclude-standard" "-z")))
    (unless (zerop status)
      (user-error "Cannot list untracked files: %s" (string-trim paths)))
    (mapconcat
     (lambda (relative)
       (let ((path (expand-file-name relative root)))
         (when (> (file-attribute-size (file-attributes path))
                  agent-shell-review-git-max-bytes)
           (user-error "Untracked file is too large for review: %s" relative))
         (if (agent-shell-review-git--binary-p path)
             (format "diff --git a/%s b/%s\nnew file: %s\nBinary file added: %s\n"
                     relative relative relative relative)
           (pcase-let ((`(,diff-status . ,diff)
                        (agent-shell-review-git--call
                         root "diff" "--no-index" "--no-ext-diff"
                         "--no-color" "--" "/dev/null" relative)))
             (unless (= diff-status 1)
               (user-error "Cannot diff untracked file %s: %s"
                           relative (string-trim diff)))
             diff))))
     (split-string paths "\0" t) "\n")))

(defun agent-shell-review-git-snapshot (root &optional base-ref)
  "Capture ROOT changes against BASE-REF or a discovered integration base.
Return a plist with :root, :base, :diff, and :fingerprint."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (top (agent-shell-review-git--ok root "rev-parse"
                                          "--show-toplevel")))
    (unless (and top (equal (file-truename root)
                            (file-truename (file-name-as-directory top))))
      (user-error "Not a Git project root: %s" root))
    (let* ((base (or (agent-shell-review-git--choose-base root base-ref)
                     "HEAD"))
           (merge-base (agent-shell-review-git--ok root "merge-base"
                                                   base "HEAD"))
           (start (or merge-base "HEAD")))
      (when (and base-ref (null merge-base))
        (user-error "Review base has no common ancestor: %s" base-ref))
      (pcase-let ((`(,status . ,tracked)
                   (agent-shell-review-git--call
                    root "diff" "--binary" "--no-ext-diff" "--no-color"
                    start "--")))
        (unless (zerop status)
          (user-error "Cannot capture Git diff: %s" (string-trim tracked)))
        (let ((diff (concat tracked (agent-shell-review-git--untracked root))))
          (when (string-empty-p diff)
            (user-error "There are no changes to review"))
          (when (> (string-bytes diff) agent-shell-review-git-max-bytes)
            (user-error "Review diff exceeds %d bytes"
                        agent-shell-review-git-max-bytes))
          (list :root root :base base :diff diff
                :fingerprint (secure-hash 'sha1 diff)))))))

(defun agent-shell-review-git-current-fingerprint (snapshot)
  "Recompute SNAPSHOT's fingerprint for stale-result detection."
  (condition-case nil
      (plist-get (agent-shell-review-git-snapshot
                  (plist-get snapshot :root)
                  (plist-get snapshot :base))
                 :fingerprint)
    (user-error nil)))

(provide 'agent-shell-review-git)
;;; agent-shell-review-git.el ends here
