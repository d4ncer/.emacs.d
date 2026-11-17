;;; config-elpaca-guard-test.el --- Shared package folders stay unbuilt -*- lexical-binding: t; -*-

;;; Commentary:
;; A linked worktree, or a checkout whose elpaca links elsewhere, loads
;; existing builds and changes no package; the gate rejects build links
;; that do not resolve or point outside elpaca/sources/.

;;; Code:

(require 'ert)
(require 'config-test-support)
(require '+bootstrap)
(require '+elpaca-guard)
(require 'config-elpaca-links)

(defmacro +elpaca-guard-test--with-root (kind &rest body)
  "Run BODY with `root' a fresh checkout of KIND bound as `+config-root'.
KIND is `owner', with a .git directory and its own elpaca, `worktree',
whose .git is a file, or `linked', whose elpaca is a symlink.  BODY also
sees `shared', the directory a `linked' elpaca points to."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (file-truename (make-temp-file "elpaca-guard-" t))))
          (shared (file-name-as-directory (make-temp-file "elpaca-shared-" t)))
          (+config-root root))
     (unwind-protect
         (progn
           (pcase ,kind
             ('worktree (with-temp-file (expand-file-name ".git" root)
                          (insert "gitdir: /elsewhere\n")))
             (_ (make-directory (expand-file-name ".git" root))))
           (if (eq ,kind 'linked)
               (make-symbolic-link (directory-file-name shared)
                                   (expand-file-name "elpaca" root))
             (make-directory (expand-file-name "elpaca" root)))
           ,@body)
       (delete-directory root t)
       (delete-directory shared t))))

(defun +elpaca-guard-test--order (builtp)
  "Return an order for package p, already built when BUILTP."
  (elpaca<--create :id 'p :package "p" :recipe '(:package "p")
                   :builtp builtp))

(ert-deftest +elpaca-guard-test-reason ()
  "Name a linked worktree and a linked elpaca, and nothing else."
  (+elpaca-guard-test--with-root 'owner
    (should-not (+bootstrap-shared-packages)))
  (+elpaca-guard-test--with-root 'worktree
    (should (string-match-p "is a linked git worktree"
                            (+bootstrap-shared-packages))))
  (+elpaca-guard-test--with-root 'linked
    (should (string-match-p "elpaca links to"
                            (+bootstrap-shared-packages)))))

(ert-deftest +elpaca-guard-test-build-steps ()
  "Only load existing builds in a shared checkout; build freely otherwise."
  (+elpaca-guard-test--with-root 'owner
    (should (equal elpaca-default-build-steps
                   (elpaca-build-steps (+elpaca-guard-test--order nil)))))
  (dolist (kind '(worktree linked))
    (+elpaca-guard-test--with-root kind
      (should (equal elpaca--pre-built-steps
                     (elpaca-build-steps (+elpaca-guard-test--order t))))
      (should (equal '(+elpaca-guard--refuse)
                     (elpaca-build-steps (+elpaca-guard-test--order nil))))
      (dolist (context '(:rebuild :merge))
        (should (equal '(+elpaca-guard--refuse)
                       (elpaca-build-steps (+elpaca-guard-test--order t)
                                           context)))))))

(ert-deftest +elpaca-guard-test-refusal ()
  "Fail an order that would build here, saying why, and keep its build."
  (+elpaca-guard-test--with-root 'worktree
    (let* ((elpaca--event-log nil)
           (e (+elpaca-guard-test--order nil))
           (build (expand-file-name "elpaca/builds/p/" root)))
      (make-directory build t)
      (setf (elpaca<-build-dir e) build
            (elpaca<-source-dir e) (expand-file-name "elpaca/sources/p/" root)
            (elpaca<-build-steps e) (elpaca-build-steps e))
      (elpaca-continue e)
      (should (eq 'failed (elpaca<-status e)))
      (should (seq-some (lambda (event)
                          (string-match-p
                           "builds, updates and deletes no package here: .* \
is a linked git worktree"
                           (or (plist-get (elpaca-event<-payload event) :info)
                               "")))
                        elpaca--event-log))
      (should-error (elpaca--delete e) :type 'user-error)
      (should (file-directory-p build)))))

(ert-deftest +elpaca-guard-test-bad-links ()
  "Name each build link that dangles or points outside elpaca/sources/."
  (+elpaca-guard-test--with-root 'linked
    (let ((sources (expand-file-name "sources/p/" shared))
          (builds (expand-file-name "builds/p/" shared))
          (outside (make-temp-file "elpaca-outside-")))
      (unwind-protect
          (progn
            (make-directory sources t)
            (make-directory builds t)
            (dolist (file '("good.el" "own.el"))
              (write-region "" nil (expand-file-name file sources)))
            (make-symbolic-link (expand-file-name "good.el"
                                                  (file-truename sources))
                                (expand-file-name "good.el" builds))
            (should-not (+config-elpaca-bad-links root))
            (make-symbolic-link (expand-file-name "elpaca/sources/p/own.el"
                                                  root)
                                (expand-file-name "own.el" builds))
            (make-symbolic-link "/nonexistent/gone.el"
                                (expand-file-name "gone.el" builds))
            (make-symbolic-link outside (expand-file-name "out.el" builds))
            (let ((bad (+config-elpaca-bad-links root)))
              (should (equal '("builds/p/gone.el" "builds/p/out.el"
                               "builds/p/own.el")
                             (mapcar (lambda (line) (car (split-string line)))
                                     bad)))
              (should (string-match-p "gone.el -> /nonexistent/gone.el \
does not resolve" (nth 0 bad)))
              (should (string-match-p "is outside" (nth 1 bad)))
              (should (string-match-p "is outside" (nth 2 bad)))))
        (delete-file outside)))))

(provide 'config-elpaca-guard-test)
;;; config-elpaca-guard-test.el ends here
