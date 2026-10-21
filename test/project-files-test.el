;;; project-files-test.el --- Large-project finder tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise actual child processes and Consult source transitions.

;;; Code:

(require 'ert)
(require '+project-files)
(require 'consult)

(defmacro +project-files-test--with-project (&rest body)
  "Evaluate BODY in a temporary Git project with isolated snapshot storage."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "file-project-" t)))
          (default-directory root)
          (+project-files-directory (expand-file-name "cache/" root))
          (+project-files--states (make-hash-table :test #'equal))
          (projectile-projects-cache (make-hash-table :test #'equal))
          (projectile-projects-cache-time (make-hash-table :test #'equal))
          (projectile-project-root-cache (make-hash-table :test #'equal))
          (projectile-git-use-fd t)
          (projectile-globally-ignored-directories '("cache" ".git"))
          (projectile-globally-ignored-file-suffixes '("elc"))
          (state (+project-files--state root)))
     (unwind-protect
         (progn
           (unless (zerop (call-process "git" nil nil nil "init" "-q"))
             (error "Cannot initialize fixture"))
           ,@body)
       (when (process-live-p (+project-files--state-process state))
         (delete-process (+project-files--state-process state)))
       (when (timerp (+project-files--state-timeout state))
         (cancel-timer (+project-files--state-timeout state)))
       (delete-directory root t))))

(defun +project-files-test--write (root name &optional contents)
  "Create NAME in ROOT with optional CONTENTS."
  (let ((path (expand-file-name name root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert (or contents "")))
    path))

(defun +project-files-test--wait (predicate)
  "Wait at most ten seconds for PREDICATE, pumping process output."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun +project-files-test--manifest (state)
  "Read STATE's snapshot records."
  (let ((coding-system-for-read 'utf-8-unix))
    (with-temp-buffer
      (insert-file-contents (+project-files--state-manifest state))
      (split-string (buffer-string) "\0" t))))

(ert-deftest +project-files-test-highlight-preserves-separate-term-colours ()
  (let ((candidate (copy-sequence "ui/Button.test.ts")))
    (funcall (+project-files--highlight "button test") candidate)
    (should (memq 'orderless-match-face-0
                  (ensure-list (get-text-property 3 'face candidate))))
    (should (memq 'orderless-match-face-1
                  (ensure-list (get-text-property 10 'face candidate))))
    (should-not (get-text-property 0 'face candidate))
    (should (equal (substring-no-properties candidate) "ui/Button.test.ts"))))

(ert-deftest +project-files-test-highlight-does-not-interpret-regexp-characters ()
  (let ((candidate (copy-sequence "button[1]/button1.test.ts")))
    (funcall (+project-files--highlight "button[1] test") candidate)
    (should (memq 'orderless-match-face-0
                  (ensure-list (get-text-property 0 'face candidate))))
    (should-not (get-text-property 10 'face candidate))
    (should (memq 'orderless-match-face-1
                  (ensure-list (get-text-property 18 'face candidate))))))

(ert-deftest +project-files-test-watch-limit-bounds-the-work ()
  (let* ((projectile-watch-max-directories 2)
         (files '("a/one" "b/two" "c/three" "d/four"))
         (original (symbol-function 'projectile--directory-ancestors))
         (visited 0))
    (cl-letf (((symbol-function 'projectile--directory-ancestors)
               (lambda (file) (cl-incf visited) (funcall original file))))
      (should (= 3 (length (+project-files--watch-directories nil "/tmp/" files))))
      (should (= 2 visited)))))

(ert-deftest +project-files-test-small-watch-set-is-complete ()
  (let ((projectile-watch-max-directories 100))
    (should (equal (sort (+project-files--watch-directories
                         nil "/tmp/" '("a/b/file" "a/other" "top")) #'string<)
                   '("/tmp/" "/tmp/a/" "/tmp/a/b/")))))

(ert-deftest +project-files-test-router-preserves-small-and-remote-finder ()
  (let ((+project-files-threshold 3)
        (+project-files-large-projects nil)
        (+project-files-git-index-threshold 10000000)
        (projectile-projects-cache (make-hash-table :test #'equal))
        calls)
    (cl-letf (((symbol-function 'projectile-acquire-root) (lambda () "/tmp/"))
              ((symbol-function 'projectile-find-file)
               (lambda (arg) (push (list 'ordinary arg) calls)))
              ((symbol-function '+project-find-file-fast)
               (lambda (arg) (push (list 'fast arg) calls))))
      (+project-find-file t)
      (puthash "/tmp/" '("a" "b" "c") projectile-projects-cache)
      (should (eq t (+project-files--large-p "/tmp/")))
      (+project-find-file nil)
      (should (equal calls '((fast nil) (ordinary t)))))
    (should-not (+project-files--large-p "/ssh:host:/repo/"))))

(ert-deftest +project-files-test-worktree-index-detection ()
  (+project-files-test--with-project
    (let ((git (make-temp-file "worktree-git-" t))
          (+project-files-git-index-threshold 10))
      (unwind-protect
          (progn
            (delete-directory (expand-file-name ".git" root) t)
            (+project-files-test--write root ".git" (concat "gitdir: " git "\n"))
            (+project-files-test--write git "index" (make-string 20 ?x))
            (should (equal git (+project-files--git-directory root)))
            (should (+project-files--large-p root)))
        (delete-directory git t)))))

(ert-deftest +project-files-test-worker-inherits-discovery-and-preserves-paths ()
  (+project-files-test--with-project
    (+project-files-test--write root ".gitignore" "ignored/\n")
    (+project-files-test--write root ".projectile" "+src\n")
    (+project-files-test--write root "src/button.ts")
    (+project-files-test--write root "src/space 日本語\nbutton.ts")
    (+project-files-test--write root "src/compiled.elc")
    (+project-files-test--write root "ignored/button.ts")
    (+project-files-test--write root "outside/button.ts")
    (+project-files--refresh state t)
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (should-not (+project-files--state-error state))
    (should (equal (sort (+project-files-test--manifest state) #'string<)
                   '("src/button.ts" "src/space 日本語\nbutton.ts")))
    (should (= 2 (+project-files--state-count state)))
    ;; A new session can recover metadata and reuse a fresh snapshot.
    (let ((+project-files--states (make-hash-table :test #'equal)))
      (let ((loaded (+project-files--state root)))
        (should (= 2 (+project-files--state-count loaded)))
        (+project-files--refresh loaded)
        (should-not (+project-files--state-process loaded))))))

(ert-deftest +project-files-test-signature-rebuilds-after-ignore-change ()
  (+project-files-test--with-project
    (+project-files-test--write root "button.ts")
    (+project-files--refresh state t)
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (+project-files-test--write root ".gitignore" "button.ts\n")
    (+project-files--refresh state)
    (should (+project-files--state-process state))
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (should-not (member "button.ts" (+project-files-test--manifest state)))))

(ert-deftest +project-files-test-failed-index-keeps-previous-snapshot ()
  (+project-files-test--with-project
    (+project-files-test--write root "button.ts")
    (+project-files--refresh state t)
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (let ((before (+project-files-test--manifest state))
          (projectile-git-use-fd nil)
          (projectile-git-command "false"))
      (+project-files--refresh state t)
      (+project-files-test--wait
       (lambda () (not (+project-files--state-process state))))
      (should (+project-files--state-error state))
      (should (equal before (+project-files-test--manifest state)))
      (kill-buffer " *Project files index*"))))

(ert-deftest +project-files-test-save-overlay-honors-ignores-and-deletes ()
  (+project-files-test--with-project
    (+project-files-test--write root ".gitignore" "ignored/\n")
    (let ((file (+project-files-test--write root "new/button.ts"))
          (ignored (+project-files-test--write root "ignored/button.ts")))
      (+project-files--remember-file file 'add)
      (+project-files--remember-file ignored 'add)
      (should (eq 'add (gethash "new/button.ts"
                                (+project-files--state-overlay state))))
      (should-not (gethash "ignored/button.ts"
                           (+project-files--state-overlay state)))
      (+project-files--remember-file file 'delete)
      (should (eq 'delete (gethash "new/button.ts"
                                   (+project-files--state-overlay state)))))))

(ert-deftest +project-files-test-overlay-obeys-project-scope ()
  (+project-files-test--with-project
    (+project-files-test--write root ".projectile" "+src\n")
    (let ((inside (+project-files-test--write root "src/button.ts"))
          (outside (+project-files-test--write root "elsewhere/button.ts")))
      (+project-files--remember-file inside 'add)
      (+project-files--remember-file outside 'add)
      (should (eq 'add (gethash "src/button.ts"
                                (+project-files--state-overlay state))))
      (should-not (gethash "elsewhere/button.ts"
                           (+project-files--state-overlay state))))))

(ert-deftest +project-files-test-stable-refresh-retires-overlay ()
  (+project-files-test--with-project
    (+project-files-test--write root "button.ts")
    (puthash "old/button.ts" 'add (+project-files--state-overlay state))
    (+project-files--refresh state t)
    ;; Keep changes made while the index was running.
    (+project-files--remember-file
     (+project-files-test--write root "during/button.ts") 'add)
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (should (gethash "during/button.ts" (+project-files--state-overlay state)))
    (+project-files--refresh state t)
    (+project-files-test--wait
     (lambda () (not (+project-files--state-process state))))
    (should (= 0 (hash-table-count (+project-files--state-overlay state))))))

(ert-deftest +project-files-test-source-refresh-short-input-and-more ()
  (+project-files-test--with-project
    (+project-files-test--write
     root "cache/test.nul" "src/button.ts\0other/button.ts\0")
    (setf (+project-files--state-manifest state)
          (expand-file-name "cache/test.nul" root))
    (let ((+project-files-candidate-limit 1)
          (candidates nil) (refreshes 0) source)
      (with-temp-buffer
        (setq source
              (funcall (+project-files--source
                        state '("recent/button.ts") ""
                        (executable-find "rg"))
                       (lambda (action)
                         (pcase action
                           ('flush (setq candidates nil))
                           ('refresh (cl-incf refreshes))
                           ((pred consp)
                            (setq candidates (append candidates action)))))))
        (unwind-protect
            (progn
              (funcall source 'setup)
              (should (equal candidates
                             '("recent/button.ts")))
              (funcall source "button")
              (+project-files-test--wait
               (lambda () (equal candidates
                                 '("src/button.ts"))))
              ;; A late response from an obsolete query cannot replace results,
              ;; including when its JSON arrives in several chunks.
              (let* ((process (process-get (get-process "project-files-search")
                                           'channel))
                     (filter (process-filter process)))
                (funcall filter process "{\"id\":-1,\"candidates\":[\"bad\"")
                (funcall filter process "]}\n")
                (should (equal candidates '("src/button.ts"))))
              (funcall +project-files--more-function)
              (+project-files-test--wait (lambda () (= 2 (length candidates))))
              (funcall source "b")
              (should (equal candidates
                             '("recent/button.ts")))
              (funcall source "newbutton")
              (+project-files-test--write root "cache/test.nul"
                                         "src/newbutton.ts\0")
              (+project-files-test--wait
               (lambda () (equal candidates
                                 '("src/newbutton.ts"))))
              (should (> refreshes 0)))
          (funcall source 'destroy)
          (should-not (directory-files +project-files-directory nil "^query-")))))))

(provide 'project-files-test)
;;; project-files-test.el ends here
