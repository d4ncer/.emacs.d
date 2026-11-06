;;; agent-shell-pstack-walkthrough-test.el --- Tests for walkthrough links -*- lexical-binding: t; -*-

;;; Commentary:
;; Result records, walkthrough links and review worktrees against
;; synthetic repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack-walkthrough)

(defmacro +agent-shell-pstack-walkthrough-test--with (&rest body)
  "Run BODY in a scratch repository with a fresh panel directory."
  (declare (indent 0))
  `(let* ((scratch (file-name-as-directory
                    (file-truename (make-temp-file "pstack-walkthrough-" t))))
          (repo (expand-file-name "repo/" scratch))
          (gitconfig (expand-file-name "gitconfig" scratch))
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL="
                                                     gitconfig)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (+agent-shell-pstack-panel-directory
           (expand-file-name "reports/" scratch))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" scratch))
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-pstack-walkthrough-opens-file
           (expand-file-name "opens.eld" scratch))
          (+agent-shell-pstack-walkthrough--trees
           (make-hash-table :test #'equal))
          (find-file-hook nil)
          (before (buffer-list)))
     (with-temp-file gitconfig
       (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
               "[init]\n\tdefaultBranch = main\n"
               "[commit]\n\tgpgsign = false\n"))
     (make-directory repo)
     (+agent-shell-pstack-walkthrough-test--git repo "init" "-q")
     (unwind-protect
         (progn
           (+agent-shell-pstack-walkthrough-links-mode 1)
           ,@body)
       (+agent-shell-pstack-walkthrough-links-mode -1)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory scratch t))))

(defun +agent-shell-pstack-walkthrough-test--git (directory &rest args)
  "Return the trimmed output of git ARGS in DIRECTORY, or signal."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eql 0 (apply #'process-file "git" nil t nil args))
        (error "git %s: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +agent-shell-pstack-walkthrough-test--commit (directory file text)
  "Commit FILE holding TEXT in DIRECTORY and return the commit."
  (let ((path (expand-file-name file directory)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert text)))
  (+agent-shell-pstack-walkthrough-test--git directory "add" "-A")
  (+agent-shell-pstack-walkthrough-test--git directory "commit" "-q" "-m"
                                             (concat "Change " file))
  (+agent-shell-pstack-walkthrough-test--git directory "rev-parse" "HEAD"))

(defun +agent-shell-pstack-walkthrough-test--record (panel role &rest fields)
  "Write PANEL's result record for ROLE with FIELDS and return it."
  (let ((file (expand-file-name (format "%s/%s.result.json" panel role)
                                +agent-shell-pstack-panel-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (json-serialize
               (append (list :version 1 :panel panel :role role) fields))))
    (+agent-shell-pstack-walkthrough--read file)))

(defun +agent-shell-pstack-walkthrough-test--report (panel role text)
  "Write PANEL's report for ROLE holding TEXT and visit it."
  (let ((file (expand-file-name (format "%s/%s.md" panel role)
                                +agent-shell-pstack-panel-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    (find-file-noselect file)))

(defun +agent-shell-pstack-walkthrough-test--links (buffer)
  "Return the text of each walkthrough link in BUFFER, in order."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (buffer-substring-no-properties (overlay-start overlay)
                                              (overlay-end overlay)))
            (sort (seq-filter
                   (lambda (overlay)
                     (overlay-get overlay '+agent-shell-pstack-walkthrough))
                   (overlays-in (point-min) (point-max)))
                  (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun +agent-shell-pstack-walkthrough-test--follow (result path line)
  "Follow RESULT's PATH at LINE and return the buffer once it shows."
  (let (shown)
    (+agent-shell-pstack-walkthrough-follow
     result path line (lambda (buffer) (setq shown buffer)))
    (let ((deadline (+ (float-time) 30)))
      (while (and (not shown) (< (float-time) deadline))
        (accept-process-output nil 0.02)))
    (should shown)
    shown))

(defun +agent-shell-pstack-walkthrough-test--opens ()
  "Return the entries of the opens log."
  (with-temp-buffer
    (insert-file-contents +agent-shell-pstack-walkthrough-opens-file)
    (mapcar #'read (split-string (buffer-string) "\n" t))))

(ert-deftest +agent-shell-pstack-walkthrough-test-contracts ()
  "Every code-writing contract asks for a walkthrough with path:line claims."
  (let ((shapes (eval (car (get '+agent-shell-pstack-panel-shapes
                                'standard-value))
                      t)))
    (dolist (contract (list (plist-get (alist-get 'arena shapes) :contract)
                            (plist-get (alist-get 'delegate shapes) :contract)
                            (plist-get (alist-get 'delegate shapes)
                                       :sandbox-contract)))
      (should (string-search "## Walkthrough" contract))
      (should (string-search "path:line" contract))
      (should (string-search "(recorded)" contract))
      (should (string-search "(inferred)" contract)))
    (let ((sandboxed (plist-get (alist-get 'delegate shapes) :sandbox-contract)))
      (should (< (string-search "## Walkthrough" sandboxed)
                 (string-search "```pstack-commit" sandboxed))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-arena-records ()
  "Write a result record for each arena candidate with a commit, and no other."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((base (+agent-shell-pstack-walkthrough-test--commit
                  repo "lisp/a.el" "one\n"))
           (commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "two\n"))
           (manifest (+agent-shell-arena-git--manifest
                      :id "a1b2c3d4e5f6-0000abcd"
                      :git-dir (expand-file-name ".git" repo)
                      :toplevel repo :base base))
           (panel (+agent-shell-pstack-panel--make
                   :id "a1b2c3d4e5f6" :shape 'arena :arena manifest
                   :members
                   (list (+agent-shell-pstack-panel--make-member
                          :role "opus" :kind 'candidate
                          :result (list :commit commit :checkout "/tmp/c/"))
                         (+agent-shell-pstack-panel--make-member
                          :role "codex" :kind 'candidate
                          :result (list :commit commit
                                        :bundle "/tmp/r/results.bundle"
                                        :bundle-ref
                                        "refs/pstack-arena/h/codex"))
                         (+agent-shell-pstack-panel--make-member
                          :role "empty" :kind 'candidate
                          :result (list :error "nothing"))
                         (+agent-shell-pstack-panel--make-member
                          :role "judge" :kind 'judge))))
           (file (lambda (role)
                   (expand-file-name (format "a1b2c3d4e5f6/%s.result.json"
                                             role)
                                     +agent-shell-pstack-panel-directory))))
      (+agent-shell-pstack-panel--write-results panel)
      (let ((opus (+agent-shell-pstack-walkthrough--read
                   (funcall file "opus")))
            (codex (+agent-shell-pstack-walkthrough--read
                    (funcall file "codex"))))
        (should (equal "a1b2c3d4e5f6" (plist-get opus :panel)))
        (should (equal "opus" (plist-get opus :role)))
        (should (equal commit (plist-get opus :commit)))
        (should (equal base (plist-get opus :base)))
        (should (equal repo (plist-get opus :toplevel)))
        (should (equal (file-truename (expand-file-name ".git/" repo))
                       (file-truename (file-name-as-directory
                                       (plist-get opus :git_dir)))))
        (should (equal "/tmp/c/" (plist-get opus :worktree)))
        (should-not (plist-get opus :bundle))
        (should (equal "/tmp/r/results.bundle" (plist-get codex :bundle)))
        (should (equal "refs/pstack-arena/h/codex"
                       (plist-get codex :bundle_ref))))
      (should-not (file-exists-p (funcall file "empty")))
      (should-not (file-exists-p (funcall file "judge"))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-links ()
  "Link only citations of paths in the result commit's tree."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "one\ntwo\nthree\n")))
      (+agent-shell-pstack-walkthrough-test--record
       "p1" "builder" :git_dir (expand-file-name ".git" repo)
       :toplevel repo :commit commit)
      (let ((report (+agent-shell-pstack-walkthrough-test--report
                     "p1" "builder"
                     "## Walkthrough\n\n1. lisp/a.el:2 adds it (recorded).\n\
2. See `lisp/a.el:1-3` and ./lisp/a.el:3.\n3. lisp/missing.el:4, lisp:1, \
http://example.com:80 and /abs/lisp/a.el:2 are not links.\n")))
        (with-current-buffer report
          (should +agent-shell-pstack-walkthrough-mode))
        (should (equal '("lisp/a.el:2" "lisp/a.el:1-3" "./lisp/a.el:3")
                       (+agent-shell-pstack-walkthrough-test--links report)))
        (with-current-buffer report
          (goto-char (point-min))
          (search-forward "lisp/a.el:2")
          (let ((button (button-at (match-beginning 0))))
            (should (eq '+agent-shell-pstack-walkthrough-link
                        (button-get button 'face)))
            (should (button-get button 'follow-link))
            (should (equal (list commit "lisp/a.el" 2)
                           (let ((link (button-get
                                        button
                                        '+agent-shell-pstack-walkthrough-link)))
                             (list (plist-get (car link) :commit)
                                   (nth 1 link) (nth 2 link)))))))
        (should (eq 'link (face-attribute '+agent-shell-pstack-walkthrough-link
                                          :inherit)))
        (dolist (key (list (kbd "RET") (kbd "<return>") [mouse-1]))
          (should (eq #'push-button
                      (lookup-key +agent-shell-pstack-walkthrough-link-map
                                  key)))))
      (let ((other (+agent-shell-pstack-walkthrough-test--report
                    "p1" "judge" "lisp/a.el:2\n")))
        (with-current-buffer other
          (should-not +agent-shell-pstack-walkthrough-mode))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-existing-checkouts ()
  "Open the file in the role's worktree, or the lead's checkout at the commit."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "one\ntwo\nthree\n"))
           (worktree (expand-file-name "wt/" scratch)))
      (+agent-shell-pstack-walkthrough-test--git
       repo "worktree" "add" "-q" "--detach" (directory-file-name worktree)
       commit)
      (let* ((result (+agent-shell-pstack-walkthrough-test--record
                      "p2" "builder" :git_dir (expand-file-name ".git" repo)
                      :toplevel repo :commit commit :worktree worktree))
             (report (+agent-shell-pstack-walkthrough-test--report
                      "p2" "builder" "Read lisp/a.el:2 first.\n")))
        (switch-to-buffer report)
        (goto-char (point-min))
        (search-forward "lisp/a.el")
        (push-button (match-beginning 0))
        (should (equal (expand-file-name "lisp/a.el" worktree)
                       (buffer-file-name (window-buffer (selected-window)))))
        (with-current-buffer (window-buffer (selected-window))
          (should (= 2 (line-number-at-pos)))
          (should (looking-at "two")))
        (should-not (file-exists-p
                     (+agent-shell-pstack-walkthrough--review-directory
                      "p2" "builder")))
        (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                       (plist-put (copy-sequence result) :worktree nil)
                       "lisp/a.el" 3)))
          (should (equal (expand-file-name "lisp/a.el" repo)
                         (buffer-file-name buffer)))
          (with-current-buffer buffer (should (looking-at "three"))))
        (should (equal '(("p2" "builder" "lisp/a.el")
                         ("p2" "builder" "lisp/a.el"))
                       (mapcar #'cdr
                               (+agent-shell-pstack-walkthrough-test--opens))))
        (should (string-match-p
                 "\\`[0-9-]+T[0-9:]+[+-][0-9]+\\'"
                 (car (car (+agent-shell-pstack-walkthrough-test--opens)))))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-review-worktree ()
  "Open a review worktree at the commit once the role's worktree is gone."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "result\nline\n"))
           (worktree (expand-file-name "wt/" scratch)))
      (+agent-shell-pstack-walkthrough-test--git
       repo "worktree" "add" "-q" "--detach" (directory-file-name worktree)
       commit)
      (+agent-shell-pstack-walkthrough-test--git
       repo "worktree" "remove" (directory-file-name worktree))
      (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el"
                                                    "lead\nmoved\n")
      (let* ((result (+agent-shell-pstack-walkthrough-test--record
                      "p3" "builder" :git_dir (expand-file-name ".git" repo)
                      :toplevel repo :commit commit :worktree worktree))
             (review (+agent-shell-pstack-walkthrough--review-directory
                      "p3" "builder"))
             (buffer (+agent-shell-pstack-walkthrough-test--follow
                      result "lisp/a.el" 2)))
        (should (equal (expand-file-name "lisp/a.el" review)
                       (buffer-file-name buffer)))
        (should (equal commit (+agent-shell-pstack-walkthrough-test--git
                               review "rev-parse" "HEAD")))
        (with-current-buffer buffer
          (should (equal "result\nline\n" (buffer-string)))
          (should (= 2 (line-number-at-pos))))
        (should (equal "lead\nmoved\n"
                       (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "lisp/a.el" repo))
                         (buffer-string))))
        (should (eq buffer (+agent-shell-pstack-walkthrough-test--follow
                            result "lisp/a.el" 1)))
        (should (= 2 (length (+agent-shell-pstack-walkthrough-test--opens))))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-bundle ()
  "Fetch a commit the repository lacks from its results bundle."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((host (expand-file-name "host/" scratch))
           (bundle (expand-file-name "results.bundle" scratch)))
      (+agent-shell-pstack-walkthrough-test--git
       scratch "clone" "-q" repo (directory-file-name host))
      (let ((base (+agent-shell-pstack-walkthrough-test--git
                   host "rev-parse" "HEAD"))
            (commit (+agent-shell-pstack-walkthrough-test--commit
                     host "lisp/b.el" "from the host\n")))
        (+agent-shell-pstack-walkthrough-test--git
         host "update-ref" "refs/pstack-arena/h1/codex" commit)
        (+agent-shell-pstack-walkthrough-test--git
         host "bundle" "create" "-q" bundle "refs/pstack-arena/h1/codex"
         (concat "^" base))
        (should-error (+agent-shell-pstack-walkthrough-test--git
                       repo "cat-file" "-e" commit))
        (+agent-shell-pstack-walkthrough-test--record
         "p4" "codex" :git_dir (expand-file-name ".git" repo)
         :toplevel repo :base base :commit commit :bundle bundle
         :bundle_ref "refs/pstack-arena/h1/codex")
        (let ((report (+agent-shell-pstack-walkthrough-test--report
                       "p4" "codex" "lisp/b.el:1 is new.\n")))
          (should (equal '("lisp/b.el:1")
                         (+agent-shell-pstack-walkthrough-test--links
                          report)))
          (should (equal commit (+agent-shell-pstack-walkthrough-test--git
                                 repo "rev-parse"
                                 "refs/pstack/review/p4/codex")))
          (let ((buffer (with-current-buffer report
                          (+agent-shell-pstack-walkthrough-test--follow
                           +agent-shell-pstack-walkthrough--result
                           "lisp/b.el" 1))))
            (with-current-buffer buffer
              (should (equal "from the host\n" (buffer-string))))))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-vanished ()
  "Say which panel and role lost its commit when nothing holds it."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (+agent-shell-pstack-walkthrough-test--git repo "checkout" "-q" "-b" "gone")
    (let ((commit (+agent-shell-pstack-walkthrough-test--commit
                   repo "lisp/a.el" "lost\n")))
      (+agent-shell-pstack-walkthrough-test--git repo "checkout" "-q" "main")
      (+agent-shell-pstack-walkthrough-test--git repo "branch" "-q" "-D" "gone")
      (+agent-shell-pstack-walkthrough-test--git
       repo "reflog" "expire" "--expire=now" "--all")
      (+agent-shell-pstack-walkthrough-test--git repo "gc" "-q" "--prune=now")
      (let* ((result (+agent-shell-pstack-walkthrough-test--record
                      "p5" "builder" :git_dir (expand-file-name ".git" repo)
                      :toplevel repo :commit commit
                      :bundle (expand-file-name "missing.bundle" scratch)
                      :bundle_ref "refs/heads/x"))
             (err (should-error (+agent-shell-pstack-walkthrough-follow
                                 result "lisp/a.el" 1)
                                :type 'user-error)))
        (should (string-search "Panel p5, role builder: result" (cadr err)))
        (should (string-search "is gone" (cadr err)))
        (should-not (file-exists-p
                     (+agent-shell-pstack-walkthrough--review-directory
                      "p5" "builder")))
        (should-not (file-exists-p
                     +agent-shell-pstack-walkthrough-opens-file))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-cleanup ()
  "Cleaning a panel's arena removes its review worktrees and refs."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((base (+agent-shell-pstack-walkthrough-test--commit
                  repo "lisp/a.el" "base\n"))
           (commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "result\n"))
           (directory (expand-file-name "p6-0000abcd/"
                                        +agent-shell-pstack-arena-directory))
           (manifest (+agent-shell-arena-git--manifest
                      :id "p6-0000abcd" :directory directory
                      :git-dir (expand-file-name ".git" repo)
                      :toplevel repo :base base))
           (ended nil))
      (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "moved\n")
      (make-directory directory t)
      (dolist (role '("opus" "codex"))
        (+agent-shell-pstack-walkthrough-test--follow
         (+agent-shell-pstack-walkthrough-test--record
          "p6" role :git_dir (expand-file-name ".git" repo)
          :toplevel repo :base base :commit commit)
         "lisp/a.el" 1))
      (+agent-shell-pstack-walkthrough-test--git
       repo "update-ref" "refs/pstack/review/p6/codex" commit)
      (with-temp-file (expand-file-name
                       "lisp/a.el"
                       (+agent-shell-pstack-walkthrough--review-directory
                        "p6" "codex"))
        (insert "edited\n"))
      (should (+agent-shell-pstack-walkthrough-reviews-p "p6"))
      (+agent-shell-pstack-arena--cleanup manifest
                                          (lambda (error) (setq ended
                                                                (list error))))
      (let ((deadline (+ (float-time) 30)))
        (while (and (not ended) (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (equal '(nil) ended))
      (should-not (file-exists-p
                   (+agent-shell-pstack-walkthrough--review-directory
                    "p6" "opus")))
      (should (file-exists-p
               (+agent-shell-pstack-walkthrough--review-directory
                "p6" "codex")))
      (should (equal "" (+agent-shell-pstack-walkthrough-test--git
                         repo "for-each-ref" "refs/pstack/review/")))
      (should-not (file-exists-p directory)))))

(provide 'agent-shell-pstack-walkthrough-test)
;;; agent-shell-pstack-walkthrough-test.el ends here
