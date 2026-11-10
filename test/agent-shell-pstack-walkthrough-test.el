;;; agent-shell-pstack-walkthrough-test.el --- Tests for walkthrough links -*- lexical-binding: t; -*-

;;; Commentary:
;; Result records, walkthrough links and reviews in the lead's checkout
;; against synthetic repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack-walkthrough)
(require '+agent-shell-pstack-panels)

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
          (+agent-shell-pstack-walkthrough-review-file
           (expand-file-name "review.eld" scratch))
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

(defun +agent-shell-pstack-walkthrough-test--wait (predicate)
  "Wait until PREDICATE returns non-nil, and return its value."
  (let ((deadline (+ (float-time) 30))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should value)
    value))

(defun +agent-shell-pstack-walkthrough-test--branch-result (repo panel text)
  "Commit lisp/a.el holding TEXT on PANEL's delegate branch in REPO.
Leave REPO on main and return the result record."
  (+agent-shell-pstack-walkthrough-test--git
   repo "switch" "-q" "-c" (concat "pstack/delegate/" panel))
  (let ((commit (+agent-shell-pstack-walkthrough-test--commit
                 repo "lisp/a.el" text)))
    (+agent-shell-pstack-walkthrough-test--git repo "switch" "-q" "main")
    (+agent-shell-pstack-walkthrough-test--record
     panel "builder" :git_dir (expand-file-name ".git" repo)
     :toplevel repo :commit commit)))

(defun +agent-shell-pstack-walkthrough-test--head (repo)
  "Return REPO's branch, or its commit when detached."
  (let ((branch (+agent-shell-pstack-walkthrough-test--git
                 repo "branch" "--show-current")))
    (if (string-empty-p branch)
        (+agent-shell-pstack-walkthrough-test--git repo "rev-parse" "HEAD")
      branch)))

(defvar-local +agent-shell-pstack-walkthrough-test--status nil
  "A fake agent shell's status.")

(defun +agent-shell-pstack-walkthrough-test--shell (name directory status)
  "Return a fake agent shell NAME working in DIRECTORY with STATUS."
  (with-current-buffer (generate-new-buffer name)
    (setq major-mode 'agent-shell-mode
          default-directory (file-name-as-directory directory)
          +agent-shell-pstack-walkthrough-test--status status)
    (make-directory default-directory t)
    (current-buffer)))

(defmacro +agent-shell-pstack-walkthrough-test--with-shells (&rest body)
  "Run BODY with agent shells read from fake shell buffers."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-shell-cwd)
              (lambda () default-directory))
             ((symbol-function 'agent-shell-status)
              (cl-function
               (lambda (&key shell-buffer)
                 (buffer-local-value
                  '+agent-shell-pstack-walkthrough-test--status
                  shell-buffer)))))
     ,@body))

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
  "Open an unchanged file in the lead's checkout, else the role's worktree."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((commit (+agent-shell-pstack-walkthrough-test--commit
                    repo "lisp/a.el" "one\ntwo\nthree\n"))
           (worktree (expand-file-name "wt/" scratch)))
      (+agent-shell-pstack-walkthrough-test--git
       repo "worktree" "add" "-q" "--detach" (directory-file-name worktree)
       commit)
      (+agent-shell-pstack-walkthrough-test--commit repo "lisp/b.el" "lead\n")
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (error "Asked %s" prompt))))
        (let* ((result (+agent-shell-pstack-walkthrough-test--record
                        "p2" "builder" :git_dir (expand-file-name ".git" repo)
                        :toplevel repo :commit commit :worktree worktree))
               (report (+agent-shell-pstack-walkthrough-test--report
                        "p2" "builder" "Read lisp/a.el:2 first.\n")))
          (switch-to-buffer report)
          (goto-char (point-min))
          (search-forward "lisp/a.el")
          (push-button (match-beginning 0))
          (should (equal (expand-file-name "lisp/a.el" repo)
                         (buffer-file-name (window-buffer (selected-window)))))
          (with-current-buffer (window-buffer (selected-window))
            (should (= 2 (line-number-at-pos)))
            (should (looking-at "two")))
          (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                                 repo)))
          (with-temp-file (expand-file-name "lisp/a.el" repo)
            (insert "edited\n"))
          (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                         result "lisp/a.el" 3)))
            (should (equal (expand-file-name "lisp/a.el" worktree)
                           (buffer-file-name buffer)))
            (with-current-buffer buffer (should (looking-at "three"))))
          (+agent-shell-pstack-walkthrough-test--git repo "checkout" "lisp/a.el")
          (+agent-shell-pstack-walkthrough-test--git
           repo "update-index" "--cacheinfo"
           (concat "100644,"
                   (+agent-shell-pstack-walkthrough-test--git
                    repo "hash-object" "-w" "lisp/b.el")
                   ",lisp/a.el"))
          (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                         result "lisp/a.el" 1)))
            (should (equal (expand-file-name "lisp/a.el" worktree)
                           (buffer-file-name buffer))))
          (should (equal '(("p2" "builder" "lisp/a.el")
                           ("p2" "builder" "lisp/a.el")
                           ("p2" "builder" "lisp/a.el"))
                         (mapcar #'cdr
                                 (+agent-shell-pstack-walkthrough-test--opens))))
          (should (string-match-p
                   "\\`[0-9-]+T[0-9:]+[+-][0-9]+\\'"
                   (car (car (+agent-shell-pstack-walkthrough-test--opens)))))
          (should-not (file-exists-p
                       +agent-shell-pstack-walkthrough-review-file)))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-switch-branch ()
  "Ask before switching the checkout to the delegate branch at the result."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((result (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p3" "result\nline\n"))
           (short (substring (plist-get result :commit) 0 7))
           prompts)
      (with-temp-file (expand-file-name "notes.txt" repo) (insert "untracked\n"))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (push prompt prompts) t)))
        (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                       result "lisp/a.el" 2)))
          (should (equal (list (format "Switch %s from main to panel p3's \
builder result %s (branch pstack/delegate/p3)? "
                                       (abbreviate-file-name repo) short))
                         prompts))
          (should (equal (expand-file-name "lisp/a.el" repo)
                         (buffer-file-name buffer)))
          (with-current-buffer buffer
            (should (equal "result\nline\n" (buffer-string)))
            (should (= 2 (line-number-at-pos))))))
      (should (equal "pstack/delegate/p3"
                     (+agent-shell-pstack-walkthrough-test--head repo)))
      (should (equal (list (list :checkout repo :origin '(branch . "main")
                                 :panel "p3" :role "builder"
                                 :commit (plist-get result :commit)))
                     (+agent-shell-pstack-walkthrough--reviews)))
      (should (= 1 (length (split-string
                            (+agent-shell-pstack-walkthrough-test--git
                             repo "worktree" "list")
                            "\n")))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-switch-detached ()
  "Detach at the result when no free local branch points at it."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((result (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p3" "result\n"))
           (commit (plist-get result :commit))
           prompts)
      (+agent-shell-pstack-walkthrough-test--git
       repo "worktree" "add" "-q"
       (directory-file-name (expand-file-name "wt/" scratch))
       "pstack/delegate/p3")
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (push prompt prompts) t)))
        (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1))
      (should (string-suffix-p (format "builder result %s (detached)? "
                                       (substring commit 0 7))
                               (car prompts)))
      (should (equal commit (+agent-shell-pstack-walkthrough-test--head repo)))
      (should (equal '(branch . "main")
                     (plist-get (car (+agent-shell-pstack-walkthrough--reviews))
                                :origin))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-declined ()
  "Change nothing when the switch is declined."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n"))
          (shown nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (+agent-shell-pstack-walkthrough-follow
         result "lisp/a.el" 1 (lambda (buffer) (setq shown buffer))))
      (accept-process-output nil 0.2)
      (should-not shown)
      (should (equal "main" (+agent-shell-pstack-walkthrough-test--head repo)))
      (should-not (file-exists-p +agent-shell-pstack-walkthrough-review-file))
      (should-not (file-exists-p +agent-shell-pstack-walkthrough-opens-file)))))

(ert-deftest +agent-shell-pstack-walkthrough-test-refusals ()
  "Refuse to switch a checkout with tracked changes or an operation underway."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n")))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (error "Asked %s" prompt))))
        (with-temp-file (expand-file-name "lisp/a.el" repo) (insert "dirty\n"))
        (let ((err (should-error (+agent-shell-pstack-walkthrough-follow
                                  result "lisp/a.el" 1)
                                 :type 'user-error)))
          (should (equal (format "%s has uncommitted changes to tracked \
files: lisp/a.el; commit or stash them first" (abbreviate-file-name repo))
                         (cadr err))))
        (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "main\n")
        (with-temp-buffer
          (let ((default-directory repo))
            (should-not (eql 0 (process-file "git" nil t nil "cherry-pick"
                                             (plist-get result :commit))))))
        (let ((err (should-error (+agent-shell-pstack-walkthrough-follow
                                  result "lisp/a.el" 1)
                                 :type 'user-error)))
          (should (equal (format "%s has a cherry-pick in progress; finish \
or abort it first" (abbreviate-file-name repo))
                         (cadr err))))
        (+agent-shell-pstack-walkthrough-test--git repo "cherry-pick" "--abort")
        (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                               repo)))
        (should-not (file-exists-p
                     +agent-shell-pstack-walkthrough-review-file))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-return ()
  "Return a checkout to its branch or detached commit, refusing when dirty."
  (+agent-shell-pstack-walkthrough-test--with
    (let* ((base (+agent-shell-pstack-walkthrough-test--commit
                  repo "lisp/a.el" "base\n"))
           (result (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p3" "result\n"))
           (key (+agent-shell-pstack-walkthrough--key repo))
           (back (lambda ()
                   (+agent-shell-pstack-walkthrough-return key)
                   (+agent-shell-pstack-walkthrough-test--wait
                    (lambda ()
                      (not (file-exists-p
                            +agent-shell-pstack-walkthrough-review-file)))))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                       result "lisp/a.el" 1)))
          (with-temp-file (expand-file-name "lisp/a.el" repo)
            (insert "dirty\n"))
          (should (string-search
                   "uncommitted changes to tracked files: lisp/a.el"
                   (cadr (should-error
                          (+agent-shell-pstack-walkthrough-return key)
                          :type 'user-error))))
          (should (equal "pstack/delegate/p3"
                         (+agent-shell-pstack-walkthrough-test--head repo)))
          (should (+agent-shell-pstack-walkthrough--review key))
          (+agent-shell-pstack-walkthrough-test--git
           repo "checkout" "lisp/a.el")
          (with-current-buffer buffer
            (should (equal "result\n" (buffer-string))))
          (funcall back)
          (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                                 repo)))
          (with-current-buffer buffer
            (should (equal "base\n" (buffer-string))))))
      (+agent-shell-pstack-walkthrough-test--git
       repo "switch" "-q" "--detach" base)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1))
      (should (equal (cons 'detached base)
                     (plist-get (+agent-shell-pstack-walkthrough--review key)
                                :origin)))
      (with-current-buffer (find-file-noselect
                            (expand-file-name "lisp/a.el" repo))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (error "Asked"))))
          (call-interactively #'+agent-shell-pstack-walkthrough-return)))
      (+agent-shell-pstack-walkthrough-test--wait
       (lambda ()
         (not (file-exists-p +agent-shell-pstack-walkthrough-review-file))))
      (should (equal base (+agent-shell-pstack-walkthrough-test--head repo)))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args)
                     (push (apply #'format-message args) messages))))
          (call-interactively #'+agent-shell-pstack-walkthrough-return))
        (should (equal '("No checkout is switched to a walkthrough result")
                       messages))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-second-switch ()
  "Keep the original place when switching from one result to another."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((first (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "first\n"))
           (second (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p4" "second\n"))
           (key (+agent-shell-pstack-walkthrough--key repo))
           prompts)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (push prompt prompts) t)))
        (+agent-shell-pstack-walkthrough-test--follow first "lisp/a.el" 1)
        (let ((buffer (+agent-shell-pstack-walkthrough-test--follow
                       second "lisp/a.el" 1)))
          (with-current-buffer buffer
            (should (equal "second\n" (buffer-string))))))
      (should (string-prefix-p (format "Switch %s from pstack/delegate/p3 to \
panel p4's builder result" (abbreviate-file-name repo))
                               (car prompts)))
      (should (equal "pstack/delegate/p4"
                     (+agent-shell-pstack-walkthrough-test--head repo)))
      (should (equal (list (list :checkout key :origin '(branch . "main")
                                 :panel "p4" :role "builder"
                                 :commit (plist-get second :commit)))
                     (+agent-shell-pstack-walkthrough--reviews)))
      (+agent-shell-pstack-walkthrough-return key)
      (+agent-shell-pstack-walkthrough-test--wait
       (lambda ()
         (not (file-exists-p +agent-shell-pstack-walkthrough-review-file))))
      (should (equal "main" (+agent-shell-pstack-walkthrough-test--head repo))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-panels-view ()
  "Show the review near the top of the panels view, with R to return."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((result (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p3" "result\n"))
           (short (substring (plist-get result :commit) 0 7))
           (line (format "◆ REVIEWING  %s shows panel p3's builder result %s \
· R returns it to main" (abbreviate-file-name repo) short)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1))
      (save-window-excursion
        (+agent-shell-pstack-panels)
        (with-current-buffer +agent-shell-pstack-panels--buffer
          (goto-char (point-min))
          (forward-line 1)
          (should (equal line (buffer-substring-no-properties
                               (point) (line-end-position))))
          (should (eq '+agent-shell-pstack-panels-review
                      (get-text-property (point) 'font-lock-face)))
          (forward-line 1)
          (should (looking-at-p "\n"))
          (should (string-match-p "^  R +Return checkout +the checkout \
switched to a result$" (buffer-string)))
          (should (eq #'+agent-shell-pstack-panels-return (key-binding "R")))
          (goto-char (point-min))
          (search-forward "REVIEWING")
          (should (equal (list (cons "RET" "visit checkout")
                               (cons "R" "switch it back to main"))
                         (+agent-shell-pstack-panels--actions
                          (magit-current-section))))
          (+agent-shell-pstack-panels-keys)
          (with-current-buffer "*pstack panels keys*"
            (should (string-search "R        return a checkout switched to \
a result" (buffer-string))))
          (+agent-shell-pstack-panels-return)
          (+agent-shell-pstack-walkthrough-test--wait
           (lambda ()
             (not (file-exists-p
                   +agent-shell-pstack-walkthrough-review-file))))
          (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                                 repo)))
          (should-not (string-search "REVIEWING" (buffer-string)))
          (goto-char (point-min))
          (forward-line 1)
          (should (looking-at-p "\n")))))))

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
          (let ((buffer (cl-letf (((symbol-function 'yes-or-no-p)
                                   (lambda (_) t)))
                          (with-current-buffer report
                            (+agent-shell-pstack-walkthrough-test--follow
                             +agent-shell-pstack-walkthrough--result
                             "lisp/b.el" 1)))))
            (with-current-buffer buffer
              (should (equal "from the host\n" (buffer-string))))
            (should (equal commit (+agent-shell-pstack-walkthrough-test--head
                                   repo)))))))))

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
                     +agent-shell-pstack-walkthrough-review-file))
        (should-not (file-exists-p
                     +agent-shell-pstack-walkthrough-opens-file))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-cleanup ()
  "Cleaning a panel's arena removes its review refs and adds no worktree."
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
      (make-directory directory t)
      (dolist (role '("opus" "codex"))
        (+agent-shell-pstack-walkthrough-test--record
         "p6" role :git_dir (expand-file-name ".git" repo)
         :toplevel repo :base base :commit commit)
        (+agent-shell-pstack-walkthrough-test--git
         repo "update-ref" (concat "refs/pstack/review/p6/" role) commit))
      (+agent-shell-pstack-walkthrough-test--git
       repo "update-ref" "refs/pstack/review/p7/opus" commit)
      (should (+agent-shell-pstack-walkthrough-reviews-p "p6"))
      (+agent-shell-pstack-arena--cleanup manifest
                                          (lambda (error) (setq ended
                                                                (list error))))
      (+agent-shell-pstack-walkthrough-test--wait (lambda () ended))
      (should (equal '(nil) ended))
      (should-not (+agent-shell-pstack-walkthrough-reviews-p "p6"))
      (should (equal "refs/pstack/review/p7/opus"
                     (+agent-shell-pstack-walkthrough-test--git
                      repo "for-each-ref" "--format=%(refname)"
                      "refs/pstack/review/")))
      (should-not (file-exists-p
                   (expand-file-name "p6/review"
                                     +agent-shell-pstack-panel-directory)))
      (should (= 1 (length (split-string
                            (+agent-shell-pstack-walkthrough-test--git
                             repo "worktree" "list")
                            "\n"))))
      (should-not (file-exists-p directory)))))

(ert-deftest +agent-shell-pstack-walkthrough-test-busy-shell ()
  "Refuse to switch a checkout an agent shell is working in."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n")))
      (+agent-shell-pstack-walkthrough-test--with-shells
        (let ((shell (+agent-shell-pstack-walkthrough-test--shell
                      "*lead*" (expand-file-name "lisp/" repo) 'busy))
              (other (+agent-shell-pstack-walkthrough-test--shell
                      "*member*" repo 'blocked)))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (error "Asked %s" prompt))))
            (should (equal (format "*lead*, *member* are working in %s; wait \
until they are idle" (abbreviate-file-name repo))
                           (cadr (should-error
                                  (+agent-shell-pstack-walkthrough-follow
                                   result "lisp/a.el" 1)
                                  :type 'user-error))))
            (kill-buffer other)
            (should (equal (format "*lead* is working in %s; wait until it \
is idle" (abbreviate-file-name repo))
                           (cadr (should-error
                                  (+agent-shell-pstack-walkthrough-follow
                                   result "lisp/a.el" 1)
                                  :type 'user-error)))))
          (should (buffer-live-p shell))
          (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                                 repo)))
          (should-not (file-exists-p
                       +agent-shell-pstack-walkthrough-review-file)))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-branch-panel ()
  "Refuse to switch a checkout an unfinished branch-mode delegate works in."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let* ((result (+agent-shell-pstack-walkthrough-test--branch-result
                    repo "p3" "result\n"))
           (panel (+agent-shell-pstack-panel--make
                   :id "p9" :shape 'delegate :phase 'running :cwd repo
                   :arena (+agent-shell-arena-git--manifest
                           :id "p9" :toplevel repo
                           :delegate (+agent-shell-arena-git--delegate
                                      :isolation 'branch))))
           prompts)
      (puthash "p9" panel +agent-shell-pstack-panel--panels)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt) (push prompt prompts) t)))
        (should (equal (format "Panel p9 has a delegate working in %s; wait \
until it finishes" (abbreviate-file-name repo))
                       (cadr (should-error
                              (+agent-shell-pstack-walkthrough-follow
                               result "lisp/a.el" 1)
                              :type 'user-error))))
        (should-not prompts)
        (setf (+agent-shell-pstack-panel-arena panel) nil
              (+agent-shell-pstack-panel-delegate-options panel)
              '(:isolation branch))
        (should-error (+agent-shell-pstack-walkthrough-follow
                       result "lisp/a.el" 1)
                      :type 'user-error)
        (setf (+agent-shell-pstack-panel-delegate-options panel)
              '(:isolation worktree))
        (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1)
        (should (= 1 (length prompts)))
        (should (equal "pstack/delegate/p3"
                       (+agent-shell-pstack-walkthrough-test--head repo)))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-idle-shells ()
  "Name idle shells in the checkout in the prompt, and ignore others."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n"))
          (name (abbreviate-file-name repo))
          prompts)
      (+agent-shell-pstack-walkthrough-test--with-shells
        (+agent-shell-pstack-walkthrough-test--shell "*lead*" repo 'ready)
        (+agent-shell-pstack-walkthrough-test--shell
         "*elsewhere*" (expand-file-name "repo2/" scratch) 'busy)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt) (push prompt prompts) t)))
          (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1))
        (should (equal (list (format "*lead* works in %s and will see the \
result's files until you return.\nSwitch %s from main to panel p3's builder \
result %s (branch pstack/delegate/p3)? " name name
(substring (plist-get result :commit)
           0 7)))
                       prompts))
        (should (equal "pstack/delegate/p3"
                       (+agent-shell-pstack-walkthrough-test--head repo)))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-other-directory ()
  "Ignore busy shells and branch-mode panels in other directories."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n"))
          (other (expand-file-name "repo2/" scratch))
          prompts)
      (+agent-shell-pstack-walkthrough-test--with-shells
        (+agent-shell-pstack-walkthrough-test--shell "*other*" other 'busy)
        (puthash "p9" (+agent-shell-pstack-panel--make
                       :id "p9" :shape 'delegate :phase 'running :cwd other
                       :arena (+agent-shell-arena-git--manifest
                               :id "p9" :toplevel other
                               :delegate (+agent-shell-arena-git--delegate
                                          :isolation 'branch)))
                 +agent-shell-pstack-panel--panels)
        (puthash "p8" (+agent-shell-pstack-panel--make
                       :id "p8" :shape 'delegate :phase 'finished :cwd repo
                       :arena (+agent-shell-arena-git--manifest
                               :id "p8" :toplevel repo
                               :delegate (+agent-shell-arena-git--delegate
                                          :isolation 'branch)))
                 +agent-shell-pstack-panel--panels)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt) (push prompt prompts) t)))
          (+agent-shell-pstack-walkthrough-test--follow result "lisp/a.el" 1))
        (should (= 1 (length prompts)))
        (should (string-prefix-p "Switch " (car prompts)))
        (should (equal "pstack/delegate/p3"
                       (+agent-shell-pstack-walkthrough-test--head repo)))))))

(ert-deftest +agent-shell-pstack-walkthrough-test-return-agents ()
  "Refuse to return a checkout an agent is working in."
  (+agent-shell-pstack-walkthrough-test--with
    (+agent-shell-pstack-walkthrough-test--commit repo "lisp/a.el" "base\n")
    (let ((result (+agent-shell-pstack-walkthrough-test--branch-result
                   repo "p3" "result\n"))
          (key (+agent-shell-pstack-walkthrough--key repo))
          (name (abbreviate-file-name repo)))
      (+agent-shell-pstack-walkthrough-test--with-shells
        (let ((shell (+agent-shell-pstack-walkthrough-test--shell
                      "*lead*" repo 'ready)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (+agent-shell-pstack-walkthrough-test--follow
             result "lisp/a.el" 1))
          (with-current-buffer shell
            (setq +agent-shell-pstack-walkthrough-test--status 'busy))
          (should (equal (format "*lead* is working in %s; wait until it is \
idle" name)
                         (cadr (should-error
                                (+agent-shell-pstack-walkthrough-return key)
                                :type 'user-error))))
          (with-current-buffer shell
            (setq +agent-shell-pstack-walkthrough-test--status 'ready))
          (puthash "p9" (+agent-shell-pstack-panel--make
                         :id "p9" :shape 'delegate :phase 'preparing :cwd repo
                         :delegate-options '(:isolation branch))
                   +agent-shell-pstack-panel--panels)
          (should (equal (format "Panel p9 has a delegate working in %s; wait \
until it finishes" name)
                         (cadr (should-error
                                (+agent-shell-pstack-walkthrough-return key)
                                :type 'user-error))))
          (should (equal "pstack/delegate/p3"
                         (+agent-shell-pstack-walkthrough-test--head repo)))
          (should (+agent-shell-pstack-walkthrough--review key))
          (clrhash +agent-shell-pstack-panel--panels)
          (+agent-shell-pstack-walkthrough-return key)
          (+agent-shell-pstack-walkthrough-test--wait
           (lambda ()
             (not (file-exists-p
                   +agent-shell-pstack-walkthrough-review-file))))
          (should (equal "main" (+agent-shell-pstack-walkthrough-test--head
                                 repo))))))))

(provide 'agent-shell-pstack-walkthrough-test)
;;; agent-shell-pstack-walkthrough-test.el ends here
