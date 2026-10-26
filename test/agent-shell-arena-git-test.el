;;; agent-shell-arena-git-test.el --- Tests for arena git work -*- lexical-binding: t; -*-

;;; Commentary:
;; Each test runs real git against scratch repositories with a private
;; global config, and waits for callbacks in a bounded loop.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-arena-git)

(defvar +arena-git-test--scratch nil)

(defmacro +arena-git-test--with-scratch (&rest body)
  "Run BODY with a scratch directory, private git config and arena root."
  (declare (indent 0))
  `(let* ((+arena-git-test--scratch
           (file-name-as-directory
            (file-truename (make-temp-file "arena-git-test-" t))))
          (config (expand-file-name "gitconfig" +arena-git-test--scratch))
          (process-environment
           (append (list (concat "GIT_CONFIG_GLOBAL=" config)
                         "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (+agent-shell-pstack-arena-directory
           (expand-file-name "arenas/" +arena-git-test--scratch))
          (+agent-shell-pstack-arena-dependencies nil))
     (with-temp-file config
       (insert "[user]\n\tname = Test\n\temail = test@example.com\n"
               "[init]\n\tdefaultBranch = main\n"))
     (unwind-protect (progn ,@body)
       (delete-directory +arena-git-test--scratch t))))

(defun +arena-git-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY and return trimmed output, failing on error."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory directory))
           (status (apply #'process-file "git" nil t nil
                          "-c" "core.fsmonitor=false" args)))
      (unless (eql status 0)
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun +arena-git-test--write (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert content)))

(defun +arena-git-test--read (file)
  "Return FILE's contents."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun +arena-git-test--repo (&rest files)
  "Return a scratch repository committing FILES, a plist of path and text."
  (let ((repo (file-name-as-directory
               (make-temp-file
                (expand-file-name "repo-" +arena-git-test--scratch) t))))
    (+arena-git-test--git repo "init" "-q")
    (cl-loop for (path text) on files by #'cddr
             do (+arena-git-test--write (expand-file-name path repo) text))
    (+arena-git-test--git repo "add" "-A")
    (+arena-git-test--git repo "commit" "-q" "-m" "initial")
    repo))

(defun +arena-git-test--wait (start)
  "Call START with a callback and return the (RESULT ERROR) it receives."
  (let ((outcome nil) (deadline (+ (float-time) 30)))
    (funcall start (lambda (result error) (setq outcome (list result error))))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (or outcome (error "Timed out waiting for an arena callback"))))

(defun +arena-git-test--ok (start)
  "Return the result START delivers, failing the test on an error."
  (pcase-let ((`(,result ,error) (+arena-git-test--wait start)))
    (should-not error)
    result))

(defun +arena-git-test--base (directory)
  "Capture and return the base of the lead at DIRECTORY."
  (+arena-git-test--ok
   (lambda (done) (+agent-shell-arena-git-capture-base directory done))))

(defun +arena-git-test--rejection (directory)
  "Return the message rejecting a base at DIRECTORY."
  (pcase-let ((`(,result ,error)
               (+arena-git-test--wait
                (lambda (done)
                  (+agent-shell-arena-git-capture-base directory done)))))
    (should-not result)
    (should (stringp error))
    error))

(defun +arena-git-test--tree (repo commit)
  "Return COMMIT's files in REPO as an alist of path to mode."
  (mapcar (lambda (line)
            (string-match "\\`\\([0-9]+\\) [a-z]+ [0-9a-f]+\t\\(.*\\)\\'" line)
            (cons (match-string 2 line) (match-string 1 line)))
          (split-string (+arena-git-test--git repo "ls-tree" "-r" commit)
                        "\n" t)))

(defun +arena-git-test--arena (base)
  "Create an arena over BASE."
  (+agent-shell-arena-git-create base "panel1"))

(defun +arena-git-test--provision (manifest role)
  "Provision ROLE in MANIFEST and return its working directory."
  (+arena-git-test--ok
   (lambda (done) (+agent-shell-arena-git-provision manifest role done))))

(defun +arena-git-test--capture (manifest role)
  "Capture ROLE in MANIFEST and return its commit."
  (+arena-git-test--ok
   (lambda (done) (+agent-shell-arena-git-capture manifest role done))))

(ert-deftest +agent-shell-arena-git-test-base-snapshots-working-tree ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--repo
                  ".gitignore" "*.log\nignored.txt\n"
                  "edit.txt" "old\n" "gone.txt" "gone\n" "move.txt" "move\n"
                  "run.sh" "echo\n"))
           (index (expand-file-name ".git/index" repo))
           (head (+arena-git-test--git repo "rev-parse" "HEAD")))
      (+arena-git-test--write (expand-file-name "edit.txt" repo) "new\n")
      (delete-file (expand-file-name "gone.txt" repo))
      (rename-file (expand-file-name "move.txt" repo)
                   (expand-file-name "moved.txt" repo))
      (set-file-modes (expand-file-name "run.sh" repo) #o755)
      (+arena-git-test--write (expand-file-name "untracked.txt" repo) "u\n")
      (make-symbolic-link "edit.txt" (expand-file-name "link" repo))
      (+arena-git-test--write (expand-file-name "ignored.txt" repo) "i\n")
      (+arena-git-test--write (expand-file-name "forced.log" repo) "f\n")
      (+arena-git-test--git repo "add" "-f" "forced.log")
      (+arena-git-test--write (expand-file-name ".agent-shell/t.md" repo) "t\n")
      (let* ((index-before (+arena-git-test--read index))
             (base (+arena-git-test--base repo))
             (commit (+agent-shell-arena-git-base-commit base)))
        (should (equal (+agent-shell-arena-git-base-head base) head))
        (should (equal (+arena-git-test--git repo "rev-parse"
                                             (concat commit "^"))
                       head))
        (should (equal (+arena-git-test--tree repo commit)
                       '((".gitignore" . "100644") ("edit.txt" . "100644")
                         ("forced.log" . "100644") ("link" . "120000")
                         ("moved.txt" . "100644") ("run.sh" . "100755")
                         ("untracked.txt" . "100644"))))
        (should (equal (+arena-git-test--git repo "show"
                                             (concat commit ":edit.txt"))
                       "new"))
        (should (equal (+arena-git-test--git repo "show"
                                             (concat commit ":link"))
                       "edit.txt"))
        (should (equal (+arena-git-test--read index) index-before))
        (should (equal (+arena-git-test--git repo "rev-parse" "HEAD") head))))))

(ert-deftest +agent-shell-arena-git-test-rejections ()
  (+arena-git-test--with-scratch
    (let ((outside (make-temp-file
                    (expand-file-name "plain-" +arena-git-test--scratch) t))
          (unborn (make-temp-file
                   (expand-file-name "unborn-" +arena-git-test--scratch) t))
          (submodule (+arena-git-test--repo "a.txt" "a\n" "b.txt" "b\n")))
      (+arena-git-test--git unborn "init" "-q")
      (+arena-git-test--git
       submodule "update-index" "--add" "--cacheinfo"
       (concat "160000," (+arena-git-test--git submodule "rev-parse" "HEAD")
               ",sub/vendor"))
      (+arena-git-test--git submodule "commit" "-q" "-m" "gitlink")
      (should (string-match-p "not inside a git"
                              (+arena-git-test--rejection outside)))
      (should (string-match-p "unborn" (+arena-git-test--rejection unborn)))
      (should (string-match-p "submodule"
                              (+arena-git-test--rejection submodule))))))

(ert-deftest +agent-shell-arena-git-test-subdirectory-lead ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--repo "sub/dir/a.txt" "a\n" "b.txt" "b\n"))
           (base (+arena-git-test--base (expand-file-name "sub/dir/" repo)))
           (manifest (+arena-git-test--arena base))
           (directory (+arena-git-test--provision manifest "opus")))
      (should (equal (+agent-shell-arena-git-base-prefix base) "sub/dir/"))
      (should (equal directory
                     (expand-file-name
                      "opus/sub/dir/"
                      (+agent-shell-arena-git-manifest-directory manifest))))
      (should (file-exists-p (expand-file-name "a.txt" directory))))))

(ert-deftest +agent-shell-arena-git-test-linked-worktree-lead ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (lead (expand-file-name "lead/" +arena-git-test--scratch)))
      (+arena-git-test--git repo "worktree" "add" "-q" "-b" "feature"
                            (directory-file-name lead))
      (+arena-git-test--write (expand-file-name "a.txt" lead) "lead edit\n")
      (let* ((base (+arena-git-test--base lead))
             (manifest (+arena-git-test--arena base))
             (directory (+arena-git-test--provision manifest "opus")))
        (should (equal (+agent-shell-arena-git-base-head base)
                       (+arena-git-test--git lead "rev-parse" "HEAD")))
        (should (equal (+arena-git-test--read
                        (expand-file-name "a.txt" directory))
                       "lead edit\n"))
        (+arena-git-test--write (expand-file-name "b.txt" directory) "b\n")
        (let ((commit (+arena-git-test--capture manifest "opus")))
          (should (equal (+arena-git-test--git
                          repo "diff" "--name-status"
                          (+agent-shell-arena-git-base-commit base) commit)
                         "A\tb.txt")))
        (should (equal (+arena-git-test--git lead "status" "--porcelain")
                       "M a.txt"))))))

(defun +arena-git-test--dependency-repo ()
  "Return a repository with an ignored shared and private dependency."
  (let ((repo (+arena-git-test--repo ".gitignore" "deps/\nbuild/\n"
                                     "src/a.txt" "a\n")))
    (+arena-git-test--write (expand-file-name "deps/lib.el" repo) "lib\n")
    (+arena-git-test--write (expand-file-name "build/sources/x.el" repo) "x\n")
    (make-symbolic-link (expand-file-name "build/sources/x.el" repo)
                        (expand-file-name "build/x.el" repo))
    (make-symbolic-link "sources/x.el" (expand-file-name "build/rel.el" repo))
    (setq +agent-shell-pstack-arena-dependencies
          `((,repo :shared ("deps") :private ("build"))))
    repo))

(ert-deftest +agent-shell-arena-git-test-provision-dependencies ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--dependency-repo))
           (base (+arena-git-test--base repo))
           (manifest (+arena-git-test--arena base))
           (worktree (+arena-git-test--provision manifest "codex")))
      (should (equal (+arena-git-test--git worktree "rev-parse" "HEAD")
                     (+agent-shell-arena-git-base-commit base)))
      (should (equal (file-symlink-p (expand-file-name "deps" worktree))
                     (expand-file-name "deps" repo)))
      (should (file-directory-p (expand-file-name "build" worktree)))
      (should-not (file-symlink-p (expand-file-name "build" worktree)))
      (should (equal (file-symlink-p (expand-file-name "build/x.el" worktree))
                     (expand-file-name "build/sources/x.el" worktree)))
      (should (equal (file-symlink-p (expand-file-name "build/rel.el" worktree))
                     "sources/x.el"))
      (should (equal (file-symlink-p (expand-file-name "build/x.el" repo))
                     (expand-file-name "build/sources/x.el" repo))))))

(ert-deftest +agent-shell-arena-git-test-capture-candidate ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--dependency-repo))
           (_forced (progn
                      (+arena-git-test--write
                       (expand-file-name ".git/info/exclude" repo) "*.log\n")
                      (+arena-git-test--write (expand-file-name "keep.log" repo)
                                              "kept\n")
                      (+arena-git-test--git repo "add" "-f" "keep.log")))
           (base (+arena-git-test--base repo))
           (manifest (+arena-git-test--arena base))
           (worktree (+arena-git-test--provision manifest "opus")))
      (+arena-git-test--write (expand-file-name "src/a.txt" worktree)
                              "changed\n")
      (+arena-git-test--write (expand-file-name "src/new.txt" worktree) "new\n")
      (+arena-git-test--write (expand-file-name ".agent-shell/t.md" worktree)
                              "transcript\n")
      (+arena-git-test--write
       (expand-file-name "src/.agent-shell/t.md" worktree) "transcript\n")
      (let ((commit (+arena-git-test--capture manifest "opus"))
            (ref (format "refs/pstack-arena/%s/opus"
                         (+agent-shell-arena-git-manifest-id manifest))))
        (should (equal (+arena-git-test--git
                        repo "diff" "--name-status"
                        (+agent-shell-arena-git-base-commit base) commit)
                       "M\tsrc/a.txt\nA\tsrc/new.txt"))
        (should (equal (+arena-git-test--git repo "rev-parse" ref) commit))
        (let ((role (+agent-shell-arena-git-role
                     (+agent-shell-arena-git-read-manifest
                      (+agent-shell-arena-git-manifest-directory manifest))
                     "opus")))
          (should (equal (+agent-shell-arena-git-role-commit role) commit))
          (should (equal (+agent-shell-arena-git-role-ref role) ref)))
        (+arena-git-test--git repo "reflog" "expire" "--expire=now" "--all")
        (+arena-git-test--git repo "gc" "-q" "--prune=now")
        (should (equal (+arena-git-test--git repo "cat-file" "-t" commit)
                       "commit"))))))

(defun +arena-git-test--old-capture-tree (manifest role)
  "Return the tree the old capture made of ROLE in MANIFEST.
It seeded the snapshot index with the base tree alone."
  (let* ((role (+agent-shell-arena-git-role manifest role))
         (worktree (+agent-shell-arena-git-role-worktree role))
         (index (expand-file-name "old-index" +arena-git-test--scratch))
         (process-environment (cons (concat "GIT_INDEX_FILE=" index)
                                    process-environment))
         (git (lambda (&rest args)
                (apply #'+arena-git-test--git worktree
                       "--git-dir"
                       (+agent-shell-arena-git-manifest-git-dir manifest)
                       "--work-tree" worktree args))))
    (funcall git "read-tree" (+agent-shell-arena-git-manifest-base manifest))
    (apply git "add" "-A" "--" "." ":(exclude,glob)**/.agent-shell/**"
           (mapcar (lambda (path) (concat ":(exclude,literal)" path))
                   (+agent-shell-arena-git-role-shared role)))
    (prog1 (funcall git "write-tree") (delete-file index))))

(ert-deftest +agent-shell-arena-git-test-capture-matches-old-method ()
  "Seeding from the worktree's index yields the tree a base seed did, even
after the candidate stages excluded paths and commits in its worktree."
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--dependency-repo))
           (_ (+arena-git-test--write (expand-file-name "src/b.txt" repo) "b\n"))
           (base (+arena-git-test--base repo))
           (manifest (+arena-git-test--arena base))
           (worktree (+arena-git-test--provision manifest "opus"))
           (role (+agent-shell-arena-git-role manifest "opus")))
      (should (equal (+agent-shell-arena-git-role-git-dir
                      (+agent-shell-arena-git-role
                       (+agent-shell-arena-git-read-manifest
                        (+agent-shell-arena-git-manifest-directory manifest))
                       "opus"))
                     (+arena-git-test--git worktree "rev-parse"
                                           "--absolute-git-dir")))
      (should (file-exists-p
               (expand-file-name "index"
                                 (+agent-shell-arena-git-role-git-dir role))))
      (+arena-git-test--write (expand-file-name "src/a.txt" worktree) "edit\n")
      (delete-file (expand-file-name "src/b.txt" worktree))
      (+arena-git-test--write (expand-file-name "src/new.txt" worktree) "new\n")
      (+arena-git-test--write (expand-file-name ".agent-shell/t.md" worktree)
                              "transcript\n")
      (+arena-git-test--git worktree "add" "-f" "-A")
      (+arena-git-test--git worktree "commit" "-q" "-m" "candidate commit")
      (+arena-git-test--write (expand-file-name "src/late.txt" worktree) "l\n")
      (let ((commit (+arena-git-test--capture manifest "opus")))
        (should (equal (+arena-git-test--git repo "rev-parse"
                                             (concat commit "^{tree}"))
                       (+arena-git-test--old-capture-tree manifest "opus")))
        (should (equal (+arena-git-test--git
                        repo "diff" "--name-status"
                        (+agent-shell-arena-git-base-commit base) commit)
                       "M\tsrc/a.txt\nD\tsrc/b.txt\nA\tsrc/late.txt\n\
A\tsrc/new.txt"))))))

(ert-deftest +agent-shell-arena-git-test-capture-hashes-changed-files-only ()
  "A capture runs the clean filter only on files changed since checkout.
Files written in the same second as an index are racily clean and get
hashed anyway, so the worktree's index is refreshed a second later."
  (+arena-git-test--with-scratch
    (let* ((repo (apply #'+arena-git-test--repo
                        (cl-loop for i below 20
                                 append (list (format "f%02d.txt" i) "x\n"))))
           (log (expand-file-name "clean.log" +arena-git-test--scratch)))
      (+arena-git-test--git
       repo "config" "filter.count.clean"
       (format "sh -c 'echo \"$1\" >> %s; cat' clean %%f"
               (shell-quote-argument log)))
      (+arena-git-test--write (expand-file-name ".git/info/attributes" repo)
                              "*.txt filter=count\n")
      (let* ((base (+arena-git-test--base repo))
             (manifest (+arena-git-test--arena base))
             (worktree (+arena-git-test--provision manifest "opus")))
        (sleep-for 1.1)
        (+arena-git-test--git worktree "update-index" "-q" "--refresh")
        (+arena-git-test--write (expand-file-name "f07.txt" worktree) "y\n")
        (when (file-exists-p log) (delete-file log))
        (+arena-git-test--capture manifest "opus")
        (should (equal (delete-dups
                        (split-string (+arena-git-test--read log) "\n" t))
                       '("f07.txt")))))))

(defun +arena-git-test--sparse-repo (&rest arguments)
  "Return a repository sparsely checked out by `sparse-checkout set' ARGUMENTS."
  (let ((repo (+arena-git-test--repo
               "top.txt" "top\n" "src/a.txt" "a\n" "l10n/m.en.xlf" "en\n"
               "l10n/m.fr.xlf" "fr\n" "docs/d.txt" "d\n")))
    (apply #'+arena-git-test--git repo "sparse-checkout" "set" arguments)
    repo))

(defun +arena-git-test--files (directory)
  "Return the files under DIRECTORY, relative to it, outside .git."
  (sort (seq-remove (lambda (file) (string-prefix-p ".git" file))
                    (mapcar (lambda (file) (file-relative-name file directory))
                            (directory-files-recursively directory "")))
        #'string<))

(defun +arena-git-test--sparse-capture (repo)
  "Provision a candidate over sparse REPO, then check what it sees and makes.
Return the candidate's diff plist after it edits a file, creates one inside
and one outside the sparse patterns."
  (let* ((base (+arena-git-test--base repo))
         (manifest (+arena-git-test--arena base))
         (worktree (+arena-git-test--provision manifest "opus"))
         (commit (+agent-shell-arena-git-base-commit base)))
    (should (equal (+arena-git-test--git worktree "sparse-checkout" "list")
                   (+arena-git-test--git repo "sparse-checkout" "list")))
    (dolist (key '("core.sparseCheckout" "core.sparseCheckoutCone"
                   "index.sparse"))
      (should (equal (ignore-errors
                       (+arena-git-test--git worktree "config" "--type=bool"
                                             key))
                     (ignore-errors
                       (+arena-git-test--git repo "config" "--type=bool"
                                             key)))))
    (should (equal (+arena-git-test--files worktree)
                   (+arena-git-test--files repo)))
    (should (equal (+arena-git-test--git repo "ls-tree" "-r" commit)
                   (+arena-git-test--git repo "ls-tree" "-r" "HEAD")))
    (+arena-git-test--write (expand-file-name "src/a.txt" worktree) "edit\n")
    (+arena-git-test--write (expand-file-name "src/new.txt" worktree) "new\n")
    (+arena-git-test--write (expand-file-name "l10n/m.de.xlf" worktree) "de\n")
    (let ((result (+arena-git-test--capture manifest "opus")))
      (should (equal (+arena-git-test--git repo "diff" "--name-status" commit
                                           result)
                     "A\tl10n/m.de.xlf\nM\tsrc/a.txt\nA\tsrc/new.txt"))
      (+arena-git-test--ok
       (lambda (done) (+agent-shell-arena-git-diff manifest "opus" done))))))

(ert-deftest +agent-shell-arena-git-test-sparse-non-cone ()
  "A non-cone sparse lead's candidate gets its patterns and only its files.
Skip-worktree entries survive the base, and a new file outside the
patterns is captured and reported."
  (+arena-git-test--with-scratch
    (let ((repo (+arena-git-test--sparse-repo
                 "--no-cone" "/*" "!*.xlf" "*en.xlf" "!/docs/")))
      (should (equal (+arena-git-test--files repo)
                     '("l10n/m.en.xlf" "src/a.txt" "top.txt")))
      (should (equal (plist-get (+arena-git-test--sparse-capture repo)
                                :outside)
                     '("l10n/m.de.xlf"))))))

(ert-deftest +agent-shell-arena-git-test-sparse-cone-index ()
  "A cone-mode lead with a sparse index works like a non-cone one."
  (+arena-git-test--with-scratch
    (let ((repo (+arena-git-test--sparse-repo "--cone" "--sparse-index" "src")))
      (should (equal (+arena-git-test--files repo) '("src/a.txt" "top.txt")))
      (should (equal (plist-get (+arena-git-test--sparse-capture repo)
                                :outside)
                     '("l10n/m.de.xlf"))))))

(ert-deftest +agent-shell-arena-git-test-sparse-common-config ()
  "A sparse checkout configured in the shared config, without
extensions.worktreeConfig, reaches the candidate too."
  (+arena-git-test--with-scratch
    (let ((repo (+arena-git-test--repo "a.txt" "a\n" "b.txt" "b\n")))
      (+arena-git-test--git repo "config" "core.sparseCheckout" "true")
      (+arena-git-test--write (expand-file-name ".git/info/sparse-checkout"
                                                repo)
                              "/a.txt\n")
      (+arena-git-test--git repo "read-tree" "-mu" "HEAD")
      (let* ((manifest (+arena-git-test--arena (+arena-git-test--base repo)))
             (worktree (+arena-git-test--provision manifest "opus")))
        (should (equal (+arena-git-test--files worktree) '("a.txt")))))))

(ert-deftest +agent-shell-arena-git-test-lfs ()
  "An LFS edit is captured as a pointer change that the graft applies;
untouched LFS files stay out of the diff."
  (skip-unless (eql 0 (ignore-errors
                        (call-process "git" nil nil nil "lfs" "version"))))
  (+arena-git-test--with-scratch
    (+arena-git-test--git +arena-git-test--scratch "lfs" "install"
                          "--skip-repo")
    (let ((repo (+arena-git-test--repo "a.txt" "a\n")))
      (+arena-git-test--git repo "lfs" "track" "*.bin")
      (+arena-git-test--write (expand-file-name "x.bin" repo) "x binary\n")
      (+arena-git-test--write (expand-file-name "y.bin" repo) "y binary\n")
      (+arena-git-test--git repo "add" "-A")
      (+arena-git-test--git repo "commit" "-q" "-m" "lfs")
      (let* ((base (+arena-git-test--base repo))
             (manifest (+arena-git-test--arena base))
             (worktree (+arena-git-test--provision manifest "opus"))
             (commit (+agent-shell-arena-git-base-commit base)))
        (should (equal (+arena-git-test--read
                        (expand-file-name "x.bin" worktree))
                       "x binary\n"))
        (+arena-git-test--write (expand-file-name "x.bin" worktree) "x v2\n")
        (+arena-git-test--write (expand-file-name "a.txt" worktree) "a2\n")
        (let ((result (+arena-git-test--capture manifest "opus")))
          (should (equal (+arena-git-test--git repo "diff" "--name-only"
                                               commit result)
                         "a.txt\nx.bin"))
          (should (string-prefix-p
                   "version https://git-lfs.github.com/spec/v1\n"
                   (+arena-git-test--git repo "show"
                                         (concat result ":x.bin"))))
          (should (equal (plist-get
                          (+arena-git-test--ok
                           (lambda (done)
                             (+agent-shell-arena-git-diff manifest "opus"
                                                          done)))
                          :lfs)
                         '("x.bin")))
          (let ((default-directory repo))
            (should (eql 0 (process-file
                            "sh" nil nil nil "-c"
                            (format "git diff --binary %s %s | git apply \
--3way" commit result)))))
          (should (equal (+arena-git-test--read (expand-file-name "x.bin" repo))
                         "x v2\n")))))))

(ert-deftest +agent-shell-arena-git-test-needs-no-identity-or-key ()
  (+arena-git-test--with-scratch
    (let ((repo (+arena-git-test--repo "a.txt" "a\n")))
      (+arena-git-test--git repo "config" "commit.gpgSign" "true")
      (+arena-git-test--git repo "config" "gpg.program" "false")
      (with-temp-file config (insert "[user]\n\tuseConfigOnly = true\n"))
      (let* ((base (+arena-git-test--base repo))
             (manifest (+arena-git-test--arena base))
             (worktree (+arena-git-test--provision manifest "opus")))
        (+arena-git-test--write (expand-file-name "b.txt" worktree) "b\n")
        (should (+arena-git-test--capture manifest "opus"))))))

(ert-deftest +agent-shell-arena-git-test-clean ()
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--dependency-repo))
           (base (+arena-git-test--base repo))
           (manifest (+arena-git-test--arena base))
           (directory (+agent-shell-arena-git-manifest-directory manifest))
           (id (+agent-shell-arena-git-manifest-id manifest))
           (opus (+arena-git-test--provision manifest "opus"))
           (codex (+arena-git-test--provision manifest "codex")))
      (+arena-git-test--write (expand-file-name "x.txt" opus) "x\n")
      (+arena-git-test--write (expand-file-name "y.txt" codex) "y\n")
      (+arena-git-test--capture manifest "opus")
      (+arena-git-test--capture manifest "codex")
      (+arena-git-test--git repo "update-ref"
                            (format "refs/pstack-arena/%s/codex" id)
                            (+agent-shell-arena-git-base-commit base))
      (should (equal (mapcar #'+agent-shell-arena-git-manifest-id
                             (+agent-shell-arena-git-arenas))
                     (list id)))
      (let ((left (+arena-git-test--ok
                   (lambda (done)
                     (+agent-shell-arena-git-clean
                      (+agent-shell-arena-git-read-manifest directory) done)))))
        (should (= (length left) 1))
        (should (string-match-p "codex" (car left))))
      (should-not (file-exists-p opus))
      (should-not (file-exists-p codex))
      (should-not (file-exists-p directory))
      (should-not (string-match-p
                   "arenas" (+arena-git-test--git repo "worktree" "list")))
      (should (equal (+arena-git-test--git
                      repo "for-each-ref" "--format=%(refname) %(objectname)"
                      "refs/pstack-arena/")
                     (format "refs/pstack-arena/%s/codex %s" id
                             (+agent-shell-arena-git-base-commit base))))
      (should (file-exists-p (expand-file-name "build/x.el" repo)))
      (should (file-exists-p (expand-file-name "deps/lib.el" repo)))
      (should-not (+agent-shell-arena-git-arenas)))))

(ert-deftest +agent-shell-arena-git-test-diff-and-checkout ()
  "Summarize each result against the base and check it out for reading.
Cleanup removes the checkout with the worktree."
  (+arena-git-test--with-scratch
    (let* ((repo (+arena-git-test--repo "a.txt" "a\n"))
           (base (+arena-git-test--base repo))
           (manifest (+arena-git-test--arena base))
           (directory (+agent-shell-arena-git-manifest-directory manifest))
           (opus (+arena-git-test--provision manifest "opus")))
      (+arena-git-test--provision manifest "codex")
      (+arena-git-test--write (expand-file-name "b.txt" opus) "b\nc\n")
      (let ((commit (+arena-git-test--capture manifest "opus")))
        (+arena-git-test--capture manifest "codex")
        (should (equal '(:changed t :stat " b.txt | 2 ++\n 1 file changed, \
2 insertions(+)" :outside nil :lfs nil)
                       (+arena-git-test--ok
                        (lambda (done)
                          (+agent-shell-arena-git-diff manifest "opus" done)))))
        (should (equal '(:changed nil :stat "" :outside nil :lfs nil)
                       (+arena-git-test--ok
                        (lambda (done)
                          (+agent-shell-arena-git-diff
                           manifest "codex" done)))))
        (pcase-let ((`(,result ,error)
                     (+arena-git-test--wait
                      (lambda (done)
                        (+agent-shell-arena-git-diff manifest "gemini" done)))))
          (should-not result)
          (should (string-match-p "no result" error)))
        (let ((checkout (+arena-git-test--ok
                         (lambda (done)
                           (+agent-shell-arena-git-checkout
                            manifest "opus" done)))))
          (should (equal (expand-file-name "checkouts/opus/" directory)
                         checkout))
          (should (equal "b\nc\n" (+arena-git-test--read
                                   (expand-file-name "b.txt" checkout))))
          (+arena-git-test--write (expand-file-name "late.txt" opus) "late\n")
          (should-not (file-exists-p (expand-file-name "late.txt" checkout)))
          (should (equal commit (+arena-git-test--git checkout "rev-parse"
                                                      "HEAD")))
          (should (equal checkout
                         (+agent-shell-arena-git-role-checkout
                          (+agent-shell-arena-git-role
                           (+agent-shell-arena-git-read-manifest directory)
                           "opus"))))
          (should-not (+arena-git-test--ok
                       (lambda (done)
                         (+agent-shell-arena-git-clean
                          (+agent-shell-arena-git-read-manifest directory)
                          done))))
          (should-not (file-exists-p checkout))
          (should-not (file-exists-p directory))
          (should (equal (file-truename repo)
                         (file-name-as-directory
                          (substring (+arena-git-test--git
                                      repo "worktree" "list" "--porcelain")
                                     (length "worktree ")
                                     (string-search
                                      "\n" (+arena-git-test--git
                                            repo "worktree" "list"
                                            "--porcelain")))))))))))

(provide 'agent-shell-arena-git-test)
;;; agent-shell-arena-git-test.el ends here
