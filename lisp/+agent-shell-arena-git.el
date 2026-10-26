;;; +agent-shell-arena-git.el --- Git and filesystem work for pstack arenas -*- lexical-binding: t; -*-

;;; Commentary:
;; An arena snapshots the lead's working tree as a base commit, gives each
;; candidate a detached worktree at that base, captures each candidate's tree
;; as a result commit under refs/pstack-arena/, and cleans up from the
;; manifest it keeps at <arena-dir>/manifest.json.
;;
;; Every git command after discovery names its git directory and work tree
;; and runs with hooks and fsmonitor off.  Configured clean and smudge
;; filters, such as Git LFS's, still run, so a snapshot stores what git
;; add would and a checkout holds what git checkout would.  Snapshots go
;; through a temporary index, so the lead's index and HEAD never change.
;; Slow work runs in async processes.  Asynchronous operations take a
;; CALLBACK of (RESULT ERROR): exactly one is non-nil, and the callback
;; never runs before the operation returns.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup +agent-shell-pstack-arena nil
  "Candidate workspaces for pstack arena panels."
  :group 'tools)

(defcustom +agent-shell-pstack-arena-directory
  (expand-file-name "~/.local/state/pstack-arena/")
  "Directory holding one subdirectory per arena."
  :type 'directory)

(defcustom +agent-shell-pstack-arena-profiles nil
  "How arena candidates get set up in each repository.
Each entry is (MATCHER . PROFILE).  MATCHER is a directory, matching the
repository whose top level has the same true name, or a function called
with the top level, a true name ending in a slash.  The first entry that
matches applies.  PROFILE is a plist with any of these keys:

:shared   Paths symlinked from the lead into each candidate, readable
          but not writable.
:private  Paths cloned into each candidate with symlinks into the
          lead's checkout rewritten to the candidate's copy.
:copy     Ignored or untracked paths cloned like :private and left out
          of the candidate's result, such as generated config.
:files    An alist of (PATH . FUNCTION).  Before the agent starts, each
          FUNCTION is called with the candidate's `+agent-shell-sandbox'
          and returns the text written to PATH.  Left out of the result.
:write    Extra paths the candidate may write.
:read     Extra paths inside a read-denied folder the candidate may read.
:env      Extra NAME=VALUE environment entries, or a function of the
          sandbox returning them.  They win over the sandbox defaults.

Paths in :shared, :private, :copy and :files are relative to the top
level and stay inside it.  Each element of :write and :read is an
absolute path or a function of the sandbox returning a path or a list
of them."
  :type '(alist :key-type (choice directory function)
                :value-type (plist :key-type symbol :value-type sexp)))

(cl-defstruct (+agent-shell-arena-git-base
               (:constructor +agent-shell-arena-git--base)
               (:copier nil))
  "A lead's repository and the commit snapshotting its working tree."
  git-dir toplevel prefix head commit)

(cl-defstruct (+agent-shell-arena-git-role
               (:constructor +agent-shell-arena-git--role)
               (:copier nil))
  "A candidate's worktree, the dependency paths it got, and its result.
GIT-DIR is the worktree's own git directory, which holds its index.
CHECKOUT is a separate worktree holding the result for reading.
GENERATED lists the paths its profile's :copy and :files wrote."
  name worktree git-dir shared private ref commit state checkout generated)

(cl-defstruct (+agent-shell-arena-git-manifest
               (:constructor +agent-shell-arena-git--manifest)
               (:copier nil))
  "An arena's record.  Cleanup trusts nothing else."
  id directory git-dir toplevel prefix base roles)

(defconst +agent-shell-arena-git--manifest-version 1)

(defconst +agent-shell-arena-git--identity
  '("GIT_AUTHOR_NAME=pstack arena" "GIT_AUTHOR_EMAIL=pstack-arena@localhost"
    "GIT_COMMITTER_NAME=pstack arena"
    "GIT_COMMITTER_EMAIL=pstack-arena@localhost"))

(defconst +agent-shell-arena-git--agent-shell-exclude
  ":(exclude,glob)**/.agent-shell/**"
  "Pathspec leaving agent-shell transcripts out of every snapshot.")

;;;; Processes

(defun +agent-shell-arena-git--environment (extra)
  "Return `process-environment' plus EXTRA, without repository overrides."
  (append extra '("GIT_OPTIONAL_LOCKS=0" "GIT_TERMINAL_PROMPT=0")
          (seq-remove
           (lambda (entry)
             (string-match-p
              "\\`GIT_\\(DIR\\|WORK_TREE\\|INDEX_FILE\\|COMMON_DIR\\|\
OBJECT_DIRECTORY\\|NAMESPACE\\)=" entry))
           process-environment)))

(defun +agent-shell-arena-git--run (command directory env fail done
                                            &optional input)
  "Run COMMAND in DIRECTORY with ENV added to the environment.
Send it INPUT, when non-nil, on standard input.  Call DONE with its
output on success, otherwise FAIL with a message."
  (let* ((default-directory (file-name-as-directory directory))
         (process-environment (+agent-shell-arena-git--environment env))
         (out (generate-new-buffer " *pstack-arena*" t))
         (err (generate-new-buffer " *pstack-arena-stderr*" t))
         (stderr (make-pipe-process :name "pstack-arena-stderr" :buffer err
                                    :noquery t :sentinel #'ignore))
         (process
          (make-process
           :name "pstack-arena" :buffer out :stderr stderr :command command
           :coding 'utf-8-unix :connection-type 'pipe :noquery t
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (dotimes (_ 50)
                 (when (process-live-p stderr)
                   (accept-process-output stderr 0.01)))
               (let ((status (process-exit-status process))
                     (output (with-current-buffer out (buffer-string)))
                     (message (with-current-buffer err
                                (string-trim (buffer-string)))))
                 (delete-process stderr)
                 (kill-buffer out)
                 (kill-buffer err)
                 (if (and (eq (process-status process) 'exit) (zerop status))
                     (funcall done output)
                   (funcall fail (if (string-empty-p message)
                                     (format "exit status %s" status)
                                   message)))))))))
    (when input (process-send-string process input))
    (process-send-eof process)))

(defun +agent-shell-arena-git--git (git-dir work-tree args fail done
                                            &optional env input)
  "Run git ARGS on GIT-DIR and WORK-TREE, then call DONE or FAIL.
ENV adds environment entries; INPUT goes to standard input."
  (+agent-shell-arena-git--run
   (append (list "git" "--git-dir" git-dir "--work-tree" work-tree
                 "-c" "core.hooksPath=/dev/null" "-c" "core.fsmonitor=false")
           args)
   work-tree env
   (lambda (message) (funcall fail (format "git %s: %s" (car args) message)))
   done input))

(defun +agent-shell-arena-git--read (directory &rest args)
  "Return the output of git ARGS run in DIRECTORY, or nil if it fails.
Only for reads that stay fast however large the repository is."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory))
          (process-environment (+agent-shell-arena-git--environment nil)))
      (when (eql 0 (apply #'process-file "git" nil (list t nil) nil
                          "-c" "core.hooksPath=/dev/null"
                          "-c" "core.fsmonitor=false" args))
        (buffer-string)))))

(defun +agent-shell-arena-git--read-in (git-dir work-tree &rest args)
  "Return trimmed output of git ARGS on GIT-DIR and WORK-TREE, or nil."
  (when-let* ((output (apply #'+agent-shell-arena-git--read work-tree
                             "--git-dir" git-dir "--work-tree" work-tree
                             args)))
    (string-trim output)))

(defun +agent-shell-arena-git--later (callback &rest args)
  "Call CALLBACK with ARGS once the current command returns."
  (apply #'run-at-time 0 nil callback args))

;;;; Snapshots

(defun +agent-shell-arena-git--snapshot (git-dir work-tree seed excludes
                                                 restore fail done)
  "Call DONE with WORK-TREE's tree once two consecutive snapshots match.
Each snapshot fills a fresh temporary index by calling SEED with the index
file, FAIL and a continuation, then adds every file except EXCLUDES
pathspecs.  RESTORE, when non-nil, is (COMMIT . PATHS): those paths then
take COMMIT's content, or leave the tree if COMMIT lacks them.  Unlike
an exclude, a restored path may be ignored.  FAIL gets a message after
three snapshots that differ.

In a sparse checkout, skip-worktree entries keep their index content, and
files on disk outside the sparse patterns are added like any other."
  (cl-labels
      ((attempt (previous count)
         (let* ((directory (make-temp-file "pstack-arena-index-" t))
                (index (expand-file-name "index" directory))
                (env (list (concat "GIT_INDEX_FILE=" index)))
                (fail (lambda (message)
                        (delete-directory directory t)
                        (funcall fail message))))
           (funcall
            seed index fail
            (lambda ()
              (+agent-shell-arena-git--git
               git-dir work-tree (append '("add" "-A" "--sparse" "--" ".")
                                         excludes)
               fail
               (lambda (_)
                 (reset
                  env fail
                  (lambda ()
                    (+agent-shell-arena-git--git
                     git-dir work-tree '("write-tree") fail
                     (lambda (output)
                       (delete-directory directory t)
                       (let ((tree (string-trim output)))
                         (cond ((equal tree previous) (funcall done tree))
                               ((< count 3) (attempt tree (1+ count)))
                               (t (funcall fail "The files kept changing \
during capture; retry once they settle")))))
                     env))))
               env)))))
       (reset (env fail done)
         (if (null (cdr restore))
             (funcall done)
           (+agent-shell-arena-git--git
            git-dir work-tree
            (append (list "reset" "-q" (car restore) "--")
                    (mapcar (lambda (path) (concat ":(literal)" path))
                            (cdr restore)))
            fail (lambda (_) (funcall done)) env))))
    (attempt nil 1)))

(defun +agent-shell-arena-git--commit (git-dir work-tree tree parent message
                                               fail done)
  "Call DONE with a commit of TREE on PARENT with MESSAGE, or FAIL.
The commit needs no user identity and is never signed."
  (+agent-shell-arena-git--git
   git-dir work-tree
   (list "commit-tree" "--no-gpg-sign" "-p" parent "-m" message tree)
   fail (lambda (output) (funcall done (string-trim output)))
   +agent-shell-arena-git--identity))

;;;; Base

(defun +agent-shell-arena-git--locate (directory)
  "Return a base for the lead at DIRECTORY, without its commit.
Signal `user-error' when the repository cannot host an arena."
  (let* ((output (and (file-directory-p directory)
                      (+agent-shell-arena-git--read
                       directory "rev-parse" "--show-toplevel"
                       "--absolute-git-dir" "--show-prefix")))
         (lines (and output (split-string output "\n"))))
    (unless (and lines (not (string-empty-p (car lines))))
      (user-error "%s is not inside a git working tree" directory))
    (let* ((toplevel (file-name-as-directory (nth 0 lines)))
           (git-dir (nth 1 lines))
           (read (lambda (&rest args)
                   (apply #'+agent-shell-arena-git--read-in git-dir toplevel
                          args)))
           (head (funcall read "rev-parse" "--verify" "-q" "HEAD^{commit}")))
      (unless head
        (user-error "HEAD in %s is unborn; commit once before starting an \
arena" toplevel))
      (when (file-exists-p (expand-file-name ".gitmodules" toplevel))
        (user-error "%s has submodules, which arenas do not support"
                    toplevel))
      (+agent-shell-arena-git--base
       :git-dir git-dir :toplevel toplevel :prefix (nth 2 lines) :head head))))

(defun +agent-shell-arena-git--check-index (base fail done)
  "Call DONE unless BASE's index has gitlinks, else FAIL with the reason."
  (let ((toplevel (+agent-shell-arena-git-base-toplevel base)))
    (+agent-shell-arena-git--git
     (+agent-shell-arena-git-base-git-dir base) toplevel
     '("ls-files" "-z" "--format=%(objectmode)") fail
     (lambda (modes)
       (if (string-match-p "\\(?:\\`\\|\0\\)160000\0" modes)
           (funcall fail (format "%s has submodules, which arenas do not \
support" (directory-file-name toplevel)))
         (funcall done))))))

(defun +agent-shell-arena-git-capture-base (directory callback)
  "Snapshot the working tree of the lead at DIRECTORY as a base commit.
The commit's parent is HEAD and its tree holds tracked and untracked,
non-ignored files.  Call CALLBACK with a `+agent-shell-arena-git-base' and
nil, or nil and a message saying why there is no base."
  (let ((fail (lambda (message) (funcall callback nil message))))
    (condition-case err
        (let* ((base (+agent-shell-arena-git--locate directory))
               (git-dir (+agent-shell-arena-git-base-git-dir base))
               (toplevel (+agent-shell-arena-git-base-toplevel base))
               (head (+agent-shell-arena-git-base-head base))
               (real-index (expand-file-name "index" git-dir)))
          (+agent-shell-arena-git--check-index
           base fail
           (lambda ()
             (+agent-shell-arena-git--snapshot
              git-dir toplevel
              (lambda (index fail done)
                (if (file-exists-p real-index)
                    (+agent-shell-arena-git--run
                     (list "cp" real-index index) toplevel nil fail
                     (lambda (_) (funcall done)))
                  (+agent-shell-arena-git--git
                   git-dir toplevel (list "read-tree" head) fail
                   (lambda (_) (funcall done))
                   (list (concat "GIT_INDEX_FILE=" index)))))
              (list +agent-shell-arena-git--agent-shell-exclude) nil
              fail
              (lambda (tree)
                (+agent-shell-arena-git--commit
                 git-dir toplevel tree head "pstack arena base" fail
                 (lambda (commit)
                   (setf (+agent-shell-arena-git-base-commit base) commit)
                   (funcall callback base nil))))))))
      (user-error (+agent-shell-arena-git--later fail (cadr err))))))

;;;; Manifest

(defun +agent-shell-arena-git--manifest-file (directory)
  "Return the manifest file of the arena in DIRECTORY."
  (expand-file-name "manifest.json" directory))

(defun +agent-shell-arena-git-write-manifest (manifest)
  "Write MANIFEST to its arena directory, replacing any earlier copy."
  (let* ((directory (+agent-shell-arena-git-manifest-directory manifest))
         (file (+agent-shell-arena-git--manifest-file directory))
         (temporary (make-temp-file (expand-file-name ".manifest-" directory)))
         (json
          `((version . ,+agent-shell-arena-git--manifest-version)
            (id . ,(+agent-shell-arena-git-manifest-id manifest))
            (git_dir . ,(+agent-shell-arena-git-manifest-git-dir manifest))
            (toplevel . ,(+agent-shell-arena-git-manifest-toplevel manifest))
            (prefix . ,(+agent-shell-arena-git-manifest-prefix manifest))
            (base . ,(+agent-shell-arena-git-manifest-base manifest))
            (roles
             . ,(vconcat
                 (mapcar
                  (lambda (role)
                    `((role . ,(+agent-shell-arena-git-role-name role))
                      (worktree . ,(+agent-shell-arena-git-role-worktree role))
                      (git_dir
                       . ,(or (+agent-shell-arena-git-role-git-dir role) :null))
                      (shared
                       . ,(vconcat (+agent-shell-arena-git-role-shared role)))
                      (private
                       . ,(vconcat (+agent-shell-arena-git-role-private role)))
                      (ref . ,(or (+agent-shell-arena-git-role-ref role) :null))
                      (commit
                       . ,(or (+agent-shell-arena-git-role-commit role) :null))
                      (state
                       . ,(or (+agent-shell-arena-git-role-state role) :null))
                      (checkout
                       . ,(or (+agent-shell-arena-git-role-checkout role)
                              :null))
                      (generated
                       . ,(vconcat
                           (+agent-shell-arena-git-role-generated role)))))
                  (+agent-shell-arena-git-manifest-roles manifest)))))))
    (with-temp-file temporary
      (insert (json-serialize json) "\n"))
    (rename-file temporary file t)
    manifest))

(defun +agent-shell-arena-git-read-manifest (directory)
  "Return the manifest of the arena in DIRECTORY."
  (let ((json (with-temp-buffer
                (insert-file-contents
                 (+agent-shell-arena-git--manifest-file directory))
                (json-parse-buffer :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil))))
    (unless (eql (alist-get 'version json)
                 +agent-shell-arena-git--manifest-version)
      (error "Unknown arena manifest version in %s" directory))
    (+agent-shell-arena-git--manifest
     :id (alist-get 'id json)
     :directory (file-name-as-directory (expand-file-name directory))
     :git-dir (alist-get 'git_dir json)
     :toplevel (alist-get 'toplevel json)
     :prefix (alist-get 'prefix json)
     :base (alist-get 'base json)
     :roles (mapcar (lambda (role)
                      (+agent-shell-arena-git--role
                       :name (alist-get 'role role)
                       :worktree (alist-get 'worktree role)
                       :git-dir (alist-get 'git_dir role)
                       :shared (alist-get 'shared role)
                       :private (alist-get 'private role)
                       :ref (alist-get 'ref role)
                       :commit (alist-get 'commit role)
                       :state (alist-get 'state role)
                       :checkout (alist-get 'checkout role)
                       :generated (alist-get 'generated role)))
                    (alist-get 'roles json)))))

(defun +agent-shell-arena-git-arenas ()
  "Return the manifests of arenas under `+agent-shell-pstack-arena-directory'.
Unreadable manifests are reported and skipped."
  (let ((root +agent-shell-pstack-arena-directory))
    (when (file-directory-p root)
      (delq nil
            (mapcar
             (lambda (directory)
               (when (file-exists-p
                      (+agent-shell-arena-git--manifest-file directory))
                 (condition-case err
                     (+agent-shell-arena-git-read-manifest directory)
                   (error (message "Skipping arena %s: %s" directory
                                   (error-message-string err))
                          nil))))
             (directory-files root t "\\`[^.]"))))))

(defun +agent-shell-arena-git-role (manifest name)
  "Return the role called NAME in MANIFEST, or nil."
  (seq-find (lambda (role) (equal (+agent-shell-arena-git-role-name role) name))
            (+agent-shell-arena-git-manifest-roles manifest)))

;;;; Candidates

(defconst +agent-shell-arena-git--name-regexp
  "\\`[[:alnum:]][[:alnum:]_-]*\\'"
  "Names usable as a directory and a ref component.")

(defun +agent-shell-arena-git-create (base panel-id)
  "Create a directory for a new arena of PANEL-ID over BASE.
Write and return its manifest.  The arena id is PANEL-ID plus a random
suffix, so a repeated request never meets an earlier arena's leftovers."
  (unless (string-match-p +agent-shell-arena-git--name-regexp panel-id)
    (error "Invalid arena panel id: %S" panel-id))
  (let* ((root (file-name-as-directory
                (expand-file-name +agent-shell-pstack-arena-directory)))
         (id (format "%s-%08x" panel-id (random #x100000000)))
         (directory (file-name-as-directory (expand-file-name id root))))
    (make-directory root t)
    (make-directory directory)
    (+agent-shell-arena-git-write-manifest
     (+agent-shell-arena-git--manifest
      :id id :directory directory
      :git-dir (+agent-shell-arena-git-base-git-dir base)
      :toplevel (+agent-shell-arena-git-base-toplevel base)
      :prefix (+agent-shell-arena-git-base-prefix base)
      :base (+agent-shell-arena-git-base-commit base)))))

(defun +agent-shell-arena-git--matches-p (matcher toplevel)
  "Return whether profile MATCHER selects the repository at TOPLEVEL."
  (cond ((stringp matcher)
         (equal (file-truename (file-name-as-directory matcher)) toplevel))
        ((functionp matcher) (funcall matcher toplevel))
        (t (user-error "Arena profile matcher must be a directory or a \
function, not %S" matcher))))

(defun +agent-shell-arena-git--check-profile (toplevel profile)
  "Return PROFILE, the arena profile for TOPLEVEL, with :env as a list.
Signal `user-error' naming the key of any malformed value."
  (let ((fail (lambda (key expected value)
                (user-error "Arena profile for %s: %s must be %s, not %S"
                            toplevel key expected value)))
        (relative-p (lambda (path)
                      (and (stringp path) (not (string-empty-p path))
                           (not (file-name-absolute-p path))
                           (not (member ".." (split-string path "/"))))))
        (under-p (lambda (path parent)
                   (or (equal path parent)
                       (string-prefix-p (file-name-as-directory parent)
                                        path)))))
    (unless (and (proper-list-p profile) (cl-evenp (length profile)))
      (funcall fail "the profile" "a plist" profile))
    (cl-loop for (key value) on profile by #'cddr
             do (pcase key
                  ((or :shared :private :copy)
                   (unless (and (proper-list-p value)
                                (seq-every-p relative-p value))
                     (funcall fail key "a list of paths inside the top level"
                              value)))
                  (:files
                   (unless (and (proper-list-p value)
                                (seq-every-p
                                 (lambda (entry)
                                   (and (consp entry)
                                        (funcall relative-p (car entry))
                                        (functionp (cdr entry))))
                                 value))
                     (funcall fail key "an alist of paths inside the top \
level to functions" value)))
                  ((or :write :read)
                   (unless (and (proper-list-p value)
                                (seq-every-p (lambda (path)
                                               (or (functionp path)
                                                   (and (stringp path)
                                                        (file-name-absolute-p
                                                         path))))
                                             value))
                     (funcall fail key "a list of absolute paths or \
functions" value)))
                  (:env
                   (unless (or (functionp value)
                               (and (proper-list-p value)
                                    (seq-every-p #'stringp value)))
                     (funcall fail key "a list of NAME=VALUE strings or a \
function" value)))
                  (_ (user-error "Arena profile for %s: unknown key %S"
                                 toplevel key))))
    (dolist (path (append (plist-get profile :copy)
                          (mapcar #'car (plist-get profile :files))))
      (dolist (shared (plist-get profile :shared))
        (when (funcall under-p path shared)
          (funcall fail ":copy and :files" "outside :shared paths" path))))
    (let ((env (plist-get profile :env)))
      (if (functionp env)
          (plist-put (copy-sequence profile) :env (list env))
        profile))))

(defun +agent-shell-arena-git-profile (toplevel)
  "Return the validated arena profile for the repository at TOPLEVEL.
See `+agent-shell-pstack-arena-profiles'; nil when no entry matches."
  (let* ((toplevel (file-truename (file-name-as-directory toplevel)))
         (entry (seq-find (lambda (entry)
                            (+agent-shell-arena-git--matches-p (car entry)
                                                               toplevel))
                          +agent-shell-pstack-arena-profiles)))
    (when entry
      (+agent-shell-arena-git--check-profile toplevel (cdr entry)))))

(defun +agent-shell-arena-git-write-files (worktree files sandbox)
  "Write each of FILES, a profile's :files, into WORKTREE for SANDBOX."
  (pcase-dolist (`(,path . ,function) files)
    (let ((text (funcall function sandbox))
          (file (expand-file-name path worktree)))
      (unless (stringp text)
        (user-error "Arena profile :files function for %s returned %S, not \
a string" path text))
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file nil 'silent)))))

(defun +agent-shell-arena-git-directory (manifest role)
  "Return ROLE's working directory in MANIFEST: its worktree plus the prefix."
  (expand-file-name (+agent-shell-arena-git-manifest-prefix manifest)
                    (+agent-shell-arena-git-role-worktree role)))

(defun +agent-shell-arena-git-common-dir (manifest)
  "Return the git directory MANIFEST's worktrees share, or signal."
  (let ((git-dir (+agent-shell-arena-git-manifest-git-dir manifest)))
    (or (+agent-shell-arena-git--read-in
         git-dir (+agent-shell-arena-git-manifest-toplevel manifest)
         "rev-parse" "--path-format=absolute" "--git-common-dir")
        (error "No common git directory for %s" git-dir))))

(defun +agent-shell-arena-git--add-role (manifest name)
  "Add role NAME to MANIFEST, write the manifest and return the role.
The role lists the profile's paths present in the lead and the paths its
:files will write."
  (unless (string-match-p +agent-shell-arena-git--name-regexp name)
    (error "Invalid arena role name: %S" name))
  (when (+agent-shell-arena-git-role manifest name)
    (error "Arena %s already has a role %s"
           (+agent-shell-arena-git-manifest-id manifest) name))
  (let* ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (profile (+agent-shell-arena-git-profile toplevel))
         (present (lambda (key)
                    (seq-filter (lambda (path)
                                  (file-exists-p
                                   (expand-file-name path toplevel)))
                                (plist-get profile key))))
         (role (+agent-shell-arena-git--role
                :name name
                :worktree (file-name-as-directory
                           (expand-file-name
                            name
                            (+agent-shell-arena-git-manifest-directory
                             manifest)))
                :shared (funcall present :shared)
                :private (funcall present :private)
                :generated (append (funcall present :copy)
                                   (mapcar #'car (plist-get profile
                                                            :files))))))
    (setf (+agent-shell-arena-git-manifest-roles manifest)
          (append (+agent-shell-arena-git-manifest-roles manifest)
                  (list role)))
    (+agent-shell-arena-git-write-manifest manifest)
    role))

(defun +agent-shell-arena-git-provision (manifest name callback)
  "Add candidate NAME to MANIFEST's arena: a worktree at the base commit.
Link and clone its dependency paths and clone the generated paths the
lead has and the worktree lacks, then call CALLBACK with the candidate's
working directory and nil, or nil and a message.

Run from the lead's git directory, git worktree add copies the lead's
sparse-checkout patterns and per-worktree config before it checks out, so
the candidate gets the same sparse checkout."
  (let ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
        (fail (lambda (message) (funcall callback nil message))))
    (condition-case err
        ;; The role is on disk before its worktree, so cleanup finds a
        ;; partly provisioned candidate.
        (let* ((role (+agent-shell-arena-git--add-role manifest name))
               (worktree (+agent-shell-arena-git-role-worktree role)))
          (+agent-shell-arena-git--git
           (+agent-shell-arena-git-manifest-git-dir manifest) toplevel
           (list "worktree" "add" "--detach" (directory-file-name worktree)
                 (+agent-shell-arena-git-manifest-base manifest))
           fail
           (lambda (_)
             (condition-case err
                 (progn
                   (setf (+agent-shell-arena-git-role-git-dir role)
                         (or (+agent-shell-arena-git--read-in
                              (expand-file-name ".git" worktree) worktree
                              "rev-parse" "--absolute-git-dir")
                             (error "No git directory for %s" worktree)))
                   (+agent-shell-arena-git-write-manifest manifest)
                   (dolist (path (+agent-shell-arena-git-role-shared role))
                     (let ((link (directory-file-name
                                  (expand-file-name path worktree))))
                       (make-directory (file-name-directory link) t)
                       (make-symbolic-link
                        (directory-file-name (expand-file-name path toplevel))
                        link)))
                   (+agent-shell-arena-git--clone
                    toplevel worktree
                    (append
                     (+agent-shell-arena-git-role-private role)
                     (seq-filter
                      (lambda (path)
                        (and (file-exists-p (expand-file-name path toplevel))
                             (not (file-exists-p
                                   (expand-file-name path worktree)))))
                      (+agent-shell-arena-git-role-generated role)))
                    fail
                    (lambda ()
                      (let ((directory (+agent-shell-arena-git-directory
                                        manifest role)))
                        (make-directory directory t)
                        (funcall callback directory nil)))))
               (error (funcall fail (error-message-string err)))))))
      (error (+agent-shell-arena-git--later fail (error-message-string err))))))

(defun +agent-shell-arena-git--clone (toplevel worktree paths fail done)
  "Clone each of PATHS from TOPLEVEL into WORKTREE, then call DONE.
Symlinks in a clone that point into TOPLEVEL are rewritten to point at
the same path in WORKTREE.  Call FAIL with a message on failure."
  (if (null paths)
      (funcall done)
    (let ((source (directory-file-name
                   (expand-file-name (car paths) toplevel)))
          (target (directory-file-name
                   (expand-file-name (car paths) worktree))))
      (make-directory (file-name-directory target) t)
      (+agent-shell-arena-git--run
       (list "cp" "-c" "-R" source target) worktree nil fail
       (lambda (_)
         (+agent-shell-arena-git--run
          (list "find" target "-type" "l" "-print0") worktree nil fail
          (lambda (output)
            (dolist (link (split-string output "\0" t))
              (let ((destination (file-symlink-p link)))
                (when (and destination (string-prefix-p toplevel destination))
                  (make-symbolic-link
                   (concat worktree (substring destination (length toplevel)))
                   link t))))
            (+agent-shell-arena-git--clone toplevel worktree (cdr paths)
                                           fail done))))))))

(defun +agent-shell-arena-git-capture (manifest name callback)
  "Commit candidate NAME's worktree in MANIFEST as a child of the base.
Leave out agent-shell transcripts, the shared dependency links and the
paths its profile generated, which keep their base content.  Point
refs/pstack-arena/ID/NAME at the commit and record it in the manifest.
Call CALLBACK with the commit and nil, or nil and a message.

The snapshot starts from the base's tree carrying the stat data of the
worktree's own index, so it hashes only files changed since checkout."
  (let* ((role (+agent-shell-arena-git-role manifest name))
         (git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
         (base (+agent-shell-arena-git-manifest-base manifest))
         (fail (lambda (message) (funcall callback nil message))))
    (if (not role)
        (+agent-shell-arena-git--later fail (format "No arena role %s" name))
      (let* ((worktree (+agent-shell-arena-git-role-worktree role))
             (ref (+agent-shell-arena-git--ref manifest name))
             (git-dir (or (+agent-shell-arena-git-role-git-dir role) git-dir))
             (own-index (expand-file-name "index" git-dir)))
        (+agent-shell-arena-git--snapshot
         git-dir worktree
         (lambda (index fail done)
           (let ((read-tree
                  (lambda (_)
                    (+agent-shell-arena-git--git
                     git-dir worktree (list "read-tree" "--reset" base) fail
                     (lambda (_) (funcall done))
                     (list (concat "GIT_INDEX_FILE=" index))))))
             ;; --reset keeps an entry's stat data while its content
             ;; matches the base, and drops any unmerged entries.
             (if (file-exists-p own-index)
                 (+agent-shell-arena-git--run (list "cp" "-p" own-index index)
                                              worktree nil fail read-tree)
               (funcall read-tree nil))))
         (cons +agent-shell-arena-git--agent-shell-exclude
               (mapcar (lambda (path) (concat ":(exclude,literal)" path))
                       (+agent-shell-arena-git-role-shared role)))
         (cons base (+agent-shell-arena-git-role-generated role))
         fail
         (lambda (tree)
           (+agent-shell-arena-git--commit
            git-dir worktree tree base
            (format "pstack arena %s %s"
                    (+agent-shell-arena-git-manifest-id manifest) name)
            fail
            (lambda (commit)
              (+agent-shell-arena-git--git
               git-dir worktree (list "update-ref" ref commit) fail
               (lambda (_)
                 (setf (+agent-shell-arena-git-role-ref role) ref
                       (+agent-shell-arena-git-role-commit role) commit)
                 (+agent-shell-arena-git-write-manifest manifest)
                 (funcall callback commit nil)))))))))))

(defun +agent-shell-arena-git--ref (manifest name)
  "Return the result ref of role NAME in MANIFEST."
  (format "refs/pstack-arena/%s/%s"
          (+agent-shell-arena-git-manifest-id manifest) name))

(defun +agent-shell-arena-git--captured (manifest name callback)
  "Return captured role NAME of MANIFEST, or arrange CALLBACK's error."
  (let ((role (+agent-shell-arena-git-role manifest name)))
    (if (and role (+agent-shell-arena-git-role-commit role))
        role
      (+agent-shell-arena-git--later callback nil
                                     (format "Arena role %s has no result"
                                             name))
      nil)))

(defun +agent-shell-arena-git-diff (manifest name callback)
  "Compare captured role NAME of MANIFEST with the base.
Call CALLBACK with (:changed BOOL :stat TEXT :outside PATHS :lfs PATHS)
and nil, or nil and a message.  :changed says whether the trees differ;
:stat is the output of git diff --stat; :outside lists the changed paths
outside the lead's sparse-checkout patterns, and :lfs those the lead's
attributes give to Git LFS."
  (when-let* ((role (+agent-shell-arena-git--captured manifest name callback)))
    (let ((git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
          (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
          (base (+agent-shell-arena-git-manifest-base manifest))
          (commit (+agent-shell-arena-git-role-commit role))
          (fail (lambda (message) (funcall callback nil message))))
      (+agent-shell-arena-git--git
       git-dir toplevel
       (list "rev-parse" (concat base "^{tree}") (concat commit "^{tree}"))
       fail
       (lambda (trees)
         (+agent-shell-arena-git--git
          git-dir toplevel (list "diff" "--stat" base commit) fail
          (lambda (stat)
            (+agent-shell-arena-git--outside
             git-dir toplevel base commit fail
             (lambda (outside)
               (+agent-shell-arena-git--git
                git-dir toplevel
                (list "diff" "--name-only" "-z" base commit "--"
                      ":(attr:filter=lfs)")
                fail
                (lambda (lfs)
                  (funcall callback
                           (list :changed (let ((trees (split-string trees)))
                                            (not (equal (car trees)
                                                        (cadr trees))))
                                 :stat (string-trim-right stat)
                                 :outside outside
                                 :lfs (split-string lfs "\0" t))
                           nil))))))))))))

(defun +agent-shell-arena-git--outside (git-dir toplevel base commit fail
                                                done)
  "Call DONE with the paths changed from BASE to COMMIT outside the sparse
patterns of the checkout at GIT-DIR and TOPLEVEL; nil when it is not
sparse.  Call FAIL with a message on failure."
  (if (not (equal (+agent-shell-arena-git--read-in
                   git-dir toplevel "config" "--type=bool"
                   "core.sparseCheckout")
                  "true"))
      (funcall done nil)
    (+agent-shell-arena-git--git
     git-dir toplevel (list "diff" "--name-only" "-z" base commit) fail
     (lambda (changed)
       (+agent-shell-arena-git--git
        git-dir toplevel '("sparse-checkout" "check-rules" "-z") fail
        (lambda (output)
          (let ((inside (make-hash-table :test #'equal)))
            (dolist (path (split-string output "\0" t))
              (puthash path t inside))
            (funcall done (seq-remove (lambda (path) (gethash path inside))
                                      (split-string changed "\0" t)))))
        nil changed)))))

(defun +agent-shell-arena-git-checkout (manifest name callback)
  "Check captured role NAME of MANIFEST out in a worktree of its own.
The worktree is detached at the role's result commit, under the arena's
checkouts/ folder, and recorded in the manifest so cleanup removes it.
Call CALLBACK with its path and nil, or nil and a message."
  (when-let* ((role (+agent-shell-arena-git--captured manifest name callback)))
    (let ((checkout (file-name-as-directory
                     (expand-file-name
                      name (expand-file-name
                            "checkouts"
                            (+agent-shell-arena-git-manifest-directory
                             manifest))))))
      (make-directory (file-name-directory (directory-file-name checkout)) t)
      ;; Recorded first, so cleanup finds a checkout that failed halfway.
      (setf (+agent-shell-arena-git-role-checkout role) checkout)
      (+agent-shell-arena-git-write-manifest manifest)
      (+agent-shell-arena-git--git
       (+agent-shell-arena-git-manifest-git-dir manifest)
       (+agent-shell-arena-git-manifest-toplevel manifest)
       (list "worktree" "add" "--detach" (directory-file-name checkout)
             (+agent-shell-arena-git-role-commit role))
       (lambda (message) (funcall callback nil message))
       (lambda (_) (funcall callback checkout nil))))))

;;;; Cleanup

(defun +agent-shell-arena-git-clean (manifest callback)
  "Remove MANIFEST's worktrees, checkouts, private clones and result refs.
A worktree the repository does not list, or a ref that no longer points
at the recorded commit, stays in place.  When no worktree stays, delete
the arena directory too.  Call CALLBACK with a list of messages naming
what stayed, and nil; or nil and a message."
  (let* ((git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
         (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (fail (lambda (message) (funcall callback nil message)))
         (left nil)
         (worktree-left nil))
    (+agent-shell-arena-git--git
     git-dir toplevel '("worktree" "list" "--porcelain" "-z") fail
     (lambda (output)
       (let ((registered
              (delq nil (mapcar (lambda (field)
                                  (when (string-prefix-p "worktree " field)
                                    (file-truename
                                     (file-name-as-directory
                                      (substring field (length "worktree "))))))
                                (split-string output "\0" t)))))
         (cl-labels
             ((next (roles)
                (if roles
                    (clean-role (car roles) (lambda () (next (cdr roles))))
                  (unless worktree-left
                    (delete-directory
                     (+agent-shell-arena-git-manifest-directory manifest) t))
                  (funcall callback (nreverse left) nil)))
              (clean-role (role done)
                (remove-trees
                 (cons (cons (+agent-shell-arena-git-role-worktree role)
                             (+agent-shell-arena-git-role-private role))
                       (when-let* ((checkout
                                    (+agent-shell-arena-git-role-checkout
                                     role)))
                         (list (list checkout))))
                 (lambda () (clean-ref role done))))
              (remove-trees (trees done)
                (if (null trees)
                    (funcall done)
                  (let ((tree (caar trees))
                        (next (lambda () (remove-trees (cdr trees) done))))
                    (cond
                     ((member (file-truename tree) registered)
                      (remove-private
                       tree (cdar trees)
                       (lambda ()
                         (+agent-shell-arena-git--git
                          git-dir toplevel
                          (list "worktree" "remove" "--force"
                                (directory-file-name tree))
                          fail (lambda (_) (funcall next))))))
                     ((file-exists-p tree)
                      (setq worktree-left t)
                      (push (format "%s is not a registered worktree; left \
in place" tree)
                            left)
                      (funcall next))
                     (t (funcall next))))))
              (remove-private (worktree paths done)
                (if (null paths)
                    (funcall done)
                  (+agent-shell-arena-git--run
                   (append '("rm" "-rf" "--")
                           (mapcar (lambda (path)
                                     (expand-file-name path worktree))
                                   paths))
                   toplevel nil fail (lambda (_) (funcall done)))))
              (clean-ref (role done)
                (let* ((ref (+agent-shell-arena-git--ref
                             manifest (+agent-shell-arena-git-role-name role)))
                       (commit (+agent-shell-arena-git-role-commit role))
                       (current (+agent-shell-arena-git--read-in
                                 git-dir toplevel "rev-parse" "--verify" "-q"
                                 ref)))
                  (cond
                   ((null current) (funcall done))
                   ((equal current commit)
                    (+agent-shell-arena-git--git
                     git-dir toplevel (list "update-ref" "-d" ref commit) fail
                     (lambda (_) (funcall done))))
                   (t
                    (push (format "%s points at %s, not the recorded %s; left \
in place" ref current (or commit "nothing"))
                          left)
                    (funcall done))))))
           (next (+agent-shell-arena-git-manifest-roles manifest))))))))

(provide '+agent-shell-arena-git)
;;; +agent-shell-arena-git.el ends here
