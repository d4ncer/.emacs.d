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
(require '+agent-shell-sandbox)

(defgroup +agent-shell-pstack-arena nil
  "Candidate workspaces for pstack arena panels."
  :group 'tools)

(defcustom +agent-shell-arena-remote-host-kinds
  '((coder :exec +agent-shell-arena-remote--coder-exec
           :wake +agent-shell-arena-remote--coder-wake
           :extend +agent-shell-arena-remote--coder-extend
           :label +agent-shell-arena-remote--coder-label)
    (ssh :exec +agent-shell-arena-remote--ssh-exec
         :wake nil :extend nil :label +agent-shell-arena-remote--ssh-label)
    (local :exec +agent-shell-arena-remote--local-exec
           :wake nil :extend nil :label +agent-shell-arena-remote--local-label
           :local t))
  "Host kinds accepted by arena profiles and remote records.
Each entry is (KIND . PLIST).  :exec is a function of NAME and a command
argument list, returning the local command that runs it on the host in
its state folder.  :label is a function of NAME returning message text.
:extend is nil or a function of NAME, SECONDS and CALLBACK, called with
nil on success or an error message.

:wake is nil or a function of NAME and CALLBACK.  It calls CALLBACK with
(EVENT DETAIL NEXT), where EVENT is `ready', `wait', `retry', `starting',
`check' or `error'.  DETAIL describes a wait or error.  NEXT is a function
of no arguments that continues the wake check, retaining backend state.
The submission controls retries, timeout and cancellation.  See the
remote host documentation for the event meanings.

:local, when non-nil, makes NAME an absolute local state directory,
uses its ready.json and starts the runner with nohup instead of systemd.
The remote library implements the default callbacks; defining this
option here lets profile validation run without loading that library."
  :group '+agent-shell-pstack-arena
  :type '(alist :key-type symbol :value-type plist))

(defun +agent-shell-arena-git-host-kinds ()
  "Return the registered arena host kinds, in registry order."
  (mapcar #'car +agent-shell-arena-remote-host-kinds))

(defun +agent-shell-arena-git-host-description ()
  "Return the registered host forms for validation messages."
  (mapconcat (lambda (kind) (format "(%s . NAME)" kind))
             (+agent-shell-arena-git-host-kinds) ", "))

(defcustom +agent-shell-pstack-arena-directory
  (expand-file-name "~/.local/state/pstack-arena/")
  "Directory holding one subdirectory per arena."
  :type 'directory)

(defcustom +agent-shell-pstack-arena-profiles nil
  "How arena candidates and pstack delegates get set up in each repository.
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
:connect  Unix socket paths the candidate may connect to, such as a
          build tool's tunnel to its remote cache at
          /private/tmp/NAME-tunnel.sock.  Every other unix socket but
          mDNSResponder's is refused, and none may be in the Emacs
          server's socket folder, `server-socket-dir'.
:env      Extra NAME=VALUE environment entries, or a function of the
          sandbox returning them.  They win over the sandbox defaults.
:host     A host that runs the whole arena in place of this Emacs:
          (coder . NAME) for the Coder workspace NAME, reached as
          ssh coder.NAME; (ssh . HOST) for a plain SSH host; or
          (local . DIRECTORY), which runs the host's side as local
          processes with DIRECTORY as its state folder.  Kinds come from
          `+agent-shell-arena-remote-host-kinds'.  See
          docs/AGENT-SHELL-PSTACK-ARENA-REMOTE.md.
:host-directory  The repository's checkout on the host, such as
          \"~/src/big-repo\".  Required with :host.
:delegate A plist of how pstack delegates run here; see
          `+agent-shell-arena-git-delegate-options'.  :isolation is
          `worktree' (the default) or `branch'.  `worktree' gives a
          delegate a worktree of its own on a new branch, provisioned
          from :shared, :private and :copy, with the lead's copies of
          the :files paths.  `branch' has it work on a new branch in
          the lead's own checkout instead, for a repository too large
          for a second worktree; Emacs refuses the request unless the
          checkout is clean.  A delegate runs on the profile's :host
          when it names one, in a worktree there; :host `local' keeps
          it on this machine.  :sandbox t confines it to its worktree,
          where it cannot commit: Emacs makes its one commit, as your
          git identity, from the message its final response proposes.

Paths in :shared, :private, :copy and :files are relative to the top
level and stay inside it.  Each element of :write and :read is an
absolute path, a (regexp . RE) pattern, or a function of the sandbox
returning one of those or a list of them.  A pattern allows every true
name RE matches, for tools that make randomly named files in a shared
folder, such as diff's /private/tmp/diff.XXXXXXXX:

  (regexp . \"^/private/tmp/diff\\\\.[A-Za-z0-9]+$\")

RE is a sandbox-exec regex.  It must start with ^ and the literal true
name of a folder below the root, with no alternative outside
parentheses, so it can never match / or the whole filesystem.
`+agent-shell-sandbox-regexp-quote' quotes a path computed in a
function, such as the per-user temp folder.

Each element of :connect is an absolute socket path or a function of
the sandbox returning one or a list of them.  The socket need not exist
yet, and its true name is what the profile allows."
  :type '(alist :key-type (choice directory function)
                :value-type (plist :key-type symbol :value-type sexp)))

(cl-defstruct (+agent-shell-arena-git-base
               (:constructor +agent-shell-arena-git--base)
               (:copier nil))
  "A lead's repository and the commit snapshotting its working tree.
SPARSE, when non-nil, is (:cone BOOL :patterns LINES): sparse-checkout
patterns each candidate's worktree gets in place of the repository's
own, for a base made elsewhere; see `+agent-shell-arena-git-base-at'.
BRANCH is the full ref HEAD was on when the repository was located, or
nil when it was detached."
  git-dir toplevel prefix head commit sparse branch)

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
  "An arena's record.  Cleanup trusts nothing else.
SPARSE, when non-nil, holds sparse-checkout patterns as a base's SPARSE
does; each new worktree then gets them in place of the repository's own.
It is not saved.  DELEGATE holds a `+agent-shell-arena-git-delegate'
when the record is a pstack delegate's, whose BASE is the commit it
started from."
  id directory git-dir toplevel prefix base roles sparse delegate)

(cl-defstruct (+agent-shell-arena-git-delegate
               (:constructor +agent-shell-arena-git--delegate)
               (:copier nil))
  "A pstack delegate's part of its manifest.
ISOLATION is `worktree' or `branch'.  In branch isolation, ORIGIN is
\(branch . FULL-REF) or (detached . OID), where the lead's checkout was
before the delegate's branch took it over.  COMMITS lists (OID . SUBJECT)
from the base to the branch tip, oldest first.  RESTORE is nil until
Emacs tries to return the checkout, then `restored', `dirty', `changed',
`missing' or `unneeded'; a worktree never needs it.  NOTE says what Emacs
found, for the user.  SANDBOX says the delegate ran confined, so Emacs
makes its one commit.  FINALIZATION is nil until that is tried, then a
plist whose :state is `prepared' or `committed', with the commit's fixed
:parent, :tree, :message, :name, :email, :author-date, :committer-date
and, once made, :oid; or `empty' or `blocked', with a :reason.  DIRTY
lists the worktree's uncommitted paths when it was last collected."
  isolation origin commits restore note sandbox finalization dirty)

(defconst +agent-shell-arena-git--manifest-version 2
  "Layout this code writes manifests in.  Version 1 lacks a delegate's
sandbox and finalization, which then read as nil.")

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
       :git-dir git-dir :toplevel toplevel :prefix (nth 2 lines) :head head
       :branch (funcall read "symbolic-ref" "-q" "HEAD")))))

(defun +agent-shell-arena-git-sparse (base)
  "Return the sparse-checkout patterns of BASE's checkout, or nil.
The value is (:cone BOOL :patterns LINES)."
  (let ((read (lambda (&rest args)
                (apply #'+agent-shell-arena-git--read-in
                       (+agent-shell-arena-git-base-git-dir base)
                       (+agent-shell-arena-git-base-toplevel base) args))))
    (when (equal (funcall read "config" "--type=bool" "core.sparseCheckout")
                 "true")
    (let ((file (funcall read "rev-parse" "--path-format=absolute"
                         "--git-path" "info/sparse-checkout")))
      (list :cone (equal (funcall read "config" "--type=bool"
                                  "core.sparseCheckoutCone")
                         "true")
            :patterns (and file (file-readable-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (split-string (buffer-string) "\n" t))))))))

(defun +agent-shell-arena-git-base-at (directory commit prefix sparse)
  "Return a base for COMMIT, which the repository at DIRECTORY holds.
PREFIX is the lead's subdirectory and SPARSE its sparse-checkout
patterns, as in `+agent-shell-arena-git-base'.  Signal `user-error'
when the repository cannot host an arena or lacks COMMIT."
  (let* ((base (+agent-shell-arena-git--locate directory))
         (resolved (+agent-shell-arena-git--read-in
                    (+agent-shell-arena-git-base-git-dir base)
                    (+agent-shell-arena-git-base-toplevel base)
                    "rev-parse" "--verify" "-q" (concat commit "^{commit}"))))
    (unless (equal resolved commit)
      (user-error "%s lacks commit %s" directory commit))
    (setf (+agent-shell-arena-git-base-head base) commit
          (+agent-shell-arena-git-base-commit base) commit
          (+agent-shell-arena-git-base-prefix base) prefix
          (+agent-shell-arena-git-base-sparse base) sparse)
    base))

(defun +agent-shell-arena-git-command (manifest args callback &optional input)
  "Run git ARGS in MANIFEST's repository, sending INPUT when non-nil.
Call CALLBACK with the output and nil, or nil and a message."
  (+agent-shell-arena-git--git
   (+agent-shell-arena-git-manifest-git-dir manifest)
   (+agent-shell-arena-git-manifest-toplevel manifest)
   args (lambda (message) (funcall callback nil message))
   (lambda (output) (funcall callback output nil))
   nil input))

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

(defun +agent-shell-arena-git-repository (directory callback)
  "Locate the repository at DIRECTORY as a base at HEAD, without a snapshot.
Call CALLBACK with a `+agent-shell-arena-git-base' whose COMMIT is HEAD
and nil, or nil and a message, for the reasons an arena's base fails."
  (let ((fail (lambda (message) (funcall callback nil message))))
    (condition-case err
        (let ((base (+agent-shell-arena-git--locate directory)))
          (+agent-shell-arena-git--check-index
           base fail
           (lambda ()
             (setf (+agent-shell-arena-git-base-commit base)
                   (+agent-shell-arena-git-base-head base))
             (funcall callback base nil))))
      (user-error (+agent-shell-arena-git--later fail (cadr err))))))

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
                     (list "/bin/cp" real-index index) toplevel nil fail
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
                      (worktree
                       . ,(or (+agent-shell-arena-git-role-worktree role)
                              :null))
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
                  (+agent-shell-arena-git-manifest-roles manifest))))
            ,@(when-let* ((delegate (+agent-shell-arena-git-manifest-delegate
                                     manifest)))
                `((delegate
                   . ,(+agent-shell-arena-git--delegate-json delegate)))))))
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
    (unless (memql (alist-get 'version json)
                   (number-sequence 1 +agent-shell-arena-git--manifest-version))
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
                    (alist-get 'roles json))
     :delegate (when-let* ((delegate (alist-get 'delegate json)))
                 (+agent-shell-arena-git--read-delegate delegate)))))

(defun +agent-shell-arena-git--delegate-json (delegate)
  "Return DELEGATE as an alist for `json-serialize'."
  (let ((origin (+agent-shell-arena-git-delegate-origin delegate))
        (symbol (lambda (value) (if value (symbol-name value) :null))))
    `((isolation . ,(funcall symbol
                             (+agent-shell-arena-git-delegate-isolation
                              delegate)))
      (origin . ,(if origin (list (cons (car origin) (cdr origin))) :null))
      (commits . ,(vconcat
                   (mapcar (lambda (commit) (vector (car commit) (cdr commit)))
                           (+agent-shell-arena-git-delegate-commits
                            delegate))))
      (restore . ,(funcall symbol
                           (+agent-shell-arena-git-delegate-restore delegate)))
      (note . ,(or (+agent-shell-arena-git-delegate-note delegate) :null))
      (sandbox . ,(if (+agent-shell-arena-git-delegate-sandbox delegate) t
                    :false))
      (dirty . ,(vconcat (+agent-shell-arena-git-delegate-dirty delegate)))
      (finalization
       . ,(if-let* ((finalization (+agent-shell-arena-git-delegate-finalization
                                   delegate)))
              (cl-loop for (key value) on finalization by #'cddr
                       collect (cons (intern (substring (symbol-name key) 1))
                                     (if (symbolp value) (symbol-name value)
                                       value)))
            :null)))))

(defun +agent-shell-arena-git--read-delegate (json)
  "Return the `+agent-shell-arena-git-delegate' that JSON, an alist, holds."
  (let ((symbol (lambda (value) (and value (intern value))))
        (origin (car (alist-get 'origin json))))
    (+agent-shell-arena-git--delegate
     :isolation (funcall symbol (alist-get 'isolation json))
     :origin (when (memq (car origin) '(branch detached)) origin)
     :commits (mapcar (lambda (commit) (cons (car commit) (cadr commit)))
                      (alist-get 'commits json))
     :restore (funcall symbol (alist-get 'restore json))
     :note (alist-get 'note json)
     :sandbox (and (alist-get 'sandbox json) t)
     :dirty (alist-get 'dirty json)
     :finalization
     (when-let* ((finalization (alist-get 'finalization json)))
       (cl-loop for (key . value) in finalization
                append (list (intern (format ":%s" key))
                             (if (eq key 'state) (intern value) value)))))))

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

(defun +agent-shell-arena-git-create (base panel-id &optional delegate)
  "Create a directory for a new arena of PANEL-ID over BASE.
Write and return its manifest, holding DELEGATE for a pstack delegate.
The arena id is PANEL-ID plus a random suffix, so a repeated request
never meets an earlier arena's leftovers."
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
      :base (+agent-shell-arena-git-base-commit base)
      :sparse (+agent-shell-arena-git-base-sparse base)
      :delegate delegate))))

(defun +agent-shell-arena-git--matches-p (matcher toplevel)
  "Return whether profile MATCHER selects the repository at TOPLEVEL."
  (cond ((stringp matcher)
         (equal (file-truename (file-name-as-directory matcher)) toplevel))
        ((functionp matcher) (funcall matcher toplevel))
        (t (user-error "Arena profile matcher must be a directory or a \
function, not %S" matcher))))

(defun +agent-shell-arena-git-host-p (value)
  "Return whether VALUE names a host as a profile's :host does."
  (and (consp value)
       (assq (car value) +agent-shell-arena-remote-host-kinds)
       (stringp (cdr value))
       (if (plist-get (alist-get (car value)
                                +agent-shell-arena-remote-host-kinds) :local)
           (file-name-absolute-p (cdr value))
         (string-match-p "\\`[[:alnum:]][[:alnum:]._@-]*\\'" (cdr value)))))

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
                  ((or :write :read :connect)
                   (unless (proper-list-p value)
                     (funcall fail key (if (eq key :connect)
                                           "a list of socket paths or \
functions"
                                         "a list of absolute paths, (regexp \
. RE) patterns or functions")
                              value))
                   (dolist (element value)
                     (unless (functionp element)
                       (when-let* ((expected
                                    (funcall
                                     (if (eq key :connect)
                                         #'+agent-shell-sandbox-socket-error
                                       #'+agent-shell-sandbox-allowance-error)
                                     element)))
                         (funcall fail (format "each element of %s" key)
                                  expected element)))))
                  (:env
                   (unless (or (functionp value)
                               (and (proper-list-p value)
                                    (seq-every-p #'stringp value)))
                     (funcall fail key "a list of NAME=VALUE strings or a \
function" value)))
                  (:host
                   (unless (+agent-shell-arena-git-host-p value)
                     (funcall fail key (+agent-shell-arena-git-host-description)
                              value)))
                  (:host-directory
                   (unless (and (stringp value)
                                (string-match-p "\\`\\(?:~/\\|/\\)[^\n]*\\'"
                                                value))
                     (funcall fail key "an absolute path or one starting \
with ~/" value)))
                  (:delegate
                   (unless (and (proper-list-p value) (cl-evenp (length value)))
                     (funcall fail key "a plist" value))
                   (let ((options (cl-loop for (option) on value by #'cddr
                                           collect option)))
                     (when-let* ((unknown (seq-find
                                           (lambda (option)
                                             (not (memq option
                                                        '(:isolation :host
                                                                     :sandbox))))
                                           options)))
                       (user-error "Arena profile for %s: unknown :delegate \
key %S" toplevel unknown))
                     (when-let* ((twice (seq-find (lambda (option)
                                                    (memq option
                                                          (cdr (memq option
                                                                     options))))
                                                  options)))
                       (user-error "Arena profile for %s: :delegate names \
%s more than once" toplevel twice))
                     (cl-loop
                      for (option setting) on value by #'cddr
                      do (pcase option
                           (:isolation
                            (unless (memq setting '(worktree branch))
                              (funcall fail ":delegate :isolation"
                                       "`worktree' or `branch'" setting)))
                           (:host
                            (unless (eq setting 'local)
                              (funcall fail ":delegate :host" "`local', \
since a delegate otherwise runs on the profile's :host" setting)))
                           (:sandbox
                            (unless (memq setting '(t nil))
                              (funcall fail ":delegate :sandbox" "t or nil"
                                       setting)))))))
                  (_ (user-error "Arena profile for %s: unknown key %S"
                                 toplevel key))))
    (when (and (plist-get profile :host)
               (not (plist-get profile :host-directory)))
      (funcall fail :host-directory "the host's checkout when :host is set"
               nil))
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

(defun +agent-shell-arena-git-provision (manifest name callback &optional
                                                  branch)
  "Add candidate NAME to MANIFEST's arena: a worktree at the base commit.
Link and clone its dependency paths and clone the generated paths the
lead has and the worktree lacks, then call CALLBACK with the candidate's
working directory and nil, or nil and a message.

With BRANCH, the worktree is on that new branch instead of detached, so
a pstack delegate can commit there.

Run from the lead's git directory, git worktree add copies the lead's
sparse-checkout patterns and per-worktree config before it checks out, so
the candidate gets the same sparse checkout.  When MANIFEST has SPARSE
patterns, the worktree gets those instead, as non-cone patterns, which
match the same paths as cone ones."
  (let ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
        (sparse (+agent-shell-arena-git-manifest-sparse manifest))
        (fail (lambda (message) (funcall callback nil message))))
    (condition-case err
        ;; The role is on disk before its worktree, so cleanup finds a
        ;; partly provisioned candidate.
        (let* ((role (+agent-shell-arena-git--add-role manifest name))
               (worktree (+agent-shell-arena-git-role-worktree role)))
          (cl-labels
              ((check-out (git-dir done)
                 (if (not sparse)
                     (funcall done)
                   (+agent-shell-arena-git--git
                    git-dir worktree
                    '("sparse-checkout" "set" "--no-cone" "--stdin") fail
                    (lambda (_)
                      (+agent-shell-arena-git--git
                       git-dir worktree '("reset" "-q" "--hard") fail
                       (lambda (_) (funcall done))))
                    nil
                    (concat (string-join (plist-get sparse :patterns) "\n")
                            "\n")))))
            (+agent-shell-arena-git--git
             (+agent-shell-arena-git-manifest-git-dir manifest) toplevel
             (append '("worktree" "add")
                     (if branch (list "-b" branch) '("--detach"))
                     (when sparse '("--no-checkout"))
                     (list (directory-file-name worktree)
                           (+agent-shell-arena-git-manifest-base manifest)))
             fail
             (lambda (_)
               (condition-case err
                   (let ((git-dir (or (+agent-shell-arena-git--read-in
                                       (expand-file-name ".git" worktree)
                                       worktree
                                       "rev-parse" "--absolute-git-dir")
                                      (error "No git directory for %s"
                                             worktree))))
                     (setf (+agent-shell-arena-git-role-git-dir role) git-dir)
                     (+agent-shell-arena-git-write-manifest manifest)
                     (check-out
                      git-dir
                      (lambda ()
                        (+agent-shell-arena-git--populate
                         manifest role toplevel fail callback))))
                 (error (funcall fail (error-message-string err))))))))
      (error (+agent-shell-arena-git--later fail (error-message-string err))))))

(defun +agent-shell-arena-git--populate (manifest role toplevel fail callback)
  "Link and clone ROLE's dependency paths from TOPLEVEL into its worktree.
Then call CALLBACK as `+agent-shell-arena-git-provision' does, or FAIL."
  (let ((worktree (+agent-shell-arena-git-role-worktree role)))
    (condition-case err
        (progn
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
      (error (funcall fail (error-message-string err))))))

(defun +agent-shell-arena-git--clone (toplevel worktree paths fail done)
  "Clone each of PATHS from TOPLEVEL into WORKTREE, then call DONE.
Symlinks in a clone that point into TOPLEVEL are rewritten to point at
the same path in WORKTREE.  Call FAIL with a message on failure.  A
clone shares blocks where the filesystem can: macOS's cp -c always, and
GNU cp --reflink=auto on Linux, which copies where it cannot."
  (if (null paths)
      (funcall done)
    (let ((source (directory-file-name
                   (expand-file-name (car paths) toplevel)))
          (target (directory-file-name
                   (expand-file-name (car paths) worktree))))
      (make-directory (file-name-directory target) t)
      (+agent-shell-arena-git--run
       (append (list "/bin/cp")
               (if (eq +agent-shell-sandbox-backend 'bwrap)
                   '("--reflink=auto" "-R")
                 '("-c" "-R"))
               (list source target))
       worktree nil fail
       (lambda (_)
         (+agent-shell-arena-git--run
          (list "/usr/bin/find" target "-type" "l" "-print0") worktree nil fail
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
                 (+agent-shell-arena-git--run (list "/bin/cp" "-p" own-index index)
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

(defun +agent-shell-arena-git-ref (manifest name)
  "Return the ref holding role NAME's commit in MANIFEST's repository."
  (+agent-shell-arena-git--ref manifest name))

(defun +agent-shell-arena-git-record (manifest name commit state)
  "Record COMMIT, which the ref of role NAME holds, and STATE in MANIFEST.
Add a role without a worktree when MANIFEST has none called NAME, as for
a result made elsewhere.  Write the manifest and return the role."
  (unless (string-match-p +agent-shell-arena-git--name-regexp name)
    (error "Invalid arena role name: %S" name))
  (let ((role (or (+agent-shell-arena-git-role manifest name)
                  (car (last (setf (+agent-shell-arena-git-manifest-roles
                                    manifest)
                                   (append
                                    (+agent-shell-arena-git-manifest-roles
                                     manifest)
                                    (list (+agent-shell-arena-git--role
                                           :name name)))))))))
    (setf (+agent-shell-arena-git-role-ref role)
          (+agent-shell-arena-git--ref manifest name)
          (+agent-shell-arena-git-role-commit role) commit
          (+agent-shell-arena-git-role-state role) state)
    (+agent-shell-arena-git-write-manifest manifest)
    role))

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

(defvar +agent-shell-arena-git-checkout-limit 1000
  "Most changed paths a judge checkout holds alone.
A result changing more gets every file, since sparse patterns cost time
in proportion to their number times the repository's files.")

(defun +agent-shell-arena-git--exact-pattern (path)
  "Return a non-cone sparse-checkout pattern matching only PATH."
  (concat "/" (replace-regexp-in-string "[][*?\\ ]" "\\\\\\&" path)))

(defun +agent-shell-arena-git-checkout (manifest name callback)
  "Check captured role NAME of MANIFEST out in a worktree of its own.
The worktree is detached at the role's result commit, under the arena's
checkouts/ folder, and recorded in the manifest so cleanup removes it.
It holds only the files the result changed from the base, unless there
are more than `+agent-shell-arena-git-checkout-limit'; the judge reads
the rest at the base.  A full second checkout of a large repository
writes every file again.  Call CALLBACK with its path and nil, or nil
and a message."
  (when-let* ((role (+agent-shell-arena-git--captured manifest name callback)))
    (let* ((checkout (file-name-as-directory
                      (expand-file-name
                       name (expand-file-name
                             "checkouts"
                             (+agent-shell-arena-git-manifest-directory
                              manifest)))))
           (git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
           (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
           (commit (+agent-shell-arena-git-role-commit role))
           (fail (lambda (message) (funcall callback nil message)))
           (done (lambda (_) (funcall callback checkout nil))))
      (make-directory (file-name-directory (directory-file-name checkout)) t)
      ;; Recorded first, so cleanup finds a checkout that failed halfway.
      (setf (+agent-shell-arena-git-role-checkout role) checkout)
      (+agent-shell-arena-git-write-manifest manifest)
      (+agent-shell-arena-git--git
       git-dir toplevel
       (list "diff" "--name-only" "-z" "--no-renames"
             (+agent-shell-arena-git-manifest-base manifest) commit)
       fail
       (lambda (output)
         (let ((paths (split-string output "\0" t)))
           (if (> (length paths) +agent-shell-arena-git-checkout-limit)
               (+agent-shell-arena-git--git
                git-dir toplevel
                (list "worktree" "add" "--detach"
                      (directory-file-name checkout) commit)
                fail done)
             (+agent-shell-arena-git--git
              git-dir toplevel
              (list "worktree" "add" "--detach" "--no-checkout"
                    (directory-file-name checkout) commit)
              fail
              (lambda (_)
                (let ((own (expand-file-name ".git" checkout)))
                  (+agent-shell-arena-git--git
                   own checkout '("sparse-checkout" "set" "--no-cone" "--stdin")
                   fail
                   (lambda (_)
                     (+agent-shell-arena-git--git
                      own checkout '("reset" "-q" "--hard") fail done))
                   nil
                   (mapconcat (lambda (path)
                                (concat (+agent-shell-arena-git--exact-pattern
                                         path)
                                        "\n"))
                              paths ""))))))))))))

;;;; Delegates

(cl-defstruct (+agent-shell-arena-git-user
               (:constructor +agent-shell-arena-git--user)
               (:copier nil))
  "A validated git identity read from the laptop repository's config.
NAME and EMAIL contain no identity delimiters or control characters.
Both author and committer use this identity; host config is no fallback."
  name email)

(defun +agent-shell-arena-git-delegate-options (profile)
  "Resolve delegate execution policy from validated repository PROFILE.
Return (:host HOST :isolation ISOLATION :sandbox SANDBOX).  Absent
:delegate :host inherits PROFILE's registered host pair; only the symbol
`local' overrides it, resolving to nil.  ISOLATION defaults to `worktree'
and SANDBOX to nil.  Signal `user-error' for branch isolation on a host,
before any resource is created.  This is the sole resolver; launch and
recovery read its frozen result."
  (let* ((delegate (plist-get profile :delegate))
         (host (unless (eq (plist-get delegate :host) 'local)
                 (plist-get profile :host)))
         (isolation (or (plist-get delegate :isolation) 'worktree)))
    (when (and host (eq isolation 'branch))
      (user-error "Remote delegates require :isolation worktree; use \
:delegate (:host local)
to use :isolation branch on the laptop."))
    (list :host host :isolation isolation
          :sandbox (and (plist-get delegate :sandbox) t))))

(defun +agent-shell-arena-git-identity-error (name email)
  "Return why NAME and EMAIL are not a usable git identity, or nil."
  (let ((bad (lambda (value)
               (or (not (stringp value))
                   (string-blank-p value)
                   (string-match-p "[<>[:cntrl:]]" value)
                   (not (equal value (string-trim value)))))))
    (cond ((funcall bad name) (format "user.name %S" name))
          ((funcall bad email) (format "user.email %S" email)))))

(defun +agent-shell-arena-git-user-identity (directory callback)
  "Read DIRECTORY's repository-effective git user config on the laptop.
Honor conditional includes; inherited GIT_AUTHOR_* and GIT_COMMITTER_*
variables never stand in for it.  Call CALLBACK, never before this
returns, with a `+agent-shell-arena-git-user' and nil, or nil and an
actionable error.  No other identity substitutes for a missing one."
  (let* ((process-environment
          (seq-remove (lambda (entry)
                        (string-match-p "\\`GIT_\\(AUTHOR\\|COMMITTER\\)_"
                                        entry))
                      process-environment))
         (read (lambda (key done)
                 (+agent-shell-arena-git--run
                  (list "git" "-C" (expand-file-name directory) "config"
                        "--includes" "--get" key)
                  directory nil (lambda (_) (funcall done nil))
                  (lambda (output) (funcall done (string-trim-right
                                                  output "\n")))))))
    (funcall
     read "user.name"
     (lambda (name)
       (funcall
        read "user.email"
        (lambda (email)
          (if-let* ((problem (+agent-shell-arena-git-identity-error
                              name email)))
              (funcall callback nil
                       (format "A delegate commits as your git identity, but \
%s's %s is missing or invalid.  Set user.name and user.email with git config \
there, then request the delegate again."
                               (abbreviate-file-name
                                (directory-file-name directory))
                               problem))
            (funcall callback (+agent-shell-arena-git--user :name name
                                                            :email email)
                     nil))))))))

(defun +agent-shell-arena-git-block-delegate (manifest reason)
  "Record that MANIFEST's sandboxed delegate gets no commit, for REASON.
A commit already made or found unneeded stays recorded as it was."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (finalization (+agent-shell-arena-git-delegate-finalization delegate)))
    (unless (memq (plist-get finalization :state) '(committed empty))
      (setf (+agent-shell-arena-git-delegate-finalization delegate)
            (plist-put (plist-put (copy-sequence finalization) :state 'blocked)
                       :reason reason))
      (+agent-shell-arena-git-write-manifest manifest))
    delegate))

(defun +agent-shell-arena-git-finalize-delegate
    (manifest identity message callback)
  "Make at most one commit of MANIFEST's stopped sandboxed delegate's work.
IDENTITY, a `+agent-shell-arena-git-user', authors and commits it, and
MESSAGE is its exact message.  The caller has established that no writer
runs.  The workspace must still be on the delegate's branch, at the base.
Its files are snapshotted through a private index as a candidate's are,
keeping the base's content for shared and generated paths.  An unchanged
tree records `empty' and makes no commit.

The intent, with fixed dates, is saved before the commit is made, and the
commit before the branch moves to it by compare-and-swap, so a rerun
after a crash reuses or remakes the same commit and never makes a second.
Neither arena authorship, trailers nor a signature is added, and a branch
that moved is never overwritten.  The workspace's index then matches the
commit, its files untouched.  A failure, or a moved branch, records
`blocked' with why and leaves the work where it is.  CALLBACK gets the
delegate record and nil, once, never before this returns."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (base (+agent-shell-arena-git-manifest-base manifest))
         (ref (concat "refs/heads/"
                      (+agent-shell-arena-git-delegate-branch manifest)))
         (role (when (eq 'worktree (+agent-shell-arena-git-delegate-isolation
                                    delegate))
                 (car (+agent-shell-arena-git-manifest-roles manifest))))
         (git-dir (if role (+agent-shell-arena-git-role-git-dir role)
                    (+agent-shell-arena-git-manifest-git-dir manifest)))
         (work-tree (if role (+agent-shell-arena-git-role-worktree role)
                      (+agent-shell-arena-git-manifest-toplevel manifest)))
         (read (lambda (&rest args)
                 (apply #'+agent-shell-arena-git--read-in git-dir work-tree
                        args)))
         (finished nil)
         (finish (lambda ()
                   (unless finished
                     (setq finished t)
                     (+agent-shell-arena-git--later callback delegate nil))))
         (block (lambda (reason)
                  (+agent-shell-arena-git-block-delegate manifest reason)
                  (funcall finish)))
         (save (lambda (finalization)
                 (setf (+agent-shell-arena-git-delegate-finalization delegate)
                       finalization)
                 (+agent-shell-arena-git-write-manifest manifest)
                 finalization))
         (git (lambda (args done &optional env input)
                (+agent-shell-arena-git--git
                 git-dir work-tree args
                 (lambda (message)
                   (funcall block (format "Emacs made no commit: %s" message)))
                 done env input))))
    (cl-labels
        ((reconcile (intent)
           (funcall git '("reset" "-q")
                    (lambda (_)
                      (funcall save (plist-put (copy-sequence intent)
                                               :state 'committed))
                      (funcall finish))))
         (publish (intent oid)
           (let ((intent (plist-put (copy-sequence intent) :oid oid)))
             (funcall save intent)
             (funcall git (list "update-ref" "-m" "pstack delegate commit"
                                ref oid base)
                      (lambda (_) (reconcile intent)))))
         (make (intent)
           (let ((name (plist-get intent :name))
                 (email (plist-get intent :email)))
             (funcall
              git (list "-c" "commit.gpgSign=false"
                        "-c" "i18n.commitEncoding=UTF-8"
                        "commit-tree" "--no-gpg-sign"
                        "-p" (plist-get intent :parent)
                        "-F" "-" (plist-get intent :tree))
              (lambda (output) (publish intent (string-trim output)))
              (list (concat "GIT_AUTHOR_NAME=" name)
                    (concat "GIT_AUTHOR_EMAIL=" email)
                    (concat "GIT_AUTHOR_DATE=" (plist-get intent :author-date))
                    (concat "GIT_COMMITTER_NAME=" name)
                    (concat "GIT_COMMITTER_EMAIL=" email)
                    (concat "GIT_COMMITTER_DATE="
                            (plist-get intent :committer-date)))
              (plist-get intent :message))))
         (recover (intent)
           (let ((tip (funcall read "rev-parse" "--verify" "-q" ref))
                 (oid (plist-get intent :oid)))
             (cond
              ((and oid (equal tip oid)) (reconcile intent))
              ((not (equal tip base))
               (funcall block (format "%s moved to %s while Emacs committed \
the delegate's work, so it made no commit" ref (or tip "nothing"))))
              ((and oid (funcall read "cat-file" "-t" oid)) (publish intent oid))
              (t (make intent)))))
         (snapshot ()
           (let ((own-index (expand-file-name "index" git-dir)))
             (+agent-shell-arena-git--snapshot
              git-dir work-tree
              (lambda (index fail done)
                (let ((read-tree
                       (lambda (_)
                         (+agent-shell-arena-git--git
                          git-dir work-tree (list "read-tree" "--reset" base)
                          fail (lambda (_) (funcall done))
                          (list (concat "GIT_INDEX_FILE=" index))))))
                  (if (file-exists-p own-index)
                      (+agent-shell-arena-git--run
                       (list "/bin/cp" "-p" own-index index) work-tree nil fail
                       read-tree)
                    (funcall read-tree nil))))
              (cons +agent-shell-arena-git--agent-shell-exclude
                    (mapcar (lambda (path) (concat ":(exclude,literal)" path))
                            (and role (+agent-shell-arena-git-role-shared role))))
              (cons base (and role (+agent-shell-arena-git-role-generated role)))
              (lambda (message)
                (funcall block (format "Emacs could not snapshot the \
delegate's work: %s" message)))
              (lambda (tree)
                (if (equal tree (funcall read "rev-parse" (concat base "^{tree}")))
                    (progn (funcall save (list :state 'empty))
                           (funcall finish))
                  (let ((date (format "%d +0000" (floor (float-time)))))
                    (make (funcall save
                                   (list :state 'prepared :parent base :tree tree
                                         :message message
                                         :name (+agent-shell-arena-git-user-name
                                                identity)
                                         :email (+agent-shell-arena-git-user-email
                                                 identity)
                                         :author-date date
                                         :committer-date date))))))))))
      (condition-case err
          (let ((finalization (+agent-shell-arena-git-delegate-finalization
                               delegate)))
            (pcase (plist-get finalization :state)
              ((or 'committed 'empty) (funcall finish))
              ('prepared (recover finalization))
              (_
               (let ((head (funcall read "symbolic-ref" "-q" "HEAD"))
                     (tip (funcall read "rev-parse" "--verify" "-q" ref)))
                 (cond
                  ((not (equal head ref))
                   (funcall block (format "The workspace is on %s, not %s, so \
Emacs made no commit" (or head "a detached HEAD") ref)))
                  ((not (equal tip base))
                   (funcall block (format "%s moved from its base %s to %s, \
so Emacs made no commit" ref base (or tip "nothing"))))
                  ((null (and identity
                              (not (+agent-shell-arena-git-identity-error
                                    (+agent-shell-arena-git-user-name identity)
                                    (+agent-shell-arena-git-user-email
                                     identity)))))
                   (funcall block "The delegate has no valid git identity to \
commit as"))
                  (t (snapshot)))))))
        (error (funcall block (format "Emacs made no commit: %s"
                                      (error-message-string err))))))))

(defun +agent-shell-arena-git-delegate-branch (manifest)
  "Return the branch MANIFEST's pstack delegate commits on.
It is named for the panel, whose id is the arena id without its suffix."
  (concat "pstack/delegate/"
          (replace-regexp-in-string
           "-[[:xdigit:]]\\{8\\}\\'" ""
           (+agent-shell-arena-git-manifest-id manifest))))

(defun +agent-shell-arena-git-describe-paths (paths)
  "Return PATHS as a short series, naming at most ten."
  (concat (string-join (seq-take paths 10) ", ")
          (when (nthcdr 10 paths)
            (format " and %d more" (- (length paths) 10)))))

(defun +agent-shell-arena-git-origin-name (origin)
  "Return a name for ORIGIN, a delegate's (branch . REF) or (detached . OID)."
  (pcase origin
    (`(branch . ,ref) (string-remove-prefix "refs/heads/" ref))
    (`(detached . ,oid) (format "commit %s" oid))))

(defun +agent-shell-arena-git-changes (git-dir work-tree excludes callback)
  "Call CALLBACK with the paths git status reports in WORK-TREE.
Leave out agent-shell transcripts and EXCLUDES, paths relative to the top
level.  CALLBACK gets the paths, nil when nothing changed, and nil; or
nil and a message."
  (+agent-shell-arena-git--git
   git-dir work-tree
   (append (list "status" "--porcelain=v1" "-z" "--untracked-files=normal"
                 "--" ":/" +agent-shell-arena-git--agent-shell-exclude)
           (mapcar (lambda (path) (concat ":(exclude,literal)" path))
                   excludes))
   (lambda (message) (funcall callback nil message))
   (lambda (output)
     (let ((entries (split-string output "\0" t)) paths)
       (while entries
         (let ((entry (pop entries)))
           (push (substring entry 3) paths)
           ;; A rename or copy names its source in the next entry.
           (when (memq (aref entry 0) '(?R ?C)) (pop entries))))
       (funcall callback (nreverse paths) nil)))))

(defun +agent-shell-arena-git-take (manifest callback)
  "Put the lead's checkout on MANIFEST's new delegate branch at the base.
Call CALLBACK with the branch and nil, or nil and a message."
  (let ((branch (+agent-shell-arena-git-delegate-branch manifest)))
    (+agent-shell-arena-git--git
     (+agent-shell-arena-git-manifest-git-dir manifest)
     (+agent-shell-arena-git-manifest-toplevel manifest)
     (list "switch" "-q" "-c" branch
           (+agent-shell-arena-git-manifest-base manifest))
     (lambda (message) (funcall callback nil message))
     (lambda (_) (funcall callback branch nil)))))

(defun +agent-shell-arena-git-restore (manifest callback)
  "Return the lead's checkout from MANIFEST's delegate branch to its origin.
Switch only when the checkout is still on the branch with nothing
changed, and record the outcome, or why not, in the delegate record.  A
record already `restored' or `unneeded' stays as it is.  Call CALLBACK
with the record and nil, or nil and a message."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
         (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (branch (+agent-shell-arena-git-delegate-branch manifest))
         (origin (+agent-shell-arena-git-delegate-origin delegate))
         (name (+agent-shell-arena-git-origin-name origin))
         (read (lambda (&rest args)
                 (apply #'+agent-shell-arena-git--read-in git-dir toplevel
                        args)))
         (fail (lambda (message) (funcall callback nil message)))
         (end (lambda (restore note)
                (condition-case err
                    (progn
                      (setf (+agent-shell-arena-git-delegate-restore delegate)
                            restore
                            (+agent-shell-arena-git-delegate-note delegate)
                            note)
                      (+agent-shell-arena-git-write-manifest manifest)
                      (+agent-shell-arena-git--later callback delegate nil))
                  (error (+agent-shell-arena-git--later
                          fail (error-message-string err)))))))
    (condition-case err
        (let ((head (funcall read "symbolic-ref" "-q" "HEAD"))
              (oid (funcall read "rev-parse" "-q" "--verify" "HEAD")))
          (cond
           ((or (memq (+agent-shell-arena-git-delegate-restore delegate)
                      '(restored unneeded))
                (not (eq (+agent-shell-arena-git-delegate-isolation delegate)
                         'branch)))
            (+agent-shell-arena-git--later callback delegate nil))
           ((pcase origin
              (`(branch . ,ref) (equal head ref))
              (`(detached . ,commit) (and (null head) (equal oid commit))))
            (funcall end 'unneeded (format "The checkout was already back on \
%s." name)))
           ((not (equal head (concat "refs/heads/" branch)))
            (funcall end 'changed
                     (format "The checkout is on %s, not %s, so Emacs left it."
                             (if head (string-remove-prefix "refs/heads/" head)
                               (format "commit %s" oid))
                             branch)))
           (t
            (+agent-shell-arena-git-changes
             git-dir toplevel nil
             (lambda (paths error)
               (cond
                (error (funcall fail error))
                (paths
                 (funcall end 'dirty
                          (format "The checkout has uncommitted changes: %s."
                                  (+agent-shell-arena-git-describe-paths
                                   paths))))
                ((not (funcall read "rev-parse" "-q" "--verify"
                               (concat (cdr origin) "^{commit}")))
                 (funcall end 'missing (format "Emacs left the checkout on \
%s, since %s no longer exists." branch name)))
                (t
                 (+agent-shell-arena-git--git
                  git-dir toplevel
                  (pcase origin
                    (`(branch . ,_) (list "switch" "-q" name))
                    (`(detached . ,commit)
                     (list "switch" "-q" "--detach" commit)))
                  fail
                  (lambda (_)
                    (funcall end 'restored
                             (format "Switched the checkout back to %s."
                                     name)))))))))))
      (error (+agent-shell-arena-git--later fail (error-message-string err))))))

(defun +agent-shell-arena-git-drop-branch (manifest callback)
  "Delete MANIFEST's delegate branch when it has no commit of its own.
Only a branch that still points exactly at the base, and that no
worktree has checked out, goes.  Call CALLBACK with the branch when it
went, or nil when it stays, and nil; or nil and a message."
  (let* ((git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
         (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (base (+agent-shell-arena-git-manifest-base manifest))
         (branch (+agent-shell-arena-git-delegate-branch manifest))
         (ref (concat "refs/heads/" branch))
         (fail (lambda (message) (funcall callback nil message))))
    (+agent-shell-arena-git--git
     git-dir toplevel '("worktree" "list" "--porcelain" "-z") fail
     (lambda (output)
       (if (or (member (concat "branch " ref) (split-string output "\0" t))
               (not (equal base (+agent-shell-arena-git--read-in
                                 git-dir toplevel
                                 "rev-parse" "-q" "--verify" ref))))
           (funcall callback nil nil)
         ;; The old value makes git refuse a branch that moved meanwhile.
         (+agent-shell-arena-git--git
          git-dir toplevel (list "update-ref" "-d" ref base) fail
          (lambda (_) (funcall callback branch nil))))))))

(defun +agent-shell-arena-git-delegate-log (manifest tip callback)
  "Call CALLBACK with the commits from MANIFEST's base to TIP, and nil.
They are (OID . SUBJECT), oldest first; or CALLBACK gets nil and a
message.  Local collection and a remote delegate's landing both list
a delegate's branch this way."
  (+agent-shell-arena-git--git
   (+agent-shell-arena-git-manifest-git-dir manifest)
   (+agent-shell-arena-git-manifest-toplevel manifest)
   (list "log" "--reverse" "--format=%H %s"
         (format "%s..%s" (+agent-shell-arena-git-manifest-base manifest) tip))
   (lambda (message) (funcall callback nil message))
   (lambda (output)
     (funcall callback
              (mapcar (lambda (line)
                        (let ((space (string-search " " line)))
                          (cons (substring line 0 space)
                                (substring line (1+ space)))))
                      (split-string output "\n" t))
              nil))))

(defun +agent-shell-arena-git-finish-delegate (manifest hold callback)
  "Record what MANIFEST's stopped delegate left in its delegate record.
List its commits since the base.  In a worktree, note uncommitted
changes; in branch isolation, return the lead's checkout with
`+agent-shell-arena-git-restore' unless HOLD, a note saying why it must
stay.  Once the checkout is back, forget the record on disk, as
`+agent-shell-arena-git-clean' would.  Call CALLBACK with the record and
nil, or nil and a message."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (fail (lambda (message) (funcall callback nil message)))
         (log
          (lambda (&rest _)
            (+agent-shell-arena-git-delegate-log
             manifest
             (concat "refs/heads/"
                     (+agent-shell-arena-git-delegate-branch manifest))
             (lambda (commits error)
               (if error (funcall fail error)
               (setf (+agent-shell-arena-git-delegate-commits delegate)
                     commits)
               (if (memq (+agent-shell-arena-git-delegate-restore delegate)
                         '(restored unneeded))
                   (+agent-shell-arena-git--remove
                    manifest (lambda (_ error)
                               (funcall callback (unless error delegate)
                                        error)))
                 (+agent-shell-arena-git-write-manifest manifest)
                 (funcall callback delegate nil))))))))
    (cond
     ((eq (+agent-shell-arena-git-delegate-isolation delegate) 'worktree)
      (let ((role (car (+agent-shell-arena-git-manifest-roles manifest))))
        (+agent-shell-arena-git-changes
         (+agent-shell-arena-git-role-git-dir role)
         (+agent-shell-arena-git-role-worktree role)
         (append (+agent-shell-arena-git-role-shared role)
                 (+agent-shell-arena-git-role-generated role))
         (lambda (paths error)
           (if error (funcall fail error)
             (setf (+agent-shell-arena-git-delegate-dirty delegate) paths
                   (+agent-shell-arena-git-delegate-note delegate)
                   (when paths
                     (format "The worktree has uncommitted changes: %s."
                             (+agent-shell-arena-git-describe-paths paths))))
             (funcall log))))))
     (hold
      (setf (+agent-shell-arena-git-delegate-note delegate) hold)
      (+agent-shell-arena-git--later log))
     (t
      (+agent-shell-arena-git-restore
       manifest (lambda (_ error) (if error (funcall fail error)
                                    (funcall log))))))))

;;;; Cleanup

(defun +agent-shell-arena-git-clean (manifest callback)
  "Remove MANIFEST's worktrees, checkouts, private clones and result refs.
A worktree the repository does not list, or a ref that no longer points
at the recorded commit, stays in place.  A step that fails is recorded
and the rest still run.  When no worktree stays, delete the arena
directory too.  Call CALLBACK with a list of messages naming what
stayed, and nil; or, when a step failed, nil and a message naming each
failure and then what stayed.

A pstack delegate's branch always stays.  A delegate that worked in the
lead's own checkout first returns it with `+agent-shell-arena-git-restore';
when that cannot, nothing is removed and the message says why.

Trees go as `+agent-shell-sandbox-delete' deletes them, so read-only
folders a candidate's tools made do not stop the cleanup."
  (let ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
        (returned-p (lambda (delegate)
                      (memq (+agent-shell-arena-git-delegate-restore delegate)
                            '(restored unneeded)))))
    (if (or (null delegate)
            (eq (+agent-shell-arena-git-delegate-isolation delegate) 'worktree)
            (funcall returned-p delegate))
        (+agent-shell-arena-git--remove manifest callback)
      (+agent-shell-arena-git-restore
       manifest
       (lambda (delegate error)
         (cond
          (error (funcall callback nil error))
          ((funcall returned-p delegate)
           (+agent-shell-arena-git--remove manifest callback))
          (t (funcall callback nil
                      (format "%s stays on %s, so Emacs kept its record: %s"
                              (directory-file-name
                               (+agent-shell-arena-git-manifest-toplevel
                                manifest))
                              (+agent-shell-arena-git-delegate-branch manifest)
                              (+agent-shell-arena-git-delegate-note
                               delegate))))))))))

(defun +agent-shell-arena-git--remove (manifest callback)
  "Remove what MANIFEST records, as `+agent-shell-arena-git-clean' says."
  (let* ((git-dir (+agent-shell-arena-git-manifest-git-dir manifest))
         (toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (left nil)
         (failures nil)
         (worktree-left nil))
    (+agent-shell-arena-git--git
     git-dir toplevel '("worktree" "list" "--porcelain" "-z")
     (lambda (message) (funcall callback nil message))
     (lambda (output)
       (let ((registered
              (delq nil (mapcar (lambda (field)
                                  (when (string-prefix-p "worktree " field)
                                    (file-truename
                                     (file-name-as-directory
                                      (substring field (length "worktree "))))))
                                (split-string output "\0" t)))))
         (cl-labels
             ((record (message) (push message failures))
              (finish ()
                (let ((left (nreverse left)))
                  (if failures
                      (funcall callback nil
                               (string-join (append (nreverse failures) left)
                                            "; "))
                    (funcall callback left nil))))
              (next (roles)
                (cond
                 (roles
                  (clean-role (car roles) (lambda () (next (cdr roles)))))
                 (worktree-left (finish))
                 (t (+agent-shell-sandbox-delete
                     (list (directory-file-name
                            (+agent-shell-arena-git-manifest-directory
                             manifest)))
                     (lambda (error)
                       (when error (record error))
                       (finish))))))
              ;; A role a remote arena fetched has a ref but no worktree.
              (clean-role (role done)
                (remove-trees
                 (append
                  (when-let* ((worktree (+agent-shell-arena-git-role-worktree
                                         role)))
                    (list (cons worktree
                                (+agent-shell-arena-git-role-private role))))
                  (when-let* ((checkout
                               (+agent-shell-arena-git-role-checkout role)))
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
                       (lambda () (remove-worktree tree next))))
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
                  (+agent-shell-sandbox-delete
                   (mapcar (lambda (path)
                             (directory-file-name
                              (expand-file-name path worktree)))
                           paths)
                   (lambda (error)
                     (when error (record error))
                     (funcall done)))))
              ;; A removal git gives up on still drops the registration,
              ;; so what it left is deleted like any other tree.
              (remove-worktree (tree done)
                (+agent-shell-arena-git--git
                 git-dir toplevel
                 (list "worktree" "remove" "--force"
                       (directory-file-name tree))
                 (lambda (_)
                   (+agent-shell-sandbox-delete
                    (list (directory-file-name tree))
                    (lambda (error)
                      (when error
                        (setq worktree-left t)
                        (record error))
                      (funcall done))))
                 (lambda (_) (funcall done))))
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
                     git-dir toplevel (list "update-ref" "-d" ref commit)
                     (lambda (message) (record message) (funcall done))
                     (lambda (_) (funcall done))))
                   (t
                    (push (format "%s points at %s, not the recorded %s; left \
in place" ref current (or commit "nothing"))
                          left)
                    (funcall done))))))
           (next (+agent-shell-arena-git-manifest-roles manifest))))))))

(provide '+agent-shell-arena-git)
;;; +agent-shell-arena-git.el ends here
