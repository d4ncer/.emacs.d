;;; +agent-shell-sandbox.el --- Confine agent shells with sandbox-exec -*- lexical-binding: t; -*-

;;; Commentary:
;; A `+agent-shell-sandbox' describes where a confined agent may write: its
;; root, a short private temp folder and a private agent config folder.  It
;; also lists folders the agent may not read, to which every sandbox adds
;; `+agent-shell-sandbox-secret-reads' and every other sandbox's private
;; folders.  `+agent-shell-sandbox-profile' renders it as a macOS
;; sandbox-exec profile, `+agent-shell-sandbox-readable-p' applies the same
;; read rules in Emacs, and
;; `+agent-shell-sandbox-client-maker' wraps an agent config's client maker
;; so the adapter starts under that profile with the agent's private
;; environment from `+agent-shell-sandbox-agents' and the defaults from
;; `+agent-shell-sandbox-environment'.  The kernel logs each refusal with
;; the sandbox's tag, and a sandbox in trace mode reports what its profile
;; would refuse instead of refusing it.
;;
;; Stopping takes three steps, in order: `+agent-shell-sandbox-kill-tree'
;; while the adapter lives, `+agent-shell-sandbox-sweep' once it is gone,
;; then `+agent-shell-sandbox-finish'.  `+agent-shell-drive-stop' runs them
;; around killing the shell.  `+agent-shell-sandbox-discard' then deletes
;; the private folders, whatever modes the agent's tools left in them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar server-socket-dir)

(defgroup +agent-shell-sandbox nil
  "Confine agent shells with sandbox-exec."
  :group 'agent-shell)

(defcustom +agent-shell-sandbox-agents
  '((claude-code :mode "bypassPermissions"
                 :prepare +agent-shell-sandbox-prepare-claude)
    (codex :mode "agent-full-access"
           :prepare +agent-shell-sandbox-prepare-codex
           :finish +agent-shell-sandbox-finish-codex))
  "How each agent runs in a sandbox, by agent config identifier.
:mode is the full-access session mode.  :prepare is called with the
sandbox, may record credentials in it, and returns environment entries
that take precedence over the agent's own.  :finish, when present, is
called with the sandbox once its processes are gone."
  :type '(alist :key-type symbol :value-type plist))

(defvar +agent-shell-sandbox--security "/usr/bin/security"
  "macOS's Keychain tool, by absolute path so no other one on PATH runs.")

(defcustom +agent-shell-sandbox-claude-token-service
  "pstack-arena-claude-token"
  "Keychain service holding a `claude setup-token' token for candidates."
  :type 'string)

(defcustom +agent-shell-sandbox-codex-home "~/.codex"
  "Codex home whose login and config sandboxed Codex agents copy."
  :type 'directory)

(defcustom +agent-shell-sandbox-secret-reads
  '("~/.ssh" "~/.gnupg" "~/.aws" "~/.config/gcloud" "~/.azure" "~/.kube"
    "~/.docker" "~/.netrc" "~/.authinfo" "~/.authinfo.gpg" "~/.config/gh"
    "~/.config/op" "~/.op" "~/Library/Keychains" +private-config-directory)
  "Credential locations no sandboxed agent may read.
Each element is a file or folder, or a variable whose value, when bound
and non-nil, is one.  The defaults hold SSH and GnuPG keys, the AWS,
Google Cloud, Azure and Kubernetes logins, Docker registry logins, netrc
and authinfo passwords, the GitHub and 1Password CLI logins, the macOS
Keychain files, and the private Emacs config.  A path that does not exist
is harmless.  A read allowance, such as a repository profile's :read,
can still open a path inside one."
  :type '(repeat (choice file variable)))

(defconst +agent-shell-sandbox--sweeps 10
  "Most sweeps a stop makes before giving up on survivors.")

(cl-defstruct (+agent-shell-sandbox
               (:constructor +agent-shell-sandbox--make)
               (:copier nil))
  "Where a confined agent may write and what it may not read.
Paths are true names without a trailing slash."
  (root nil :read-only t :documentation "Writable working tree.")
  (tmp nil :read-only t :documentation "Private temp folder, /tmp/pa-HEX.")
  (config nil :read-only t :documentation "Private agent config folder.")
  (deny-reads nil :read-only t :documentation "Folders the agent cannot read.")
  (agent nil :documentation "Agent identifier, set when prepared.")
  (credentials nil :documentation "Plist the agent's :finish reads.")
  (allow-reads nil :read-only t
               :documentation "Folders or patterns inside DENY-READS the \
agent may read.")
  (allow-metadata nil :read-only t
                  :documentation "Paths whose metadata alone it may read.")
  (allow-writes nil :read-only t
                :documentation "Folders or patterns beyond its own it may \
write.")
  (env nil :read-only t
       :documentation "Environment entries ahead of the defaults.")
  (allow-connects nil :read-only t
                  :documentation "Unix sockets it may connect to.")
  (tag nil :read-only t
       :documentation "Message the kernel's log of each refusal carries.")
  (trace nil :read-only t
         :documentation "Whether the profile reports instead of refusing."))

(defun +agent-shell-sandbox--path (path)
  "Return PATH's true name without a trailing slash."
  (directory-file-name (file-truename (expand-file-name path))))

(defvar +agent-shell-sandbox--config-parent nil
  "True name of the folder every sandbox's config folder is made in.
Fixed on first use, so one pattern covers every sandbox's.")

(defun +agent-shell-sandbox--config-parent ()
  "Return the folder sandbox config folders are made in."
  (or +agent-shell-sandbox--config-parent
      (setq +agent-shell-sandbox--config-parent
            (+agent-shell-sandbox--path temporary-file-directory))))

(defun +agent-shell-sandbox--private-patterns ()
  "Return patterns matching every sandbox's private temp and config folders."
  ;; sandbox-exec compiles an interval such as [0-9a-f]{8} but never
  ;; matches it, so the class is repeated.
  (list (cons 'regexp
              (concat "^" (+agent-shell-sandbox-regexp-quote
                           (+agent-shell-sandbox--path "/tmp"))
                      "/pa-" (apply #'concat (make-list 8 "[0-9a-f]"))
                      "(/|$)"))
        (cons 'regexp
              (concat "^" (+agent-shell-sandbox-regexp-quote
                           (concat (+agent-shell-sandbox--config-parent)
                                   "/pa-config-"))))))

(defun +agent-shell-sandbox--secrets ()
  "Return the paths in `+agent-shell-sandbox-secret-reads'."
  (delq nil (mapcar (lambda (item)
                      (if (symbolp item)
                          (and (boundp item) (symbol-value item))
                        item))
                    +agent-shell-sandbox-secret-reads)))

(defun +agent-shell-sandbox--make-tmp ()
  "Create and return a fresh /tmp/pa-HEX folder.
Claude Code needs it under about 30 bytes to hold AF_UNIX socket paths."
  (catch 'made
    (dotimes (_ 10)
      (let ((path (format "/tmp/pa-%08x" (random #x100000000))))
        (condition-case nil
            (progn (make-directory path) (set-file-modes path #o700)
                   (throw 'made path))
          (file-already-exists nil))))
    (error "Could not create a private temp folder in /tmp")))

(defun +agent-shell-sandbox--resolve (sandbox items check kind)
  "Return ITEMS with each function called with SANDBOX, flattened.
A function returns one value or a list.  CHECK returns nil for a valid
value and otherwise what it must be; signal `user-error' for an invalid
value, naming KIND."
  (mapcan (lambda (item)
            (let ((values (if (functionp item) (funcall item sandbox) item)))
              (mapcar (lambda (value)
                        (when-let* ((expected (funcall check value)))
                          (user-error "Sandbox %s must be %s, not %S"
                                      kind expected value))
                        value)
                      (if (or (atom values)
                              (+agent-shell-sandbox-pattern-p values))
                          (and values (list values))
                        values))))
          items))

(defun +agent-shell-sandbox-pattern-p (value)
  "Return whether VALUE is a (regexp . RE) pattern allowance."
  (eq (car-safe value) 'regexp))

(defun +agent-shell-sandbox-regexp-quote (string)
  "Return a sandbox profile regex matching STRING literally."
  (replace-regexp-in-string "[][.^$*+?(){}|\\]" "\\\\\\&" string))

(defun +agent-shell-sandbox--pattern-prefix (re)
  "Return the literal text every match of RE starts with, or nil.
RE is a profile regex.  nil when it is not anchored with ^, or holds an
alternative outside parentheses, which could match anything."
  (when (string-prefix-p "^" re)
    (let ((i 1) (end (length re)) (depth 0) (prefix "") (literal t) (ok t))
      (while (and ok (< i end))
        (let ((char (aref re i)))
          (cond
           ((eq char ?\\)
            (let ((next (and (< (1+ i) end) (aref re (1+ i)))))
              (if (and literal next
                       (not (string-match-p "[[:alnum:]]" (string next))))
                  (setq prefix (concat prefix (string next)))
                (setq literal nil)))
            (cl-incf i 2))
           ((eq char ?\[)
            (setq literal nil
                  i (+agent-shell-sandbox--bracket-end re (1+ i))))
           ((memq char '(?* ?+ ?? ?{))
            (when (and literal (> (length prefix) 0))
              (setq prefix (substring prefix 0 -1)))
            (setq literal nil)
            (cl-incf i))
           ((eq char ?|)
            (when (<= depth 0) (setq ok nil))
            (cl-incf i))
           ((memq char '(?\( ?\) ?. ?^ ?$))
            (setq literal nil
                  depth (+ depth (pcase char (?\( 1) (?\) -1) (_ 0))))
            (cl-incf i))
           (t (when literal (setq prefix (concat prefix (string char))))
              (cl-incf i)))))
      (and ok prefix))))

(defun +agent-shell-sandbox--bracket-end (re start)
  "Return the index after the bracket expression of RE opening before START."
  (let ((i start) (end (length re)))
    (when (and (< i end) (eq (aref re i) ?^)) (cl-incf i))
    (when (and (< i end) (eq (aref re i) ?\])) (cl-incf i))
    (catch 'done
      (while (< i end)
        (let ((char (aref re i)))
          (cond ((eq char ?\]) (throw 'done (1+ i)))
                ((and (eq char ?\[) (< (1+ i) end)
                      (memq (aref re (1+ i)) '(?: ?. ?=)))
                 (setq i (let ((close (string-search
                                       (string (aref re (1+ i)) ?\])
                                       re (+ i 2))))
                           (if close (+ close 2) end))))
                (t (cl-incf i)))))
      end)))

(defconst +agent-shell-sandbox--pattern-rule
  "a (regexp . RE) pattern whose RE starts with ^ and the true name of a \
folder below the root, as in ^/private/tmp/diff\\."
  "What a pattern allowance must be.")

(defun +agent-shell-sandbox--pattern-error (re)
  "Return nil when RE is a safe profile regex, otherwise why it is not.
A safe one starts with ^, a literal true name of a folder below the root
and no alternative outside parentheses, so it cannot match / or the
whole filesystem, and sandbox-exec compiles it."
  (let ((prefix (and (stringp re) (+agent-shell-sandbox--pattern-prefix re))))
    (cond
     ((not (stringp re)) +agent-shell-sandbox--pattern-rule)
     ((string-search "\"" re)
      "a (regexp . RE) pattern whose RE holds no double quote")
     ((not (and prefix (string-match-p "\\`/[^/]+/" prefix)))
      +agent-shell-sandbox--pattern-rule)
     ((let ((folder (file-name-directory prefix)))
        (unless (equal (file-name-as-directory (file-truename folder)) folder)
          (format "a pattern of true names, here %s rather than %s"
                  (file-name-as-directory (file-truename folder)) folder))))
     ((file-executable-p "/usr/bin/sandbox-exec")
      (with-temp-buffer
        (let ((default-directory "/"))
          (unless (eql 0 (call-process
                          "/usr/bin/sandbox-exec" nil t nil "-p"
                          (format "(version 1)\n(allow default)\n\
(allow file-write* (regex #\"%s\"))\n" re)
                          "/usr/bin/true"))
            (format "a regex sandbox-exec compiles (%s)"
                    (string-trim (buffer-string))))))))))

(defun +agent-shell-sandbox-socket-error (value)
  "Return nil when VALUE is a socket allowance, else what it must be.
A socket allowance is an absolute path outside the Emacs server's socket
folder, compared by true name, so no agent can drive this Emacs."
  (require 'server)
  (let ((server (and server-socket-dir
                     (file-name-as-directory
                      (+agent-shell-sandbox--path server-socket-dir)))))
    (cond ((not (and (stringp value) (file-name-absolute-p value)))
           "an absolute socket path")
          ((and server
                (string-prefix-p server (file-name-as-directory
                                         (+agent-shell-sandbox--path value))))
           (format "a socket outside the Emacs server's folder %s" server)))))

(defun +agent-shell-sandbox-allowance-error (value)
  "Return nil when VALUE is a read or write allowance, else what it must be.
An allowance is an absolute path or a (regexp . RE) pattern; see
`+agent-shell-sandbox-create'."
  (cond ((+agent-shell-sandbox-pattern-p value)
         (+agent-shell-sandbox--pattern-error (cdr value)))
        ((not (and (stringp value) (file-name-absolute-p value)))
         "an absolute path or a (regexp . RE) pattern")))

(cl-defun +agent-shell-sandbox-create (root &key deny-reads allow-reads
                                             allow-metadata allow-writes
                                             allow-connects env trace)
  "Return a sandbox writing to ROOT, creating its private folders.
DENY-READS lists folders the agent may not read, to which the sandbox
adds `+agent-shell-sandbox-secret-reads'.  It cannot read any sandbox's
private temp or config folder but its own.  ALLOW-READS lists folders
inside these it may read anyway.  A read is checked against the path a
symlink resolves to, so a link into a denied folder needs an allowance.
ALLOW-METADATA lists paths, each exactly, whose metadata the agent may
read even inside DENY-READS, such as a folder git must stat.
ALLOW-WRITES lists more folders the agent may write, and ENV NAME=VALUE
entries that win over `+agent-shell-sandbox-environment's defaults.

An element of ALLOW-READS or ALLOW-WRITES may instead be (regexp . RE),
allowing every path RE matches, for tools that make randomly named
files in a shared folder.  RE is a sandbox-exec regex matched against
true names, such as ^/private/tmp/diff\\.; it must start with ^ and a
literal folder below the root.  `+agent-shell-sandbox-regexp-quote'
quotes a path for one.

ALLOW-CONNECTS lists unix socket paths the agent may connect to, such as
a build tool's tunnel to its remote cache.  Every other unix socket is
refused but mDNSResponder's.  None may be the Emacs server's socket
folder, `server-socket-dir', or under it.

Each element of ALLOW-READS, ALLOW-WRITES, ALLOW-CONNECTS and ENV may
instead be a function called with the new sandbox, whose root and private
folders exist, returning one path, pattern or entry or a list of them.

With TRACE non-nil, the profile reports what it would refuse rather than
refusing it; see `+agent-shell-sandbox-profile'.  Either way the kernel
logs each report or refusal with the sandbox's tag as its message."
  (unless (file-directory-p root)
    (user-error "Sandbox root %s is not a directory" root))
  (let* ((tmp (+agent-shell-sandbox--path (+agent-shell-sandbox--make-tmp)))
         (tag (format "pa-sandbox-%08x" (random #x100000000)))
         (config (+agent-shell-sandbox--path
                  (make-temp-file
                   (expand-file-name "pa-config-"
                                     (+agent-shell-sandbox--config-parent))
                   t)))
         (make (lambda (&rest resolved)
                 (apply #'+agent-shell-sandbox--make
                        :root (+agent-shell-sandbox--path root)
                        :tmp tmp :config config
                        :deny-reads (mapcar #'+agent-shell-sandbox--path
                                            (append
                                             deny-reads
                                             (+agent-shell-sandbox--secrets)))
                        :allow-metadata (mapcar #'+agent-shell-sandbox--path
                                                allow-metadata)
                        :tag tag :trace (and trace t) resolved)))
         (provisional (funcall make))
         (allowances
          (lambda (items kind)
            (mapcar (lambda (value)
                      (if (+agent-shell-sandbox-pattern-p value) value
                        (+agent-shell-sandbox--path value)))
                    (+agent-shell-sandbox--resolve
                     provisional items #'+agent-shell-sandbox-allowance-error
                     kind)))))
    (condition-case err
        (funcall make
                 :allow-reads (funcall allowances allow-reads "read allowance")
                 :allow-writes (funcall allowances allow-writes
                                        "write allowance")
                 :allow-connects (mapcar #'+agent-shell-sandbox--path
                                         (+agent-shell-sandbox--resolve
                                          provisional allow-connects
                                          #'+agent-shell-sandbox-socket-error
                                          "socket allowance"))
                 :env (+agent-shell-sandbox--resolve
                       provisional env
                       (lambda (entry)
                         (unless (and (stringp entry)
                                      (string-match-p
                                       "\\`[A-Za-z_][A-Za-z0-9_]*=" entry))
                           "a NAME=VALUE string"))
                       "environment entry"))
      (error (+agent-shell-sandbox-delete (list tmp config))
             (signal (car err) (cdr err))))))

(defun +agent-shell-sandbox-delete (paths &optional callback)
  "Delete PATHS, files or folder trees, then call CALLBACK.
CALLBACK gets nil, or a message naming what could not be deleted; it
never runs before this returns.  Without it a failure is reported with
`message'.  Owner write permission comes back first, since some tools
make their trees read-only, such as Bazel's outputs and Go's module
cache.  A symlink is removed, never followed.  What can go goes even
when some path cannot."
  (unless (seq-every-p #'file-name-absolute-p paths)
    (error "Sandbox deletion needs absolute paths, not %S" paths))
  (let ((default-directory "/")
        (buffer (generate-new-buffer " *pstack-sandbox-delete*" t)))
    (make-process
     :name "pstack-sandbox-delete" :buffer buffer :noquery t
     :connection-type 'pipe
     :command (append (list "/bin/sh" "-c" "/bin/chmod -R u+rwX -- \"$@\" \
2>/dev/null; exec /bin/rm -rf -- \"$@\"" "sh")
                      paths)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let* ((output (with-current-buffer buffer
                          (string-trim (buffer-string))))
                (error (unless (eql 0 (process-exit-status process))
                         (if (string-empty-p output)
                             (format "rm exited with status %s"
                                     (process-exit-status process))
                           output))))
           (kill-buffer buffer)
           (cond (callback (funcall callback error))
                 (error (message "Could not delete %s: %s"
                                 (string-join paths ", ") error)))))))))

(defun +agent-shell-sandbox-discard (sandbox &optional callback)
  "Delete SANDBOX's private folders as `+agent-shell-sandbox-delete' does.
CALLBACK gets nil or a message saying what stayed."
  (+agent-shell-sandbox-delete (list (+agent-shell-sandbox-tmp sandbox)
                                     (+agent-shell-sandbox-config sandbox))
                               callback))

(defun +agent-shell-sandbox--string (path)
  "Return PATH as an SBPL string literal."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" path) "\""))

(defvar +agent-shell-sandbox--user-folders 'unknown
  "The macOS per-user (TEMP . CACHE) folders as true names, or nil.")

(defun +agent-shell-sandbox--user-folders ()
  "Return macOS's per-user (TEMP . CACHE) folders, or nil elsewhere."
  (when (eq +agent-shell-sandbox--user-folders 'unknown)
    (setq +agent-shell-sandbox--user-folders
          (when-let* (((eq system-type 'darwin))
                      (temp (car (+agent-shell-sandbox--lines
                                  "/usr/bin/getconf" "DARWIN_USER_TEMP_DIR")))
                      (cache (car (+agent-shell-sandbox--lines
                                   "/usr/bin/getconf" "DARWIN_USER_CACHE_DIR"))))
            (cons (+agent-shell-sandbox--path temp)
                  (+agent-shell-sandbox--path cache)))))
  +agent-shell-sandbox--user-folders)

(defun +agent-shell-sandbox--toolchain-writes ()
  "Return the allowances for per-user folders toolchains write on macOS.
Each is exact, since the temp folder also holds other programs' files,
such as the Emacs server socket.  TemporaryItems is written by
xcodebuild, xcrun_db and its siblings by xcrun, hsperfdata_USER by the
JVM, and the cache's com.apple.DeveloperTools by xcodebuild."
  (when-let* ((folders (+agent-shell-sandbox--user-folders)))
    (let ((temp (car folders)))
      (list (concat temp "/TemporaryItems")
            (cons 'regexp (concat "^" (+agent-shell-sandbox-regexp-quote
                                       (concat temp "/xcrun_db"))))
            (concat temp "/hsperfdata_" (user-login-name))
            (concat (cdr folders) "/com.apple.DeveloperTools")))))

(defun +agent-shell-sandbox--filters (allowances)
  "Return the SBPL filters for ALLOWANCES, paths or patterns, each spaced."
  (mapconcat (lambda (allowance)
               (if (+agent-shell-sandbox-pattern-p allowance)
                   (format " (regex #\"%s\")" (cdr allowance))
                 (format " (subpath %s)"
                         (+agent-shell-sandbox--string allowance))))
             allowances ""))

(defun +agent-shell-sandbox--read-denials (sandbox)
  "Return the paths and patterns SANDBOX's agent may not read."
  (append (+agent-shell-sandbox-deny-reads sandbox)
          (+agent-shell-sandbox--private-patterns)))

(defun +agent-shell-sandbox--read-allowances (sandbox)
  "Return the paths and patterns SANDBOX's agent may read despite denials."
  (append (list (+agent-shell-sandbox-tmp sandbox)
                (+agent-shell-sandbox-config sandbox))
          (+agent-shell-sandbox-allow-reads sandbox)))

(defun +agent-shell-sandbox--under-p (path directories)
  "Return non-nil if PATH is one of DIRECTORIES or inside one."
  (seq-some (lambda (directory)
              (or (equal path directory)
                  (string-prefix-p (file-name-as-directory directory) path)))
            directories))

(defun +agent-shell-sandbox-readable-p (sandbox path)
  "Return non-nil if SANDBOX's profile lets its agent read PATH.
This applies the profile's read rules in Emacs, erring towards refusal:
a denying pattern refuses every path starting with its literal prefix,
and an allowing pattern opens nothing, so only a path allowance reopens
a denied path."
  (let ((path (+agent-shell-sandbox--path path)))
    (or (not (seq-some
              (lambda (denial)
                (if (+agent-shell-sandbox-pattern-p denial)
                    (string-prefix-p (+agent-shell-sandbox--pattern-prefix
                                      (cdr denial))
                                     path)
                  (+agent-shell-sandbox--under-p path (list denial))))
              (+agent-shell-sandbox--read-denials sandbox)))
        (+agent-shell-sandbox--under-p
         path (seq-remove #'+agent-shell-sandbox-pattern-p
                          (+agent-shell-sandbox--read-allowances sandbox))))))

(defun +agent-shell-sandbox-profile (sandbox)
  "Return the sandbox-exec profile text for SANDBOX.
Every rule that refuses an operation tags the kernel's log of it with
SANDBOX's tag.  In trace mode each such rule allows the operation with a
report instead."
  (let* ((tag (+agent-shell-sandbox--string (+agent-shell-sandbox-tag
                                              sandbox)))
         (refuse (lambda (operation filters)
                   (if (+agent-shell-sandbox-trace sandbox)
                       (format "(allow %s (with report) (with message %s)%s)\n"
                               operation tag filters)
                     (format "(deny %s (with message %s)%s)\n"
                             operation tag filters)))))
    (concat
     "(version 1)\n(allow default)\n" (funcall refuse "file-write*" "")
     (format "(allow file-write*%s)\n"
             (+agent-shell-sandbox--filters
              (append (list (+agent-shell-sandbox-root sandbox)
                            (+agent-shell-sandbox-tmp sandbox)
                            (+agent-shell-sandbox-config sandbox)
                            "/dev")
                      (+agent-shell-sandbox-allow-writes sandbox)
                      (+agent-shell-sandbox--toolchain-writes))))
     ;; A worktree's .git file points git at the lead's repository.
     (funcall refuse "file-write*"
              (format " (literal %s)"
                      (+agent-shell-sandbox--string
                       (concat (+agent-shell-sandbox-root sandbox) "/.git"))))
     (funcall refuse "file-read*" (+agent-shell-sandbox--filters
                                   (+agent-shell-sandbox--read-denials
                                    sandbox)))
     ;; A later rule overrides an earlier one.
     (format "(allow file-read*%s)\n"
             (+agent-shell-sandbox--filters
              (+agent-shell-sandbox--read-allowances sandbox)))
     (when-let* ((stat (+agent-shell-sandbox-allow-metadata sandbox)))
       (format "(allow file-read-metadata%s)\n"
               (mapconcat (lambda (path)
                            (format " (literal %s)"
                                    (+agent-shell-sandbox--string path)))
                          stat "")))
     (funcall refuse "network-outbound" " (remote unix-socket)")
     (format "(allow network-outbound%s)\n"
             (mapconcat (lambda (path)
                          (format " (remote unix-socket (path-literal %s))"
                                  (+agent-shell-sandbox--string path)))
                        (cons "/private/var/run/mDNSResponder"
                              (+agent-shell-sandbox-allow-connects sandbox))
                        "")))))

(defun +agent-shell-sandbox-entry (agent)
  "Return AGENT's entry in `+agent-shell-sandbox-agents', or signal."
  (or (alist-get agent +agent-shell-sandbox-agents)
      (user-error "No sandboxed launch for agent %s" agent)))

(defun +agent-shell-sandbox-mode (agent)
  "Return AGENT's full-access session mode."
  (plist-get (+agent-shell-sandbox-entry agent) :mode))

(defun +agent-shell-sandbox-environment (sandbox)
  "Return the environment every process confined to SANDBOX gets.
That is SANDBOX's own entries, then these defaults.  TMPDIR points most
tools at the private temp folder.  GOTMPDIR does the same for the Go
toolchain's work folders, which it would put in /tmp.  GOCACHE gives each
sandbox its own Go build cache: the shared one under ~/Library/Caches is
outside the sandbox, and an entry written there would feed the user's own
builds.  GOTELEMETRY=off stops Go tools writing telemetry under ~/Library.
JAVA_TOOL_OPTIONS gains -Djava.io.tmpdir, since the JVM ignores TMPDIR on
macOS and extracts native libraries into the default temp folder.
GIT_OPTIONAL_LOCKS=0 keeps read-only commands such as git status from
refreshing the index, which lives in a git directory the agent cannot
write."
  (let ((tmp (+agent-shell-sandbox-tmp sandbox))
        (java (getenv "JAVA_TOOL_OPTIONS")))
    (append (+agent-shell-sandbox-env sandbox)
            (list (concat "TMPDIR=" tmp)
                  (concat "GOTMPDIR=" tmp)
                  (concat "GOCACHE=" tmp "/go-build")
                  "GOTELEMETRY=off"
                  (concat "JAVA_TOOL_OPTIONS="
                          (and java (not (string-empty-p java))
                               (concat java " "))
                          "-Djava.io.tmpdir=" tmp)
                  "GIT_OPTIONAL_LOCKS=0"))))

(defun +agent-shell-sandbox-client-maker (sandbox agent client-maker)
  "Prepare SANDBOX for AGENT and return CLIENT-MAKER confined to it.
The returned maker runs the client's command under sandbox-exec with the
prepared environment, then `+agent-shell-sandbox-environment', ahead of
the client's own."
  (let* ((entry (+agent-shell-sandbox-entry agent))
         (profile (expand-file-name "sandbox.sb"
                                    (+agent-shell-sandbox-config sandbox)))
         (environment (progn
                        (setf (+agent-shell-sandbox-agent sandbox) agent)
                        (append (funcall (plist-get entry :prepare) sandbox)
                                (+agent-shell-sandbox-environment
                                 sandbox)))))
    (with-temp-file profile (insert (+agent-shell-sandbox-profile sandbox)))
    (lambda (buffer)
      (when-let* ((client (funcall client-maker buffer)))
        (let* ((client (copy-alist client))
               (command (alist-get :command client)))
          (setf (alist-get :command client) "/usr/bin/sandbox-exec"
                (alist-get :command-params client)
                (append (list "-f" profile
                              (or (executable-find command) command))
                        (alist-get :command-params client))
                (alist-get :environment-variables client)
                (append environment
                        (alist-get :environment-variables client)))
          client)))))

(defun +agent-shell-sandbox--keychain (service)
  "Return the password of Keychain item SERVICE, or signal a user error."
  (with-temp-buffer
    (if (eql 0 (call-process +agent-shell-sandbox--security nil '(t nil) nil
                             "find-generic-password" "-s" service "-w"))
        (string-trim-right (buffer-string))
      (user-error "No Keychain item %s; store a `claude setup-token' token \
with: security add-generic-password -a \"$USER\" -s %s -w" service service))))

(defun +agent-shell-sandbox-prepare-claude (sandbox)
  "Return Claude Code's environment for SANDBOX, signing in by token."
  (let ((tmp (+agent-shell-sandbox-tmp sandbox)))
    (list (concat "CLAUDE_CODE_OAUTH_TOKEN="
                  (+agent-shell-sandbox--keychain
                   +agent-shell-sandbox-claude-token-service))
          "ANTHROPIC_API_KEY="
          (concat "CLAUDE_CONFIG_DIR=" (+agent-shell-sandbox-config sandbox))
          (concat "CLAUDE_CODE_TMPDIR=" tmp))))

(defun +agent-shell-sandbox--hash (file)
  "Return FILE's SHA-256, or nil when it is missing."
  (when (file-readable-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun +agent-shell-sandbox--mtime (file)
  "Return FILE's modification time."
  (file-attribute-modification-time (file-attributes file)))

(defun +agent-shell-sandbox-prepare-codex (sandbox)
  "Copy the Codex login into SANDBOX and return Codex's environment."
  (let* ((home (expand-file-name +agent-shell-sandbox-codex-home))
         (config (+agent-shell-sandbox-config sandbox))
         (real (expand-file-name "auth.json" home))
         (copy (expand-file-name "auth.json" config))
         (settings (expand-file-name "config.toml" home)))
    (unless (file-readable-p real)
      (user-error "No Codex login at %s; run codex login" real))
    (copy-file real copy t)
    (set-file-modes copy #o600)
    (when (file-readable-p settings)
      (copy-file settings (expand-file-name "config.toml" config) t))
    (setf (+agent-shell-sandbox-credentials sandbox)
          (list :real real :copy copy
                :real-hash (+agent-shell-sandbox--hash real)
                :copied (+agent-shell-sandbox--mtime copy)))
    (list (concat "CODEX_HOME=" config))))

(defun +agent-shell-sandbox-finish-codex (sandbox)
  "Copy SANDBOX's Codex login back if the agent refreshed it.
Only when the copy changed after it was made and the real login did not."
  (cl-destructuring-bind (&key real copy real-hash copied &allow-other-keys)
      (+agent-shell-sandbox-credentials sandbox)
    (when (and copy (file-readable-p copy)
               (time-less-p copied (+agent-shell-sandbox--mtime copy))
               (equal real-hash (+agent-shell-sandbox--hash real)))
      (let ((temporary (make-temp-file
                        (expand-file-name ".auth.json." (file-name-directory
                                                         real)))))
        (copy-file copy temporary t)
        (set-file-modes temporary #o600)
        (rename-file temporary real t)))))

(defun +agent-shell-sandbox-finish (sandbox)
  "Run the :finish step of SANDBOX's agent, if it has one.
A failure is reported with `message' rather than signaled, so the rest
of a stop still runs."
  (when-let* ((agent (+agent-shell-sandbox-agent sandbox))
              (finish (plist-get (+agent-shell-sandbox-entry agent) :finish)))
    (condition-case err (funcall finish sandbox)
      (error (message "Finishing the %s sandbox at %s failed: %s" agent
                      (+agent-shell-sandbox-root sandbox)
                      (error-message-string err))))))

(defun +agent-shell-sandbox--lines (program &rest arguments)
  "Return the output lines of PROGRAM run with ARGUMENTS."
  ;; From a sandbox folder, the sweep would find PROGRAM itself.
  (let ((default-directory "/"))
    (with-temp-buffer
      (apply #'call-process program nil '(t nil) nil arguments)
      (split-string (buffer-string) "\n" t))))

(defun +agent-shell-sandbox--descendants (pid parents)
  "Return the descendants of PID given PARENTS, an alist of (PID . PPID)."
  (let ((frontier (list pid)) found)
    (while frontier
      (let ((parent (pop frontier)))
        (pcase-dolist (`(,child . ,ppid) parents)
          (when (and (eql ppid parent) (not (memql child found)))
            (push child found)
            (push child frontier)))))
    (nreverse found)))

(defun +agent-shell-sandbox--kill (pids)
  "Send SIGKILL to each of PIDS except this Emacs."
  (dolist (pid pids)
    (unless (eql pid (emacs-pid))
      (ignore-errors (signal-process pid 'SIGKILL)))))

(defun +agent-shell-sandbox-kill-tree (pid)
  "Kill every descendant of PID, whatever its process group."
  (+agent-shell-sandbox--kill
   (+agent-shell-sandbox--descendants
    pid (mapcar (lambda (line)
                  (let ((fields (split-string line)))
                    (cons (string-to-number (car fields))
                          (string-to-number (cadr fields)))))
                (+agent-shell-sandbox--lines "/bin/ps" "-axo" "pid=,ppid=")))))

(defun +agent-shell-sandbox--working-in (directories)
  "Return the pids of processes whose working directory is under DIRECTORIES."
  ;; Filtering every cwd beats `lsof +D', which walks the whole tree.
  (let ((prefixes (mapcar #'file-name-as-directory directories))
        pid pids)
    (dolist (line (+agent-shell-sandbox--lines "/usr/sbin/lsof" "-w" "-n" "-P" "-d" "cwd"
                                               "-F" "pn"))
      (pcase (aref line 0)
        (?p (setq pid (string-to-number (substring line 1))))
        (?n (let ((path (file-name-as-directory (substring line 1))))
              (when (and pid (not (eql pid (emacs-pid)))
                         (seq-some (lambda (prefix)
                                     (string-prefix-p prefix path))
                                   prefixes))
                (cl-pushnew pid pids))))))
    pids))

(defun +agent-shell-sandbox-sweep (sandbox callback &optional rounds)
  "Kill processes working under SANDBOX's root or temp folder, then call back.
Repeat from a timer until none remain or ROUNDS sweeps have run, then call
CALLBACK with the pids that survived."
  (let ((rounds (or rounds +agent-shell-sandbox--sweeps))
        (pids (+agent-shell-sandbox--working-in
               (list (+agent-shell-sandbox-root sandbox)
                     (+agent-shell-sandbox-tmp sandbox)))))
    (if (or (null pids) (<= rounds 0))
        (funcall callback pids)
      (+agent-shell-sandbox--kill pids)
      (run-at-time 0.2 nil #'+agent-shell-sandbox-sweep
                   sandbox callback (1- rounds)))))

(provide '+agent-shell-sandbox)
;;; +agent-shell-sandbox.el ends here
