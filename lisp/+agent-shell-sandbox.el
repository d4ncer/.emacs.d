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
;; Stopping takes two steps, in order: `+agent-shell-sandbox-kill-tree'
;; while the adapter lives, then `+agent-shell-sandbox-sweep' once it is
;; gone.  `+agent-shell-drive-stop' runs them around killing the shell.  `+agent-shell-sandbox-discard' then deletes
;; the private folders, whatever modes the agent's tools left in them.
;;
;; On GNU/Linux, `+agent-shell-sandbox-backend' is `bwrap' and the same
;; sandbox shows an allowlist instead of hiding a denylist.
;; `+agent-shell-sandbox-linux-mounts' lists what the agent sees: the
;; system folders read-only, its root, private temp and config folders
;; writable, a private home in its temp folder, and the allowances as
;; binds of existing paths.  The adapter runs through
;; +agent-shell-sandbox-launch.sh, which gives it its own user, network,
;; PID and IPC namespaces with slirp4netns as its only route out, and an
;; environment holding only `+agent-shell-sandbox-linux-environment'.
;; The same mounts decide what `+agent-shell-sandbox-readable-p' allows.
;; On both backends agents receive the host login's access token and no
;; refresh credential, so nothing is copied back.  Linux stopping uses
;; `+agent-shell-sandbox-terminate', since the PID namespace takes every
;; process with it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar server-socket-dir)

(defgroup +agent-shell-sandbox nil
  "Confine agent shells with sandbox-exec."
  :group 'agent-shell)

(defvar +agent-shell-sandbox-backend
  (pcase system-type ('darwin 'sandbox-exec) ('gnu/linux 'bwrap))
  "How sandboxes confine agents here: `sandbox-exec', `bwrap' or nil.
nil means this system has no sandbox, and creating one signals.")

(defcustom +agent-shell-sandbox-linux-system
  '("/usr" "/etc" "/nix" "/opt" "/bin" "/sbin" "/lib" "/lib64" "/lib32"
    "/libx32")
  "System paths every Linux sandbox shows read-only.
A symlink, such as /bin on a merged /usr, is made the same symlink
inside.  A missing path is skipped."
  :type '(repeat directory))

(defcustom +agent-shell-sandbox-linux-masks '("/nix/var/nix/daemon-socket")
  "Folders inside the system paths every Linux sandbox shows empty.
A unix socket accepts connections even on a read-only mount, so each
folder holding one is covered.  The default holds the Nix daemon's."
  :type '(repeat directory))

(defcustom +agent-shell-sandbox-claude-credentials "~/.claude/.credentials.json"
  "Claude Code host login file used by the bwrap backend.
The sandbox-exec backend reads the host login Keychain instead."
  :type 'file)

(defcustom +agent-shell-sandbox-agents
  '((claude-code :mode "bypassPermissions"
                 :prepare +agent-shell-sandbox-prepare-claude
                 :name "Claude Code"
                 :login +agent-shell-sandbox--claude-login
                 :refresh ("claude" "-p" "Reply with the single word OK.")
                 :relogin "Log Claude Code in again on the host: run claude \
there and use /login.")
    (codex :mode "agent-full-access"
           :prepare +agent-shell-sandbox-prepare-codex
           :name "Codex"
           :login +agent-shell-sandbox--codex-login
           :refresh ("codex" "exec" "--skip-git-repo-check"
                     "Reply with the single word OK.")
           :relogin "A refresh of Codex's login could not be triggered; log \
Codex in again on the host with codex login."))
  "The single registry for sandbox launch and host login policy.
:mode names the full-access session mode.  :prepare receives a sandbox
and returns environment entries, using a fresh projected host login.
:login returns a `+agent-shell-sandbox--login' or signals a sanitized
user error; it is the only reader of this provider's host credential
and expiry on either backend.  :name names the provider, :refresh is
its host CLI command, and :relogin gives host login instructions.
Every arena member, including its judge, must have a :login reader.
No sandbox may refresh a host login or copy credentials back."
  :type '(alist :key-type symbol :value-type plist))

(defvar +agent-shell-sandbox--security "/usr/bin/security"
  "macOS's Keychain tool, by absolute path so no other one on PATH runs.")

(defcustom +agent-shell-sandbox-login-margin 600
  "Seconds every checked host login must outlast the supplied deadline.
Applied once by `+agent-shell-sandbox-check-logins' on both backends."
  :type 'natnum)

(defcustom +agent-shell-sandbox-refresh-timeout 180
  "Seconds a host CLI refresh may run while holding its login lease."
  :type 'natnum)

(defvar +agent-shell-sandbox--login-tick 1
  "Seconds between attempts to acquire a host login lease.
This is independent of remote transport polling.")

(defcustom +agent-shell-sandbox-codex-home "~/.codex"
  "Host Codex home supplying projected OAuth logins and filtered settings.
Sandboxes never write back to this directory."
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
  (root nil :read-only t :documentation "Working tree, writable unless \
READ-ONLY.")
  (tmp nil :read-only t :documentation "Private temp folder, /tmp/pa-HEX.")
  (config nil :read-only t :documentation "Private agent config folder.")
  (deny-reads nil :read-only t :documentation "Folders the agent cannot read.")
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
         :documentation "Whether the profile reports instead of refusing.")
  (deadline nil :read-only t
            :documentation "Time the agent's access token must outlast, or \
nil for now.")
  (read-only nil :read-only t
             :documentation "Whether the agent may write only its private \
folders, not ROOT."))

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
  (cond ((and (+agent-shell-sandbox-pattern-p value)
              (eq +agent-shell-sandbox-backend 'bwrap))
         "an absolute path, since a Linux sandbox mounts each allowance \
and a (regexp . RE) pattern names no path")
        ((+agent-shell-sandbox-pattern-p value)
         (+agent-shell-sandbox--pattern-error (cdr value)))
        ((not (and (stringp value) (file-name-absolute-p value)))
         "an absolute path or a (regexp . RE) pattern")))

(cl-defun +agent-shell-sandbox-create (root &key deny-reads allow-reads
                                             allow-metadata allow-writes
                                             allow-connects env trace
                                             deadline read-only)
  "Return a sandbox writing to ROOT, creating its private folders.
With READ-ONLY non-nil the agent works in ROOT but writes only its
private temp and config folders and ALLOW-WRITES.
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
logs each report or refusal with the sandbox's tag as its message.

On Linux the sandbox shows only `+agent-shell-sandbox-linux-mounts', so
DENY-READS and ALLOW-METADATA change nothing there.  ALLOW-READS and
ALLOW-WRITES are read-only and writable binds of the paths that exist,
patterns are refused, each of ALLOW-CONNECTS must be an existing socket,
and TRACE is refused.

On either backend, the injected access token must strictly outlast
DEADLINE, an absolute time, or the present when it is nil."
  (unless +agent-shell-sandbox-backend
    (user-error "No sandbox on %s: agents run confined only on macOS and \
GNU/Linux" system-type))
  (when (and trace (eq +agent-shell-sandbox-backend 'bwrap))
    (user-error "Sandbox trace mode needs macOS's sandbox-exec; it is not \
available on Linux"))
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
                        :tag tag :trace (and trace t) :deadline deadline
                        :read-only (and read-only t)
                        resolved)))
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
                 :allow-connects
                 (mapcar (lambda (socket)
                           (let ((path (+agent-shell-sandbox--path socket)))
                             (when (and (eq +agent-shell-sandbox-backend 'bwrap)
                                        (not (file-exists-p path)))
                               (user-error "Sandbox socket %s does not exist; \
a Linux sandbox can only bind an existing one" socket))
                             path))
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

(defun +agent-shell-sandbox-own-writes (sandbox)
  "Return the folders SANDBOX's agent may write besides its allowances."
  (append (unless (+agent-shell-sandbox-read-only sandbox)
            (list (+agent-shell-sandbox-root sandbox)))
          (list (+agent-shell-sandbox-tmp sandbox)
                (+agent-shell-sandbox-config sandbox))))

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
a denied path.  On Linux it allows only what the agent sees inside its
namespace, by `+agent-shell-sandbox-linux-visible-p'."
  (let ((path (+agent-shell-sandbox--path path)))
    (if (eq +agent-shell-sandbox-backend 'bwrap)
        (+agent-shell-sandbox-linux-visible-p
         path (+agent-shell-sandbox-linux-mounts sandbox))
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
                            (+agent-shell-sandbox--read-allowances sandbox)))))))

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
              (append (+agent-shell-sandbox-own-writes sandbox)
                      '("/dev")
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
the client's own.  On Linux it runs the command as
`+agent-shell-sandbox-linux-client-maker' does."
  (if (eq +agent-shell-sandbox-backend 'bwrap)
      (+agent-shell-sandbox-linux-client-maker sandbox agent client-maker)
    (let* ((entry (+agent-shell-sandbox-entry agent))
           (profile (expand-file-name "sandbox.sb"
                                      (+agent-shell-sandbox-config sandbox)))
           (environment
            (append (funcall (plist-get entry :prepare) sandbox)
                    (+agent-shell-sandbox-environment sandbox))))
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
            client))))))

(defun +agent-shell-sandbox-prepare-claude (sandbox)
  "Return Claude Code's environment for SANDBOX, signed in by access token.
The token is the host login's, read fresh, and must outlast SANDBOX's
deadline.  No refresh credential reaches the sandbox."
  (let ((login (+agent-shell-sandbox--read-login 'claude-code)))
    (+agent-shell-sandbox--outlast login
                                   (+agent-shell-sandbox-deadline sandbox))
    (list (concat "CLAUDE_CODE_OAUTH_TOKEN="
                  (+agent-shell-sandbox--login-access login))
          "ANTHROPIC_API_KEY="
          (concat "CLAUDE_CONFIG_DIR=" (+agent-shell-sandbox-config sandbox))
          (concat "CLAUDE_CODE_TMPDIR=" (+agent-shell-sandbox-tmp sandbox)))))

(defun +agent-shell-sandbox--codex-auth (login)
  "Return a Codex auth.json signing in with projected LOGIN's access token.
It keeps the ID token and account id, which Codex needs, and its refresh
token is empty, so the agent cannot rotate the host's login."
  (json-serialize
   `((auth_mode . "chatgpt")
     (OPENAI_API_KEY . :null)
     (tokens (id_token . ,(+agent-shell-sandbox--login-id-token login))
             (access_token . ,(+agent-shell-sandbox--login-access login))
             (refresh_token . "")
             (account_id . ,(+agent-shell-sandbox--login-account-id login)))
     ,@(when-let* ((refreshed (+agent-shell-sandbox--login-last-refresh login)))
         `((last_refresh . ,refreshed))))))

(defun +agent-shell-sandbox-prepare-codex (sandbox)
  "Write Codex's login and settings into SANDBOX; return its environment.
The login holds the host's access and ID tokens but no refresh token,
and must outlast SANDBOX's deadline.  The settings are the host's.
Nothing is ever copied back."
  (let* ((login (+agent-shell-sandbox--read-login 'codex))
         (config (+agent-shell-sandbox-config sandbox))
         (settings (expand-file-name "config.toml"
                                     +agent-shell-sandbox-codex-home))
         (text (when (file-readable-p settings)
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally settings)
                   (buffer-string)))))
    (+agent-shell-sandbox--outlast login
                                   (+agent-shell-sandbox-deadline sandbox))
    (with-file-modes #o600
      (let ((coding-system-for-write 'no-conversion))
        (write-region (+agent-shell-sandbox--codex-auth login) nil
                      (expand-file-name "auth.json" config) nil 'silent)
        (when text
          (write-region text nil (expand-file-name "config.toml" config)
                        nil 'silent))))
    (list (concat "CODEX_HOME=" config) "OPENAI_API_KEY=" "CODEX_API_KEY=")))

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
CALLBACK with the pids that survived.  On Linux every process an agent
starts lives in its PID namespace, which ends with its launcher, so
there is nothing to sweep and CALLBACK gets nil."
  (let ((rounds (or rounds +agent-shell-sandbox--sweeps))
        (pids (unless (eq +agent-shell-sandbox-backend 'bwrap)
                (+agent-shell-sandbox--working-in
                 (list (+agent-shell-sandbox-root sandbox)
                       (+agent-shell-sandbox-tmp sandbox))))))
    (if (or (null pids) (<= rounds 0))
        (funcall callback pids)
      (+agent-shell-sandbox--kill pids)
      (run-at-time 0.2 nil #'+agent-shell-sandbox-sweep
                   sandbox callback (1- rounds)))))

;;;; Linux

(defun +agent-shell-sandbox--home (sandbox)
  "Return SANDBOX's private home folder on Linux, inside its temp folder."
  (concat (+agent-shell-sandbox-tmp sandbox) "/home"))

(defun +agent-shell-sandbox--resolver (sandbox)
  "Return the resolver file SANDBOX's agent reads on Linux."
  (concat (+agent-shell-sandbox-config sandbox) "/resolv.conf"))

(defun +agent-shell-sandbox-linux-mounts (sandbox &optional reads)
  "Return the mounts SANDBOX's agent sees on Linux, in bwrap's order.
Each is (KIND DEST SOURCE).  KIND `ro' or `rw' binds SOURCE at DEST,
`symlink' makes DEST a link to SOURCE, and `tmpfs', `proc' and `dev'
make a fresh DEST.  A parent comes before what it holds, so the deeper
mount wins, as it does in the kernel.

The agent sees `+agent-shell-sandbox-linux-system' read-only with
`+agent-shell-sandbox-linux-masks' empty, and fresh /proc, /dev, /tmp
and /run.  Where /etc/resolv.conf leads it finds a file naming
slirp4netns's DNS server.  Its root, temp and config folders are
writable but for ROOT/.git, or only the latter two when the sandbox is
read-only, its read and write allowances that exist and
READS, more paths, are read-only and writable binds, and its sockets are
bound in."
  (let ((root (+agent-shell-sandbox-root sandbox))
        (binds (lambda (kind paths)
                 (mapcar (lambda (path) (list kind path path))
                         (seq-filter #'file-exists-p paths)))))
    (sort
     (append
      (mapcan (lambda (path)
                (cond ((file-symlink-p path)
                       (list (list 'symlink path (file-symlink-p path))))
                      ((file-directory-p path) (list (list 'ro path path)))))
              +agent-shell-sandbox-linux-system)
      '((proc "/proc") (dev "/dev") (tmpfs "/tmp") (tmpfs "/run"))
      (mapcar (lambda (path) (list 'tmpfs path))
              (seq-filter #'file-directory-p +agent-shell-sandbox-linux-masks))
      (when (file-exists-p "/etc/resolv.conf")
        (list (list 'ro (file-truename "/etc/resolv.conf")
                    (+agent-shell-sandbox--resolver sandbox))))
      (funcall binds 'ro (append (+agent-shell-sandbox-allow-reads sandbox)
                                 reads))
      (when (+agent-shell-sandbox-read-only sandbox)
        (funcall binds 'ro (list root)))
      (funcall binds 'rw (+agent-shell-sandbox-own-writes sandbox))
      (funcall binds 'ro (list (concat root "/.git")))
      (funcall binds 'rw (+agent-shell-sandbox-allow-writes sandbox))
      (mapcar (lambda (socket) (list 'rw socket socket))
              (+agent-shell-sandbox-allow-connects sandbox)))
     :key (lambda (mount) (length (split-string (cadr mount) "/" t))))))

(defun +agent-shell-sandbox-linux-visible-p (path mounts)
  "Return non-nil if true name PATH reads the same inside MOUNTS as here.
That holds when the deepest of MOUNTS holding PATH binds it in place."
  (pcase (car (last (seq-filter (lambda (mount)
                                  (+agent-shell-sandbox--under-p
                                   path (list (cadr mount))))
                                mounts)))
    (`(,(or 'ro 'rw) ,dest ,source) (equal dest source))))

(defun +agent-shell-sandbox-linux-environment (sandbox mounts)
  "Return the entries a Linux sandbox adds for SANDBOX with MOUNTS.
PATH keeps the folders of this Emacs's PATH that MOUNTS show, HOME is
the private home, and LANG and TERM come from this Emacs, defaulting to
C.UTF-8 and dumb."
  (list (concat "PATH="
                (string-join
                 (seq-uniq
                  (seq-filter (lambda (folder)
                                (and (file-directory-p folder)
                                     (+agent-shell-sandbox-linux-visible-p
                                      (+agent-shell-sandbox--path folder)
                                      mounts)))
                              (split-string (or (getenv "PATH") "") ":" t)))
                 ":"))
        (concat "HOME=" (+agent-shell-sandbox--home sandbox))
        (concat "LANG=" (or (getenv "LANG") "C.UTF-8"))
        (concat "TERM=" (or (getenv "TERM") "dumb"))))

(defun +agent-shell-sandbox-linux-arguments (mounts command)
  "Return bwrap's arguments running COMMAND, a list, inside MOUNTS.
The agent runs as this user in its own user, IPC and PID namespaces and
session, and dies with its launcher."
  (append (list "--unshare-user" "--uid" (number-to-string (user-uid))
                "--gid" (number-to-string (group-gid))
                "--unshare-ipc" "--unshare-pid" "--die-with-parent"
                "--new-session")
          (mapcan (pcase-lambda (`(,kind ,dest ,source))
                    (pcase kind
                      ('ro (list "--ro-bind" source dest))
                      ('rw (list "--bind" source dest))
                      ('symlink (list "--symlink" source dest))
                      (_ (list (format "--%s" kind) dest))))
                  mounts)
          (cons "--" command)))

(defun +agent-shell-sandbox--launcher ()
  "Return the launcher script's file name."
  (or (locate-file "+agent-shell-sandbox-launch.sh" load-path)
      (error "No +agent-shell-sandbox-launch.sh on `load-path'")))

(defun +agent-shell-sandbox--install (program)
  "Return the folder PROGRAM needs to run: its npm node_modules or its own."
  (let ((true (file-truename program)))
    (if (string-match "\\`\\(.*?/node_modules\\)/" true)
        (match-string 1 true)
      (directory-file-name (file-name-directory true)))))

(defun +agent-shell-sandbox-linux-client-maker (sandbox agent client-maker)
  "Prepare SANDBOX for AGENT and return CLIENT-MAKER confined to it on Linux.
The returned maker runs the client's command through
+agent-shell-sandbox-launch.sh inside SANDBOX's mounts, with the folder
it runs from, such as its npm install, added read-only.  Its environment
holds only the prepared entries, then `+agent-shell-sandbox-environment'
and `+agent-shell-sandbox-linux-environment'.  The client's own entries
are dropped, since they may hold this Emacs's whole environment."
  (let ((prepared (funcall (plist-get (+agent-shell-sandbox-entry agent)
                                    :prepare)
                           sandbox)))
    (make-directory (+agent-shell-sandbox--home sandbox) t)
    (with-temp-file (+agent-shell-sandbox--resolver sandbox)
      (insert "nameserver 10.0.2.3\n"))
    (lambda (buffer)
      (when-let* ((client (funcall client-maker buffer)))
        (let* ((client (copy-alist client))
               (command (alist-get :command client))
               (program (or (executable-find command) command))
               (install (+agent-shell-sandbox--install program))
               (mounts (+agent-shell-sandbox-linux-mounts
                        sandbox
                        (unless (+agent-shell-sandbox-linux-visible-p
                                 install
                                 (+agent-shell-sandbox-linux-mounts sandbox))
                          (list install))))
               (environment
                (seq-uniq (append prepared
                                  (+agent-shell-sandbox-environment sandbox)
                                  (+agent-shell-sandbox-linux-environment
                                   sandbox mounts))
                          (lambda (a b)
                            (equal (car (split-string a "="))
                                   (car (split-string b "=")))))))
          (setf (alist-get :command client) "/bin/bash"
                (alist-get :command-params client)
                (append (list (+agent-shell-sandbox--launcher)
                              (mapconcat (lambda (entry)
                                           (car (split-string entry "=")))
                                         environment " "))
                        (+agent-shell-sandbox-linux-arguments
                         mounts (cons program
                                      (alist-get :command-params client))))
                (alist-get :environment-variables client) environment)
          client)))))

;;;; Host logins and arena admission (both backends)

(defconst +agent-shell-sandbox--claude-service "Claude Code-credentials"
  "Keychain service of Claude Code's own login on macOS.")

(defvar +agent-shell-sandbox--login-locks
  "~/.local/state/pstack-arena/login-locks/"
  "Folder of this user's host login locks.
Fixed, so every Emacs on the host, local or a remote runner, shares them.")

(cl-defstruct (+agent-shell-sandbox--login
               (:constructor +agent-shell-sandbox--make-login)
               (:copier nil))
  "Private projection of one host OAuth login and that token's expiry.
No refresh credential, API key or raw host object is representable.
Identity fields are required for Codex only.  Never store this in panel
state, check results, diagnostics, or remote wire data."
  (agent nil :read-only t)
  (access nil :read-only t)
  (expires nil :read-only t)
  (id-token nil :read-only t)
  (account-id nil :read-only t)
  (last-refresh nil :read-only t))

(cl-defstruct (+agent-shell-sandbox-login-result
               (:constructor +agent-shell-sandbox-login-result-create)
               (:copier nil))
  "Secret-free admission result for one AGENT, safe for remote status.
REFRESHED says this check successfully refreshed it.  REMAINING is seconds
left at final validation, nil without a readable login.  ERROR is nil on
success, otherwise sanitized refusal/relogin guidance.  Any error refuses
an entire arena, including providers needed only by the judge."
  (agent nil :read-only t)
  (refreshed nil :read-only t)
  (remaining nil :read-only t)
  (error nil :read-only t))

(cl-defstruct (+agent-shell-sandbox--login-check
               (:constructor +agent-shell-sandbox--make-login-check)
               (:copier nil))
  "Private owner of one asynchronous admission operation.
It owns its timer, refresh PROCESS and LEASE, the lock folder it holds
with its NONCE.  It never holds a projected login."
  agents deadline need callback progress results cancelled done timer
  process nonce lease)

(defun +agent-shell-sandbox--json-get (object &rest keys)
  "Return the value at KEYS in parsed JSON OBJECT, or nil."
  (dolist (key keys object)
    (setq object (and (consp object) (alist-get key object)))))

(defun +agent-shell-sandbox--parse-login (text name)
  "Return host login TEXT parsed as JSON, or signal naming agent NAME.
The parser's message is dropped, since it could quote the credential."
  (condition-case nil
      (json-parse-string text :object-type 'alist :null-object nil)
    (error (user-error "%s's host login is not valid JSON" name))))

(defun +agent-shell-sandbox--finite-p (number)
  "Return whether NUMBER is a finite number."
  (and (numberp number) (= number number) (/= (abs number) 1.0e+INF)))

(defun +agent-shell-sandbox--read-login (agent)
  "Return AGENT's host login through its registry :login reader.
Signal a `user-error' without credential contents when AGENT has no
reader or its login is missing, unreadable or lacks a finite expiry."
  (let* ((entry (alist-get agent +agent-shell-sandbox-agents))
         (name (or (plist-get entry :name) (symbol-name agent)))
         (reader (plist-get entry :login))
         (login (if (not reader)
                    (user-error "%s has no host login reader, so it cannot \
run sandboxed in an arena" name)
                  (condition-case err (funcall reader)
                    (user-error (signal (car err) (cdr err)))
                    (error (user-error "%s's host login could not be read"
                                       name))))))
    (unless (and (+agent-shell-sandbox--login-p login)
                 (stringp (+agent-shell-sandbox--login-access login))
                 (not (string-empty-p (+agent-shell-sandbox--login-access
                                       login)))
                 (+agent-shell-sandbox--finite-p
                  (+agent-shell-sandbox--login-expires login)))
      (user-error "%s's host login has no access token with a known expiry"
                  name))
    login))

(defun +agent-shell-sandbox--claude-login ()
  "Return the host's Claude Code login, projected.
On macOS it is the login Keychain's, on Linux
`+agent-shell-sandbox-claude-credentials'.  There is no other source."
  (let* ((relogin "run claude on this host and use /login")
         (oauth
          (+agent-shell-sandbox--json-get
           (+agent-shell-sandbox--parse-login
            (if (eq +agent-shell-sandbox-backend 'bwrap)
                (let ((file (expand-file-name
                             +agent-shell-sandbox-claude-credentials)))
                  (unless (file-readable-p file)
                    (user-error "No Claude Code login at %s; %s" file relogin))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
              (with-temp-buffer
                (let ((default-directory "/"))
                  (unless (eql 0 (call-process
                                  +agent-shell-sandbox--security nil '(t nil)
                                  nil "find-generic-password"
                                  "-s" +agent-shell-sandbox--claude-service
                                  "-a" (user-login-name) "-w"))
                    (user-error "No readable Claude Code login in this host's \
login Keychain; %s" relogin)))
                (buffer-string)))
            "Claude Code")
           'claudeAiOauth))
         (access (+agent-shell-sandbox--json-get oauth 'accessToken))
         (expires (+agent-shell-sandbox--json-get oauth 'expiresAt)))
    (unless (and (stringp access) (not (string-empty-p access))
                 (+agent-shell-sandbox--finite-p expires))
      (user-error "Claude Code's host login has no access token with an \
expiry; %s" relogin))
    (+agent-shell-sandbox--make-login
     :agent 'claude-code :access access :expires (/ expires 1000.0))))

(defun +agent-shell-sandbox--codex-login ()
  "Return the host's Codex ChatGPT login from its auth.json, projected."
  (let* ((file (expand-file-name "auth.json" +agent-shell-sandbox-codex-home))
         (login (if (file-readable-p file)
                    (+agent-shell-sandbox--parse-login
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "Codex")
                  (user-error "No Codex login at %s; run codex login on this \
host" file)))
         (tokens (+agent-shell-sandbox--json-get login 'tokens))
         (fields (mapcar (lambda (key)
                           (let ((value (+agent-shell-sandbox--json-get
                                         tokens key)))
                             (and (stringp value) (not (string-empty-p value))
                                  value)))
                         '(access_token id_token account_id)))
         (expires (and (car fields)
                       (+agent-shell-sandbox--jwt-expiry (car fields))))
         (refreshed (+agent-shell-sandbox--json-get login 'last_refresh)))
    (unless (and (seq-every-p #'identity fields) expires)
      (user-error "No ChatGPT login with a known expiry for Codex at %s; run \
codex login on this host" file))
    (+agent-shell-sandbox--make-login
     :agent 'codex :access (nth 0 fields) :expires expires
     :id-token (nth 1 fields) :account-id (nth 2 fields)
     :last-refresh (and (stringp refreshed) refreshed))))

(defun +agent-shell-sandbox--jwt-expiry (token)
  "Return the exp claim of JSON web token TOKEN as a float, or nil.
This reads the expiry only; it does not verify the signature."
  (when-let* ((parts (split-string token "\\."))
              ((= 3 (length parts))))
    (condition-case nil
        (let ((expiry (alist-get 'exp (json-parse-string
                                       (decode-coding-string
                                        (base64-decode-string
                                         (string-trim-right (nth 1 parts) "=+")
                                         t)
                                        'utf-8)
                                       :object-type 'alist))))
          (and (+agent-shell-sandbox--finite-p expiry) (float expiry)))
      (error nil))))

(defun +agent-shell-sandbox--outlast (login deadline)
  "Signal unless projected LOGIN's token strictly outlasts DEADLINE.
DEADLINE is a time, or nil for the present."
  (let ((deadline (float-time deadline))
        (expires (+agent-shell-sandbox--login-expires login))
        (entry (alist-get (+agent-shell-sandbox--login-agent login)
                          +agent-shell-sandbox-agents)))
    (unless (< deadline expires)
      (user-error "%s's access token expires at %s, before the sandbox's \
deadline %s.  %s"
                  (or (plist-get entry :name)
                      (+agent-shell-sandbox--login-agent login))
                  (format-time-string "%F %T %Z" expires)
                  (format-time-string "%F %T %Z" deadline)
                  (or (plist-get entry :relogin)
                      "Refresh this host's login first.")))))

(defun +agent-shell-sandbox-check-logins (agents deadline callback
                                                 &optional progress)
  "Admit AGENTS for an arena ending at absolute Unix time DEADLINE.
Each distinct agent's host login must outlast DEADLINE plus
`+agent-shell-sandbox-login-margin'.  A short one is refreshed at most
once, under a host-wide lock, and every login is read again before the
check succeeds.  CALLBACK gets a `+agent-shell-sandbox-login-result' for
each agent, in order, exactly once and never before this returns.
PROGRESS, when given, gets the results so far as each agent's lands.
Return a function that cancels the check: it stops waiting and starts no
refresh, but lets one already running finish before CALLBACK runs."
  (let ((check (+agent-shell-sandbox--make-login-check
                :agents (seq-uniq agents) :deadline deadline
                :need (+ deadline +agent-shell-sandbox-login-margin)
                :callback callback :progress progress
                :nonce (format "%08x%08x" (random #x100000000)
                               (random #x100000000)))))
    (setf (+agent-shell-sandbox--login-check-timer check)
          (run-at-time 0 nil #'+agent-shell-sandbox--check-next check))
    (lambda () (+agent-shell-sandbox--check-cancel check))))

(defun +agent-shell-sandbox--check-cancel (check)
  "Cancel CHECK, waking a check that waits for a lock."
  (unless (or (+agent-shell-sandbox--login-check-cancelled check)
              (+agent-shell-sandbox--login-check-done check))
    (setf (+agent-shell-sandbox--login-check-cancelled check) t)
    (when-let* ((timer (+agent-shell-sandbox--login-check-timer check)))
      (cancel-timer timer)
      (timer-set-time timer (current-time))
      (timer-activate timer))))

(defun +agent-shell-sandbox--check-next (check)
  "Check the next agent of CHECK, or end it once none is left."
  (setf (+agent-shell-sandbox--login-check-timer check) nil)
  (let ((results (+agent-shell-sandbox--login-check-results check)))
    (if-let* ((agent (nth (length results)
                          (+agent-shell-sandbox--login-check-agents check))))
        (+agent-shell-sandbox--check-login
         check agent
         (lambda (result)
           (setf (+agent-shell-sandbox--login-check-results check)
                 (append results (list result)))
           (when-let* ((progress (+agent-shell-sandbox--login-check-progress
                                  check)))
             (funcall progress (+agent-shell-sandbox--login-check-results
                                check)))
           (+agent-shell-sandbox--check-next check)))
      (unless (+agent-shell-sandbox--login-check-cancelled check)
        (setf (+agent-shell-sandbox--login-check-results check)
              (mapcar (lambda (result)
                        (if (+agent-shell-sandbox-login-result-error result)
                            result
                          (+agent-shell-sandbox--reread check result)))
                      results)))
      (setf (+agent-shell-sandbox--login-check-done check) t)
      (funcall (+agent-shell-sandbox--login-check-callback check)
               (+agent-shell-sandbox--login-check-results check)))))

(defun +agent-shell-sandbox--try-login (agent)
  "Return (LOGIN . ERROR) for AGENT's host login; one of them is nil."
  (condition-case err (list (+agent-shell-sandbox--read-login agent))
    (user-error (cons nil (error-message-string err)))))

(defun +agent-shell-sandbox--login-result (check agent read refreshed
                                                 &optional why)
  "Return CHECK's result for AGENT, whose login READ found.
READ is from `+agent-shell-sandbox--try-login'.  REFRESHED says CHECK
refreshed it, and WHY, when non-nil, why AGENT is refused."
  (let ((login (car read))
        (entry (alist-get agent +agent-shell-sandbox-agents)))
    (+agent-shell-sandbox-login-result-create
     :agent agent :refreshed refreshed
     :remaining (and login (max 0 (round (- (+agent-shell-sandbox--login-expires
                                             login)
                                            (float-time)))))
     :error
     (when why
       (format "%s's login on this host %s, short of the arena's deadline %s \
plus %d minutes, and %s.  %s"
               (or (plist-get entry :name) agent)
               (if login
                   (format-time-string "expires at %F %T %Z"
                                       (+agent-shell-sandbox--login-expires
                                        login))
                 (format "could not be used (%s)"
                         (string-trim-right (cdr read) "\\.")))
               (format-time-string
                "%F %T %Z" (+agent-shell-sandbox--login-check-deadline check))
               (/ +agent-shell-sandbox-login-margin 60)
               why
               (or (plist-get entry :relogin)
                   "Log it in again on the host."))))))

(defun +agent-shell-sandbox--enough-p (check read)
  "Return whether login READ outlasts what CHECK needs."
  (when-let* ((login (car read)))
    (> (+agent-shell-sandbox--login-expires login)
       (+agent-shell-sandbox--login-check-need check))))

(defun +agent-shell-sandbox--reread (check result)
  "Return RESULT of CHECK after reading its agent's login once more."
  (let* ((agent (+agent-shell-sandbox-login-result-agent result))
         (read (+agent-shell-sandbox--try-login agent))
         (refreshed (+agent-shell-sandbox-login-result-refreshed result)))
    (if (+agent-shell-sandbox--enough-p check read)
        (+agent-shell-sandbox--login-result check agent read refreshed)
      (+agent-shell-sandbox--login-result
       check agent read refreshed "it changed before admission finished"))))

(defun +agent-shell-sandbox--check-login (check agent callback)
  "Check AGENT's host login for CHECK, then call CALLBACK with its result.
A short or missing login is read again under AGENT's lock and refreshed
once if still short.  A check that waited for another's lock only reads
again.  Waiting is bounded at twice `+agent-shell-sandbox-refresh-timeout'."
  (let ((entry (alist-get agent +agent-shell-sandbox-agents))
        (give-up (+ (float-time) (* 2 +agent-shell-sandbox-refresh-timeout)))
        (waited nil))
    (cl-labels
        ((result (read refreshed &optional why)
           (funcall callback (+agent-shell-sandbox--login-result
                              check agent read refreshed why)))
         (release-with (read refreshed &optional why)
           (+agent-shell-sandbox--login-release check)
           (result read refreshed why))
         (refreshed (failure)
           (let ((read (+agent-shell-sandbox--try-login agent)))
             (cond (failure (release-with read nil (concat "refreshing it \
failed: " failure)))
                   ((+agent-shell-sandbox--enough-p check read)
                    (release-with read t))
                   (t (release-with read nil "a refresh did not extend it")))))
         (attempt ()
           (setf (+agent-shell-sandbox--login-check-timer check) nil)
           (if (+agent-shell-sandbox--login-check-cancelled check)
               (funcall callback
                        (+agent-shell-sandbox-login-result-create
                         :agent agent
                         :error "Login admission was cancelled."))
             (let ((read (+agent-shell-sandbox--try-login agent)))
               (cond
                ((+agent-shell-sandbox--enough-p check read) (result read nil))
                ((not (plist-get entry :login))
                 (result read nil "Emacs has no way to read it"))
                ((not (plist-get entry :refresh))
                 (result read nil "it has no host refresh command"))
                ((+agent-shell-sandbox--login-acquire check agent)
                 (let ((read (+agent-shell-sandbox--try-login agent)))
                   (cond
                    ((+agent-shell-sandbox--enough-p check read)
                     (release-with read nil))
                    (waited (release-with read nil "another refresh did not \
extend it"))
                    (t (+agent-shell-sandbox--refresh check agent
                                                      #'refreshed)))))
                ((> (float-time) give-up)
                 (result read nil "another refresh held its lock too long"))
                (t (setq waited t)
                   (setf (+agent-shell-sandbox--login-check-timer check)
                         (run-at-time +agent-shell-sandbox--login-tick nil
                                      #'attempt))))))))
      (attempt))))

(defun +agent-shell-sandbox--login-lock (agent)
  "Return the host lock folder for refreshing AGENT's login.
It depends on nothing but this user and AGENT."
  (expand-file-name (format "login-%s" agent)
                    +agent-shell-sandbox--login-locks))

(defun +agent-shell-sandbox--lock-owner (lock)
  "Return LOCK's (PID . NONCE), or nil when unknown."
  (ignore-errors
    (pcase (split-string (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "owner" lock))
                           (buffer-string)))
      (`(,pid ,nonce) (and (string-match-p "\\`[0-9]+\\'" pid)
                           (cons (string-to-number pid) nonce))))))

(defun +agent-shell-sandbox--login-acquire (check agent)
  "Take AGENT's login lock for CHECK; return whether it did.
A lock whose owner no longer runs is renamed aside first, so only one
waiter can take it over.  A live owner, even another check in this
Emacs, keeps it."
  (let ((lock (+agent-shell-sandbox--login-lock agent))
        (nonce (+agent-shell-sandbox--login-check-nonce check)))
    (with-file-modes #o700
      (make-directory (file-name-directory lock) t))
    (condition-case nil
        (progn
          (with-file-modes #o700 (make-directory lock))
          (with-temp-file (expand-file-name "owner" lock)
            (insert (format "%d %s\n" (emacs-pid) nonce)))
          (setf (+agent-shell-sandbox--login-check-lease check) lock)
          t)
      (file-already-exists
       (let ((owner (+agent-shell-sandbox--lock-owner lock)))
         (when (and owner (/= (car owner) (emacs-pid))
                    (not (process-attributes (car owner))))
           (let ((aside (format "%s.stale-%d-%s" lock (emacs-pid) nonce)))
             (when (ignore-errors (rename-file lock aside) t)
               (delete-directory aside t)
               (+agent-shell-sandbox--login-acquire check agent)))))))))

(defun +agent-shell-sandbox--login-release (check)
  "Give up CHECK's lock if its pid and nonce still own it.
Never while CHECK's refresh runs."
  (when-let* ((lock (+agent-shell-sandbox--login-check-lease check))
              ((not (process-live-p
                     (+agent-shell-sandbox--login-check-process check)))))
    (when (equal (+agent-shell-sandbox--lock-owner lock)
                 (cons (emacs-pid)
                       (+agent-shell-sandbox--login-check-nonce check)))
      (delete-directory lock t))
    (setf (+agent-shell-sandbox--login-check-lease check) nil)))

(defun +agent-shell-sandbox--host-environment ()
  "Return `process-environment' for a host CLI refreshing its own login.
Sandbox sign-in overrides are dropped and CODEX_HOME is the host's."
  (cons (concat "CODEX_HOME=" (directory-file-name
                               (expand-file-name
                                +agent-shell-sandbox-codex-home)))
        (seq-remove (lambda (entry)
                      (string-match-p "\\`\\(?:CLAUDE_CODE_OAUTH_TOKEN\
\\|ANTHROPIC_API_KEY\\|CLAUDE_CONFIG_DIR\\|CLAUDE_CODE_TMPDIR\
\\|OPENAI_API_KEY\\|CODEX_API_KEY\\|CODEX_HOME\\)=" entry))
                    process-environment)))

(defun +agent-shell-sandbox--refresh (check agent callback)
  "Run AGENT's :refresh command on the host for CHECK, then call CALLBACK.
It runs outside any sandbox, from a scratch folder with no project, on
the host's own login, and is stopped after
`+agent-shell-sandbox-refresh-timeout' seconds.  CALLBACK gets nil once
it succeeds, or why it failed; its output, which could hold credentials,
is discarded."
  (let* ((command (plist-get (alist-get agent +agent-shell-sandbox-agents)
                             :refresh))
         (program (car command))
         (scratch (make-temp-file "pstack-login-" t))
         (default-directory (file-name-as-directory scratch))
         (process-environment (+agent-shell-sandbox--host-environment))
         (timeout +agent-shell-sandbox-refresh-timeout)
         (timed-out nil)
         (timer nil))
    (cl-flet ((finish (failure)
                (when timer (cancel-timer timer))
                (setf (+agent-shell-sandbox--login-check-process check) nil)
                (ignore-errors (delete-directory scratch t))
                (funcall callback failure)))
      (condition-case nil
          (let ((process
                 (make-process
                  :name "pstack-login-refresh" :command command
                  :connection-type 'pipe :noquery t :filter #'ignore
                  :sentinel
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (finish
                       (cond (timed-out (format "%s timed out after %d s"
                                                program timeout))
                             ((eql 0 (process-exit-status process)) nil)
                             (t (format "%s exited with status %d" program
                                        (process-exit-status process))))))))))
            (setf (+agent-shell-sandbox--login-check-process check) process)
            (process-send-eof process)
            (setq timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (when (process-live-p process)
                       (setq timed-out t)
                       (signal-process process 'TERM)
                       (setq timer (run-at-time
                                    10 nil (lambda ()
                                             (when (process-live-p process)
                                               (signal-process
                                                process 'KILL))))))))))
        (error (run-at-time 0 nil #'finish
                            (format "%s could not be started" program)))))))

;;;; Linux shutdown

(defun +agent-shell-sandbox-terminate (process callback &optional seconds)
  "Stop PROCESS, a Linux sandbox's launcher, then call CALLBACK.
On SIGTERM the launcher kills its PID namespace and exits once every
process in it has.  CALLBACK gets nil, or PROCESS's pid in a list when
it still ran after SECONDS, 10 by default, and was killed outright."
  (let ((deadline (+ (float-time) (or seconds 10)))
        (pid (process-id process)))
    (when (process-live-p process)
      (signal-process process 'SIGTERM))
    (cl-labels ((poll ()
                  (cond ((not (process-live-p process)) (funcall callback nil))
                        ((> (float-time) deadline)
                         (signal-process process 'SIGKILL)
                         (funcall callback (list pid)))
                        (t (run-at-time 0.05 nil #'poll)))))
      (poll))))

(provide '+agent-shell-sandbox)
;;; +agent-shell-sandbox.el ends here
