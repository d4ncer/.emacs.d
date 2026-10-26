;;; +agent-shell-sandbox.el --- Confine agent shells with sandbox-exec -*- lexical-binding: t; -*-

;;; Commentary:
;; A `+agent-shell-sandbox' describes where a confined agent may write: its
;; root, a short private temp folder and a private agent config folder.  It
;; also lists folders the agent may not read.  `+agent-shell-sandbox-profile'
;; renders it as a macOS sandbox-exec profile, and
;; `+agent-shell-sandbox-client-maker' wraps an agent config's client maker
;; so the adapter starts under that profile with the agent's private
;; environment from `+agent-shell-sandbox-agents'.
;;
;; Stopping takes three steps, in order: `+agent-shell-sandbox-kill-tree'
;; while the adapter lives, `+agent-shell-sandbox-sweep' once it is gone,
;; then `+agent-shell-sandbox-finish'.  `+agent-shell-drive-stop' runs them
;; around killing the shell.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

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

(defcustom +agent-shell-sandbox-claude-token-service
  "pstack-arena-claude-token"
  "Keychain service holding a `claude setup-token' token for candidates."
  :type 'string)

(defcustom +agent-shell-sandbox-codex-home "~/.codex"
  "Codex home whose login and config sandboxed Codex agents copy."
  :type 'directory)

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
               :documentation "Folders inside DENY-READS the agent may read.")
  (allow-metadata nil :read-only t
                  :documentation "Paths whose metadata alone it may read.")
  (allow-writes nil :read-only t
                :documentation "Folders beyond its own it may write.")
  (env nil :read-only t
       :documentation "Environment entries ahead of the defaults."))

(defun +agent-shell-sandbox--path (path)
  "Return PATH's true name without a trailing slash."
  (directory-file-name (file-truename (expand-file-name path))))

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

(defun +agent-shell-sandbox--resolve (sandbox items valid-p kind)
  "Return ITEMS with each function called with SANDBOX, flattened.
A function returns one value or a list.  Signal `user-error' for a value
VALID-P rejects, naming KIND."
  (mapcan (lambda (item)
            (let ((values (if (functionp item) (funcall item sandbox) item)))
              (mapcar (lambda (value)
                        (unless (funcall valid-p value)
                          (user-error "Sandbox %s must be %s, not %S"
                                      (car kind) (cdr kind) value))
                        value)
                      (if (listp values) values (list values)))))
          items))

(cl-defun +agent-shell-sandbox-create (root &key config deny-reads
                                             allow-reads allow-metadata
                                             allow-writes env)
  "Return a sandbox writing to ROOT, creating its private folders.
CONFIG is the private agent config folder, a new temporary one by default.
DENY-READS lists folders the agent may not read, and ALLOW-READS folders
inside them it may read anyway.  A read is checked against the path a
symlink resolves to, so a link into a denied folder needs an allowance.
ALLOW-METADATA lists paths, each exactly, whose metadata the agent may
read even inside DENY-READS, such as a folder git must stat.
ALLOW-WRITES lists more folders the agent may write, and ENV NAME=VALUE
entries that win over `+agent-shell-sandbox-environment's defaults.

Each element of ALLOW-READS, ALLOW-WRITES and ENV may instead be a
function called with the new sandbox, whose root and private folders
exist, returning one path or entry or a list of them."
  (unless (file-directory-p root)
    (user-error "Sandbox root %s is not a directory" root))
  (let* ((tmp (+agent-shell-sandbox--path (+agent-shell-sandbox--make-tmp)))
         (own-config (not config))
         (config (+agent-shell-sandbox--path
                  (if config (progn (make-directory config t) config)
                    (make-temp-file "pa-config-" t))))
         (make (lambda (&rest resolved)
                 (apply #'+agent-shell-sandbox--make
                        :root (+agent-shell-sandbox--path root)
                        :tmp tmp :config config
                        :deny-reads (mapcar #'+agent-shell-sandbox--path
                                            deny-reads)
                        :allow-metadata (mapcar #'+agent-shell-sandbox--path
                                                allow-metadata)
                        resolved)))
         (provisional (funcall make))
         (path-p (lambda (path) (and (stringp path)
                                     (file-name-absolute-p path))))
         (paths (lambda (items kind)
                  (mapcar #'+agent-shell-sandbox--path
                          (+agent-shell-sandbox--resolve
                           provisional items path-p
                           (cons kind "an absolute path"))))))
    (condition-case err
        (funcall make
                 :allow-reads (funcall paths allow-reads "read allowance")
                 :allow-writes (funcall paths allow-writes "write allowance")
                 :env (+agent-shell-sandbox--resolve
                       provisional env
                       (lambda (entry)
                         (and (stringp entry)
                              (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*="
                                              entry)))
                       '("environment entry" . "a NAME=VALUE string")))
      (error (delete-directory tmp t)
             (when own-config (delete-directory config t))
             (signal (car err) (cdr err))))))

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
                                  "getconf" "DARWIN_USER_TEMP_DIR")))
                      (cache (car (+agent-shell-sandbox--lines
                                   "getconf" "DARWIN_USER_CACHE_DIR"))))
            (cons (+agent-shell-sandbox--path temp)
                  (+agent-shell-sandbox--path cache)))))
  +agent-shell-sandbox--user-folders)

(defun +agent-shell-sandbox--toolchain-writes ()
  "Return SBPL filters for per-user folders toolchains write on macOS.
Each is exact, since the temp folder also holds other programs' files,
such as the Emacs server socket.  TemporaryItems is written by
xcodebuild, xcrun_db and its siblings by xcrun, hsperfdata_USER by the
JVM, and the cache's com.apple.DeveloperTools by xcodebuild."
  (when-let* ((folders (+agent-shell-sandbox--user-folders)))
    (let ((temp (car folders))
          (subpath (lambda (path)
                     (format "(subpath %s)" (+agent-shell-sandbox--string
                                             path)))))
      (list (funcall subpath (concat temp "/TemporaryItems"))
            (format "(regex #\"^%s\")"
                    (replace-regexp-in-string
                     "[][.^$*+?(){}|\\\"]" "\\\\\\&"
                     (concat temp "/xcrun_db")))
            (funcall subpath (concat temp "/hsperfdata_" (user-login-name)))
            (funcall subpath (concat (cdr folders)
                                     "/com.apple.DeveloperTools"))))))

(defun +agent-shell-sandbox-profile (sandbox)
  "Return the sandbox-exec profile text for SANDBOX."
  (let ((subpaths (lambda (paths)
                    (mapconcat (lambda (path)
                                 (format " (subpath %s)"
                                         (+agent-shell-sandbox--string path)))
                               paths ""))))
    (concat
     "(version 1)\n(allow default)\n(deny file-write*)\n"
     (format "(allow file-write*%s)\n"
             (concat
              (funcall subpaths (append
                                 (list (+agent-shell-sandbox-root sandbox)
                                       (+agent-shell-sandbox-tmp sandbox)
                                       (+agent-shell-sandbox-config sandbox)
                                       "/dev")
                                 (+agent-shell-sandbox-allow-writes sandbox)))
              (mapconcat (lambda (filter) (concat " " filter))
                         (+agent-shell-sandbox--toolchain-writes) "")))
     ;; A worktree's .git file points git at the lead's repository.
     (format "(deny file-write* (literal %s))\n"
             (+agent-shell-sandbox--string
              (concat (+agent-shell-sandbox-root sandbox) "/.git")))
     (when-let* ((denied (+agent-shell-sandbox-deny-reads sandbox)))
       (format "(deny file-read*%s)\n" (funcall subpaths denied)))
     ;; A later rule overrides an earlier one.
     (when-let* ((allowed (+agent-shell-sandbox-allow-reads sandbox)))
       (format "(allow file-read*%s)\n" (funcall subpaths allowed)))
     (when-let* ((stat (+agent-shell-sandbox-allow-metadata sandbox)))
       (format "(allow file-read-metadata%s)\n"
               (mapconcat (lambda (path)
                            (format " (literal %s)"
                                    (+agent-shell-sandbox--string path)))
                          stat "")))
     "(deny network-outbound (remote unix-socket))\n"
     "(allow network-outbound (remote unix-socket (path-literal "
     "\"/private/var/run/mDNSResponder\")))\n")))

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
toolchain's work folders, which it would put in /tmp, and GOTELEMETRY=off
stops Go tools writing telemetry under ~/Library.
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
    (if (eql 0 (call-process "security" nil '(t nil) nil
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
  "Run the :finish step of SANDBOX's agent, if it has one."
  (when-let* ((agent (+agent-shell-sandbox-agent sandbox))
              (finish (plist-get (+agent-shell-sandbox-entry agent) :finish)))
    (funcall finish sandbox)))

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
                (+agent-shell-sandbox--lines "ps" "-axo" "pid=,ppid=")))))

(defun +agent-shell-sandbox--working-in (directories)
  "Return the pids of processes whose working directory is under DIRECTORIES."
  ;; Filtering every cwd beats `lsof +D', which walks the whole tree.
  (let ((prefixes (mapcar #'file-name-as-directory directories))
        pid pids)
    (dolist (line (+agent-shell-sandbox--lines "lsof" "-w" "-n" "-P" "-d" "cwd"
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
