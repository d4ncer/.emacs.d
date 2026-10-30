;;; +agent-shell-drive.el --- Drive agent shells from Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Start agent shells pinned to a directory, submit prompts without clobbering
;; user drafts, and watch their events as parsed plists.  Watchers are keyed by
;; (OWNER . ID); the library owns one upstream subscription per shell.
;;
;; A watcher receives (:kind KIND :shell BUFFER :session ID ...), where KIND
;; is the upstream event symbol.  `turn-complete' adds :stop-reason and :text,
;; plus :turn-text before tool updates split it into parts.  `error' adds
;; :message and the JSON-RPC error's :data.  Both add
;; :provider-errors, the `_meta.codex.error' records the turn's
;; `session_info_update' notifications carried, newest first; each also
;; reaches watchers as a `provider-warning' event with :message, :info and
;; :will-retry.  `turn-complete' also adds :commands, the shell commands
;; the turn's execute tool calls ran, oldest first, for
;; `+agent-shell-drive-commands'.  `+agent-shell-drive-failure' classifies
;; a finished or failed turn.  `agent-message-chunk' only feeds the turn text, and
;; `input-submitted' starts a new turn.  A tool call ends the final part; the
;; first `tool-call-update' after agent text reports that part as :text and
;; the call's :title.
;;
;; A shell started with a policy runs in a mode the policy names and answers
;; permission requests itself.  A `read-only' shell runs in its agent's mode
;; from `+agent-shell-drive-read-only-modes': it approves reads once and
;; rejects everything else.  Its `turn-complete' adds :plan when it rejected
;; a request carrying a plan.  If the session is not in the policy's mode once
;; initialized, the shell is killed and its watchers get `clean-up' with
;; :reason instead of `init-finished'.  Shells with a policy are left out of
;; `agent-shell-buffers', so upstream pickers and project reuse never offer
;; them.
;;
;; A shell whose policy is a `+agent-shell-sandbox' runs its adapter under
;; sandbox-exec, or bwrap on Linux, in the agent's full-access mode,
;; rejects every permission request and keeps no transcript.  Stop it with
;; `+agent-shell-drive-stop', which also ends every process it started.
;;
;; A `full-access' shell runs unconfined, as an ordinary one does, but in
;; the agent's full-access mode from `+agent-shell-sandbox-agents'.  A
;; permission request rejects and ends it, since that mode should never
;; ask.  Stop it with `+agent-shell-drive-stop-unconfined', which ends the
;; adapter's process tree and process groups and nothing else.
;;
;; A shell started with a process lease records who owns its processes in
;; a file before its adapter starts.  With a systemd unit name the adapter
;; runs in a transient scope of that name, which
;; `+agent-shell-drive-stop-lease' stops, taking every descendant with it
;; however it detached.  `+agent-shell-drive-reap-lease' stops what a lease
;; file names after the Emacs that started it died.
;;
;; A watcher added with `+agent-shell-drive-watch-progress' also gets
;; `progress' events: :part `assistant' with each chunk of agent text as
;; :text, and :part `tool' with a tool call's :text title and :status.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-sandbox)

(declare-function acp-send-response "acp")
(declare-function acp-make-error "acp")
(declare-function acp-make-fs-write-text-file-response "acp")
(declare-function acp-make-fs-read-text-file-response "acp")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell--current-mode-id "agent-shell-config")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-session-id "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function +agent-shell-egress-attach "+agent-shell-egress")
(defvar agent-shell-agent-configs)
(defvar agent-shell-session-strategy)
(defvar agent-shell-cwd-function)
(defvar agent-shell-mode-hook)
(defvar agent-shell-permission-responder-function)
(defvar agent-shell-transcript-file-path-function)
(defvar agent-shell--state)

(defgroup +agent-shell-drive nil
  "Drive agent shells from Lisp."
  :group 'agent-shell)

(defcustom +agent-shell-drive-read-only-modes
  '((claude-code . "plan") (codex . "read-only"))
  "Session mode that keeps each agent read-only, by config identifier."
  :type '(alist :key-type symbol :value-type string))

(defconst +agent-shell-drive--read-kinds '("read" "search" "fetch" "think")
  "Tool call kinds a read-only shell approves.")

(defvar +agent-shell-drive--watchers (make-hash-table :test #'equal)
  "Map (OWNER . ID) to (SHELL . FUNCTION).")
(defvar +agent-shell-drive--shells (make-hash-table :test #'eq)
  "Map a watched shell to (:subscription TOKEN :turn CHUNKS :final CHUNKS).
CHUNKS lists text newest first; `+agent-shell-drive--text' joins them.
:provider-errors lists the turn's `_meta.codex.error' records, newest
first.  :commands maps each of the turn's execute tool call ids to the
command it ran, newest first.")

(defvar-local +agent-shell-drive--policy nil
  "Policy this shell was started with; see `+agent-shell-drive-start'.")
(defvar-local +agent-shell-drive--mode nil
  "Session mode this shell's policy requires.")
(defvar-local +agent-shell-drive--plan nil
  "Plan from the last request this shell rejected during the turn.")
(defvar-local +agent-shell-drive--reason nil
  "Why Lisp killed this shell.")
(defvar-local +agent-shell-drive--lease nil
  "Process lease this shell was started with; see `+agent-shell-drive-start'.")

(defvar +agent-shell-drive--progress (make-hash-table :test #'equal)
  "Keys of watchers that also get `progress' events.")

(defvar +agent-shell-drive--error-data nil
  "Data of the JSON-RPC error a driven shell's error handler is handling.")

(defun +agent-shell-drive--configs ()
  "Return `agent-shell-agent-configs' with function entries resolved."
  (require 'agent-shell)
  (mapcar (lambda (entry) (if (functionp entry) (funcall entry) entry))
          (if (functionp agent-shell-agent-configs)
              (funcall agent-shell-agent-configs)
            agent-shell-agent-configs)))

(defun +agent-shell-drive-config (spec)
  "Return the agent config for SPEC, an identifier symbol or config alist."
  (if (and (listp spec) (assq :identifier spec)) spec
    (or (seq-find (lambda (config) (eq (map-elt config :identifier) spec))
                  (+agent-shell-drive--configs))
        (user-error "Unknown agent configuration %s" spec))))

(defun +agent-shell-drive-read-only-agents ()
  "Return configured agent identifiers that have a read-only mode."
  (seq-filter (lambda (id) (alist-get id +agent-shell-drive-read-only-modes))
              (mapcar (lambda (config) (map-elt config :identifier))
                      (+agent-shell-drive--configs))))

(defun +agent-shell-drive-sandbox-agents ()
  "Return configured agent identifiers in `+agent-shell-sandbox-agents'."
  (seq-filter (lambda (id) (alist-get id +agent-shell-sandbox-agents))
              (mapcar (lambda (config) (map-elt config :identifier))
                      (+agent-shell-drive--configs))))

(defun +agent-shell-drive--answer (permission allow)
  "Approve PERMISSION once when ALLOW, otherwise reject it.
Return nil, leaving the dialog to the user, only when no option fits."
  (let* ((tool-call (map-elt permission :tool-call))
         (options (map-elt permission :options))
         (choice (seq-some (lambda (kind)
                             (seq-find (lambda (option)
                                         (equal (map-elt option :kind) kind))
                                       options))
                           (if allow
                               '("allow_once" "reject_once" "reject_always")
                             '("reject_once" "reject_always")))))
    (when choice
      (when-let* (((string-prefix-p "reject" (map-elt choice :kind)))
                  (plan (map-elt (map-elt tool-call :raw-input) 'plan)))
        (setq +agent-shell-drive--plan plan))
      (funcall (map-elt permission :respond) (map-elt choice :option-id))
      t)))

(defun +agent-shell-drive--respond (permission)
  "Approve PERMISSION once if it only reads, otherwise reject it."
  (+agent-shell-drive--answer
   permission (member (map-nested-elt permission '(:tool-call :kind))
                      +agent-shell-drive--read-kinds)))

(defun +agent-shell-drive--reject (permission)
  "Reject PERMISSION."
  (+agent-shell-drive--answer permission nil))

(defun +agent-shell-drive--end (permission)
  "Reject PERMISSION and kill this full-access shell, which should not ask."
  (let ((shell (current-buffer))
        (title (or (map-nested-elt permission '(:tool-call :title)) "a tool")))
    (+agent-shell-drive--reject permission)
    (run-at-time 0 nil #'+agent-shell-drive-kill shell
                 (format "The agent asked permission for %s, which its \
full-access mode should never need." title))
    t))

(defun +agent-shell-drive--refusal (shell)
  "Return why SHELL is not in the mode its policy requires, or nil."
  (require 'agent-shell-config)
  (with-current-buffer shell
    (when-let* ((mode +agent-shell-drive--mode))
      (let ((actual (agent-shell--current-mode-id agent-shell--state)))
        (unless (equal actual mode)
          (format "Session mode is %s, not the %s mode %s"
                  (or actual "unset")
                  (if (eq +agent-shell-drive--policy 'read-only) "read-only"
                    "full-access")
                  mode))))))

(defun +agent-shell-drive-policy (shell)
  "Return the policy live SHELL was started with, or nil."
  (and (buffer-live-p shell)
       (buffer-local-value '+agent-shell-drive--policy shell)))

(defun +agent-shell-drive--hide-owned (shells)
  "Return SHELLS without the shells started with a policy."
  (seq-remove #'+agent-shell-drive-policy shells))

(defun +agent-shell-drive--file-allowed-p (policy write path)
  "Return non-nil if POLICY lets the agent read, or with WRITE write, PATH.
agent-shell serves these requests itself, outside any sandbox-exec profile
and before any permission prompt, so the policy is enforced here too."
  (let ((path (+agent-shell-sandbox--path path)))
    (pcase policy
      ((or 'nil 'full-access) t)
      ('read-only (not write))
      ((pred +agent-shell-sandbox-p)
       (if write
           (and (+agent-shell-sandbox--under-p
                 path (+agent-shell-sandbox-own-writes policy))
                (not (equal path (concat (+agent-shell-sandbox-root policy)
                                         "/.git")))
                (not (+agent-shell-sandbox--under-p
                      path (+agent-shell-sandbox-protect policy))))
         (+agent-shell-sandbox-readable-p policy path)))
      (_ nil))))

(defun +agent-shell-drive--file-gate (write)
  "Return advice that refuses file requests, writes when WRITE, by policy."
  (cl-function
   (lambda (handle &rest args &key state acp-request &allow-other-keys)
     (let ((policy (+agent-shell-drive-policy (map-elt state :buffer)))
           (path (agent-shell--resolve-path
                  (map-nested-elt acp-request '(params path)))))
       (if (+agent-shell-drive--file-allowed-p policy write path)
           (apply handle args)
         (acp-send-response
          :client (map-elt state :client)
          :response (funcall (if write #'acp-make-fs-write-text-file-response
                               #'acp-make-fs-read-text-file-response)
                             :request-id (map-elt acp-request 'id)
                             :error (acp-make-error
                                     :code -32603
                                     :message (format "Refused by %s policy"
                                                      (if (eq policy 'read-only)
                                                          "read-only"
                                                        "sandbox"))))))))))

(defalias '+agent-shell-drive--gate-write (+agent-shell-drive--file-gate t)
  "Refuse ACP file writes that the shell's policy forbids.")
(defalias '+agent-shell-drive--gate-read (+agent-shell-drive--file-gate nil)
  "Refuse ACP file reads that the shell's policy forbids.")

(defun +agent-shell-drive-kill (shell &optional reason)
  "Kill SHELL without prompting; its watchers get `clean-up' with REASON."
  (when (buffer-live-p shell)
    (with-current-buffer shell
      (setq +agent-shell-drive--reason reason)
      ;; shell-maker adds its transcript prompt to the local hook value.
      (setq-local kill-buffer-query-functions nil))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer shell))))

(defun +agent-shell-drive--override (config key value)
  "Return CONFIG with KEY set to VALUE."
  (cons (cons key value) (assq-delete-all key (copy-alist config))))

(defvar +agent-shell-drive-systemd-run "systemd-run"
  "The systemd-run that starts a leased adapter in a scope of its own.")

(defvar +agent-shell-drive-systemctl "systemctl"
  "The systemctl that stops a lease's scope and says whether it ended.")

(defun +agent-shell-drive--write-lease (lease record)
  "Write RECORD, an alist, as LEASE's owner record, atomically and privately."
  (let* ((file (plist-get lease :file))
         (temporary (progn (make-directory (file-name-directory file) t)
                           (make-temp-file (concat file ".")))))
    (set-file-modes temporary #o600)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file temporary (insert (json-serialize record) "\n")))
    (rename-file temporary file t)))

(defun +agent-shell-drive--read-lease (file)
  "Return the owner record in lease FILE as an alist, or nil."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (json-parse-buffer :object-type 'alist :null-object nil))))

(defun +agent-shell-drive--lease-client (lease client)
  "Return CLIENT, an ACP client description, running under LEASE.
LEASE is (:file FILE :unit UNIT).  Its owner record, holding a fresh
nonce and UNIT, is written to FILE before the adapter starts.  With UNIT
the adapter runs in the transient systemd scope UNIT, outermost, so the
sandbox and environment set up on the same launch path go with it; the
scope's processes go when it is stopped, whatever they did meanwhile.
Without UNIT the record later gains the adapter's pid and start time,
and nothing ever signals a pid whose start time differs."
  (let ((unit (plist-get lease :unit))
        (client (copy-alist client)))
    (+agent-shell-drive--write-lease
     lease `((version . 1) (nonce . ,(format "%016x" (random (expt 2 62))))
             (unit . ,(or unit :null))))
    (when unit
      (let ((command (alist-get :command client)))
        (setf (alist-get :command client) +agent-shell-drive-systemd-run
              (alist-get :command-params client)
              (append (list "--user" "--scope" "--quiet" "--collect"
                            (concat "--unit=" unit) "--"
                            (or (executable-find command) command))
                      (alist-get :command-params client)))))
    client))

(defun +agent-shell-drive--note-adapter (shell)
  "Add live SHELL's adapter pid and start time to its process lease."
  (when-let* ((lease (and (buffer-live-p shell)
                          (buffer-local-value '+agent-shell-drive--lease shell)))
              ((not (plist-get lease :unit)))
              (pid (+agent-shell-drive--adapter shell))
              (record (+agent-shell-drive--read-lease (plist-get lease :file)))
              ((not (alist-get 'pid record))))
    (+agent-shell-drive--write-lease
     lease (append `((pid . ,pid)
                     (start . ,(format "%S" (alist-get 'start
                                                       (process-attributes
                                                        pid)))))
                   record))))

(defun +agent-shell-drive-lease (shell)
  "Return the systemd-scoped process lease SHELL was started with, or nil."
  (when-let* ((lease (and (buffer-live-p shell)
                          (buffer-local-value '+agent-shell-drive--lease
                                              shell))))
    (and (plist-get lease :unit) lease)))

(defun +agent-shell-drive--unit-active-p (unit)
  "Return whether systemd still runs UNIT, or `unknown' when it cannot say."
  (let ((default-directory "/"))
    (with-temp-buffer
      (pcase (ignore-errors
               (call-process +agent-shell-drive-systemctl nil t nil "--user"
                             "is-active" unit))
        ('nil 'unknown)
        (_ (and (member (string-trim (buffer-string))
                        '("active" "activating" "deactivating" "reloading"
                          "refreshing"))
                t))))))

(defun +agent-shell-drive--stop-unit (unit callback &optional rounds)
  "Stop systemd scope UNIT and call CALLBACK once it has ended.
CALLBACK gets nil, or a list naming UNIT when it still runs after
ROUNDS checks or systemctl cannot say."
  (let ((default-directory "/"))
    (ignore-errors
      (call-process +agent-shell-drive-systemctl nil nil nil "--user"
                    "stop" unit)))
  (cl-labels ((check (rounds)
                (pcase (+agent-shell-drive--unit-active-p unit)
                  ('nil (funcall callback nil))
                  (_ (if (<= rounds 0)
                         (funcall callback (list unit))
                       (run-at-time 0.5 nil #'check (1- rounds)))))))
    (check (or rounds 20))))

(defun +agent-shell-drive-stop-lease (lease shell callback)
  "Stop SHELL and everything in its LEASE's scope, then call CALLBACK.
CALLBACK gets nil once the scope has ended, or what survived.  The work
runs from a timer, so SHELL's own event callbacks may call this."
  (run-at-time
   0 nil
   (lambda ()
     (+agent-shell-drive-kill shell)
     (+agent-shell-drive--stop-unit (plist-get lease :unit) callback))))

(defun +agent-shell-drive-reap-lease (file callback)
  "Stop what the process lease in FILE owns, after its owner died.
A scope is stopped and awaited.  A recorded adapter is killed with its
process tree and groups only while its pid still has the recorded start
time.  CALLBACK gets nil once nothing it owns runs, or what survived or
could not be verified."
  (let* ((record (+agent-shell-drive--read-lease file))
         (unit (alist-get 'unit record))
         (pid (alist-get 'pid record))
         (start (alist-get 'start record)))
    (cond
     ((null record) (run-at-time 0 nil callback nil))
     (unit (+agent-shell-drive--stop-unit unit callback))
     ((and (natnump pid)
           (equal start (format "%S" (alist-get 'start
                                                (process-attributes pid)))))
      (let* ((processes (+agent-shell-drive--processes))
             (group (nth 2 (assq pid processes)))
             (groups (and group (not (eql group (nth 2 (assq (emacs-pid)
                                                             processes))))
                          (list group))))
        (cl-labels ((sweep (rounds)
                      (let ((pids (+agent-shell-drive--tree
                                   pid groups (+agent-shell-drive--processes))))
                        (when (assq pid (+agent-shell-drive--processes))
                          (push pid pids))
                        (if (or (null pids) (<= rounds 0))
                            (funcall callback pids)
                          (+agent-shell-sandbox--kill pids)
                          (run-at-time 0.2 nil #'sweep (1- rounds))))))
          (sweep +agent-shell-sandbox--sweeps))))
     (t (run-at-time 0 nil callback nil)))))

(cl-defun +agent-shell-drive-start (&key config directory policy environment
                                         lease)
  "Start a new shell from CONFIG pinned to DIRECTORY, without displaying it.
POLICY nil starts an ordinary shell.  With `read-only', the shell runs in its
agent's mode from `+agent-shell-drive-read-only-modes' with a responder that
rejects everything but reads.  With `full-access', the shell runs unconfined
in its agent's full-access mode from `+agent-shell-sandbox-agents', and a
permission request is rejected and kills it.  With a `+agent-shell-sandbox',
its adapter runs confined to the sandbox in that mode, every permission
request is rejected, and no transcript is kept.  A shell with a policy is
hidden from `agent-shell-buffers'.

ENVIRONMENT lists NAME=VALUE entries the adapter and its subprocesses
get ahead of everything else, without touching `process-environment'.
LEASE, (:file FILE :unit UNIT), runs the adapter under a durable process
lease; see `+agent-shell-drive--lease-client'."
  (require 'agent-shell)
  (let* ((config (+agent-shell-drive-config config))
         (agent (map-elt config :identifier))
         (mode (pcase policy
                 ('nil nil)
                 ('read-only
                  (or (alist-get agent +agent-shell-drive-read-only-modes)
                      (user-error "No read-only mode for agent %s" agent)))
                 ((or 'full-access (pred +agent-shell-sandbox-p))
                  (+agent-shell-sandbox-mode agent))
                 (_ (error "Unknown shell policy %S" policy))))
         (sandboxed (+agent-shell-sandbox-p policy))
         (config (if mode
                     (+agent-shell-drive--override
                      config :default-session-mode-id (lambda () mode))
                   config))
         (config (if environment
                     (let ((maker (map-elt config :client-maker)))
                       (+agent-shell-drive--override
                        config :client-maker
                        (lambda (buffer)
                          (when-let* ((client (funcall maker buffer)))
                            (let ((client (copy-alist client)))
                              (setf (alist-get :environment-variables client)
                                    (append environment
                                            (alist-get :environment-variables
                                                       client)))
                              client)))))
                   config))
         (config (if sandboxed
                     (+agent-shell-drive--override
                      config :client-maker
                      (+agent-shell-sandbox-client-maker
                       policy agent (map-elt config :client-maker)))
                   config))
         (config (if lease
                     (let ((maker (map-elt config :client-maker)))
                       (+agent-shell-drive--override
                        config :client-maker
                        (lambda (buffer)
                          (when-let* ((client (funcall maker buffer)))
                            (+agent-shell-drive--lease-client lease client)))))
                   config))
         (directory (file-name-as-directory (file-truename directory)))
         (cwd-function (lambda () directory))
         (pin (lambda ()
                (setq-local agent-shell-cwd-function cwd-function
                            default-directory directory)
                (when policy
                  (setq-local +agent-shell-drive--policy policy
                              +agent-shell-drive--mode mode
                              agent-shell-permission-responder-function
                              (pcase policy
                                ('read-only #'+agent-shell-drive--respond)
                                ('full-access #'+agent-shell-drive--end)
                                (_ #'+agent-shell-drive--reject))))
                (when lease
                  (setq-local +agent-shell-drive--lease lease))
                (when sandboxed
                  (setq-local agent-shell-transcript-file-path-function nil))))
         (shell
          ;; A caller's buffer-local values would shadow these bindings.
          (with-temp-buffer
            (let ((default-directory directory)
                  (agent-shell-cwd-function cwd-function)
                  (agent-shell-mode-hook
                   (append agent-shell-mode-hook (list pin)))
                  (agent-shell-session-strategy 'new)
                  (agent-shell-transcript-file-path-function
                   (unless sandboxed
                     agent-shell-transcript-file-path-function)))
              (save-window-excursion (agent-shell-start :config config))))))
    (with-current-buffer shell (funcall pin))
    (when lease
      (+agent-shell-drive--note-adapter shell)
      (agent-shell-subscribe-to
       :shell-buffer shell :event 'init-finished
       :on-event (lambda (_event) (+agent-shell-drive--note-adapter shell))))
    (when-let* ((session (and sandboxed (+agent-shell-sandbox-egress policy))))
      (+agent-shell-egress-attach
       session (lambda () (+agent-shell-drive--adapter shell))))
    (when policy
      (advice-add 'agent-shell-buffers :filter-return
                  #'+agent-shell-drive--hide-owned)
      (advice-add 'agent-shell--on-fs-write-text-file-request :around
                  #'+agent-shell-drive--gate-write)
      (advice-add 'agent-shell--on-fs-read-text-file-request :around
                  #'+agent-shell-drive--gate-read)
      (agent-shell-subscribe-to
       :shell-buffer shell :event 'init-finished
       :on-event (lambda (_event)
                   (when-let* ((reason (+agent-shell-drive--refusal shell)))
                     (run-at-time 0 nil #'+agent-shell-drive-kill
                                  shell reason)))))
    shell))

(defun +agent-shell-drive--adapter-process (shell)
  "Return live SHELL's agent adapter process while it runs, or nil."
  (when-let* (((buffer-live-p shell))
              (process (map-nested-elt
                        (buffer-local-value 'agent-shell--state shell)
                        '(:client :process)))
              ((process-live-p process)))
    process))

(defun +agent-shell-drive--adapter (shell)
  "Return the pid of live SHELL's agent adapter, or nil."
  (when-let* ((process (+agent-shell-drive--adapter-process shell)))
    (process-id process)))

(defun +agent-shell-drive-stop (shell &optional reason callback)
  "Stop sandboxed SHELL and every process it started, then call CALLBACK.
Kill the adapter's descendants, kill SHELL as `+agent-shell-drive-kill'
does with REASON, then sweep processes working in the sandbox.
CALLBACK gets the pids that survived the sweep.  The
work runs from a timer, so SHELL's own event callbacks may call this.

On Linux the adapter's launcher ends its PID namespace by
`+agent-shell-sandbox-terminate' before SHELL is killed, so no walk or
sweep is needed, and CALLBACK gets the launcher's pid only when it had
to be killed outright."
  (let ((sandbox (+agent-shell-drive-policy shell)))
    (unless (+agent-shell-sandbox-p sandbox)
      (error "Shell %s is not sandboxed" shell))
    (run-at-time
     0 nil
     (lambda ()
       (let ((finish (lambda (survivors)
                       (when callback (funcall callback survivors)))))
         (if (eq +agent-shell-sandbox-backend 'bwrap)
             (let ((process (+agent-shell-drive--adapter-process shell))
                   (kill (lambda (survivors)
                           (+agent-shell-drive-kill shell reason)
                           (funcall finish survivors))))
               (if process (+agent-shell-sandbox-terminate process kill)
                 (funcall kill nil)))
           (when-let* ((pid (+agent-shell-drive--adapter shell)))
             (+agent-shell-sandbox-kill-tree pid))
           (+agent-shell-drive-kill shell reason)
           (+agent-shell-sandbox-sweep sandbox finish)))))))

(defun +agent-shell-drive--processes ()
  "Return (PID PPID PGID ZOMBIE) for every process."
  (mapcar (lambda (line)
            (pcase-let ((`(,pid ,ppid ,pgid ,state) (split-string line)))
              (list (string-to-number pid) (string-to-number ppid)
                    (string-to-number pgid) (string-prefix-p "Z" state))))
          (+agent-shell-sandbox--lines "/bin/ps" "-axo"
                                       "pid=,ppid=,pgid=,stat=")))

(defun +agent-shell-drive--tree (pid groups processes)
  "Return the live pids in PID's tree or GROUPS among PROCESSES.
PROCESSES is a list from `+agent-shell-drive--processes'."
  (let ((found (+agent-shell-sandbox--descendants
                pid (mapcar (lambda (process) (cons (nth 0 process)
                                                    (nth 1 process)))
                            processes))))
    (dolist (process processes)
      (when (memql (nth 2 process) groups)
        (cl-pushnew (nth 0 process) found)))
    (seq-remove (lambda (found)
                  (or (eql found (emacs-pid))
                      (nth 3 (assq found processes))
                      (not (assq found processes))))
                found)))

(defun +agent-shell-drive--groups (pid tree processes)
  "Return the process groups of PID and its TREE among PROCESSES.
Emacs's own group is left out, so walking them never reaches Emacs."
  (let ((group (lambda (member) (nth 2 (assq member processes)))))
    (delete (funcall group (emacs-pid))
            (seq-uniq (mapcar group (cons pid tree))))))

(defun +agent-shell-drive--command-line (pid)
  "Return PID's command line, or its name when the system hides that."
  (let ((attributes (process-attributes pid)))
    (or (alist-get 'args attributes) (alist-get 'comm attributes)
        "unknown")))

(defun +agent-shell-drive--normalize (command)
  "Return COMMAND without quotes or backslashes, its blanks collapsed.
Shells and adapters quote a command differently from how a process
shows it, so both sides of a match drop quoting."
  (string-trim (replace-regexp-in-string
                "[[:space:]]+" " "
                (replace-regexp-in-string "['\"\\]" "" command))))

(defconst +agent-shell-drive--shell-prefix
  "\\`[[:space:]]*\\(?:\\S-*/\\)?\\(?:ba\\|z\\|da\\|k\\)?sh[[:space:]]+-l?c[[:space:]]+"
  "Regexp matching the shell -c an adapter may wrap a command in.")

(defconst +agent-shell-drive--simple-prefix
  "\\`\\(?:\\(?:[[:alpha:]_][[:alnum:]_]*=\\S-*\\|nohup\\|exec\\|setsid\\|command\\)[[:space:]]+\\)+"
  "Regexp matching what a simple command runs under but a process hides.")

(defconst +agent-shell-drive--redirection
  "\\(?:[0-9]*\\|&\\)[<>][<>&|]*[[:space:]]*[^[:space:]<>;&|()]*"
  "Regexp matching a redirection, which a process's command line hides.")

(defun +agent-shell-drive--split (script)
  "Return SCRIPT's parts between shell operators outside quotes.
A SCRIPT that is one quoted string is split inside it."
  (let ((script (string-trim script))
        (parts nil) (start 0) (quote nil) (index 0))
    (while (< index (length script))
      (let ((char (aref script index)))
        (cond (quote (cond ((eq char quote) (setq quote nil))
                           ((and (eq char ?\\) (eq quote ?\"))
                            (setq index (1+ index)))))
              ((memq char '(?\' ?\")) (setq quote char))
              ((eq char ?\\) (setq index (1+ index)))
              ((memq char '(?\; ?& ?| ?\( ?\) ?\n))
               (push (substring script start index) parts)
               (setq start (1+ index)))))
      (setq index (1+ index)))
    (push (substring script (min start (length script))) parts)
    (if (and (null (cdr parts)) (> (length script) 1)
             (memq (aref script 0) '(?\' ?\"))
             (eq (aref script 0) (aref script (1- (length script))))
             (not (string-search (string (aref script 0))
                                 (substring script 1 -1))))
        (+agent-shell-drive--split (substring script 1 -1))
      (nreverse parts))))

(defun +agent-shell-drive--pieces (command)
  "Return the simple commands in COMMAND, each normalized.
A command that ends in & or chains several leaves processes running
only one piece of it, without the shell -c that wrapped it or its
redirections.  A quoted piece, such as a nested sh -c, stays whole."
  (seq-uniq
   (seq-remove
    #'string-empty-p
    (mapcar (lambda (piece)
              (+agent-shell-drive--normalize
               (replace-regexp-in-string
                +agent-shell-drive--simple-prefix ""
                (string-trim
                 (replace-regexp-in-string
                  +agent-shell-drive--redirection " " piece)))))
            ;; With its own shell -c or without, as Codex's argv may be.
            (append (+agent-shell-drive--split
                     (replace-regexp-in-string
                      +agent-shell-drive--shell-prefix "" command))
                    (+agent-shell-drive--split command))))))

(defconst +agent-shell-drive--interpreters
  '("sh" "bash" "zsh" "dash" "ksh" "node" "python" "python3" "ruby" "perl")
  "Programs a process may show running a script, as node runs npm.")

(defconst +agent-shell-drive--eval-wrapper
  (rx bos (? "source " (+? nonl) " && ") "eval " (group (+? nonl))
      (? " < /dev/null") (? " && pwd -P >| " (+ (not space))) eos)
  "Regexp matching Claude Code's wrapper around a command, which it evals.")

(defun +agent-shell-drive--program (command)
  "Return normalized COMMAND naming its program without a directory.
An interpreter running a script, as in node /usr/bin/npm test, gives way
to the script."
  (let ((words (split-string command " " t)))
    (when (and (cdr words)
               (member (file-name-nondirectory (car words))
                       +agent-shell-drive--interpreters)
               (not (string-prefix-p "-" (cadr words))))
      (pop words))
    (when words
      (setcar words (file-name-nondirectory (car words))))
    (string-join words " ")))

(defun +agent-shell-drive--own (line)
  "Return (SCRIPT . COMMAND), what command LINE runs, normalized.
A shell -c, and the eval Claude Code wraps a command in, are stripped;
SCRIPT is then non-nil, since such a shell runs all of COMMAND, never
one piece of it."
  (let ((line (+agent-shell-drive--normalize line)))
    (if (string-match +agent-shell-drive--shell-prefix line)
        (let ((script (substring line (match-end 0))))
          (cons t (+agent-shell-drive--program
                   (if (string-match +agent-shell-drive--eval-wrapper script)
                       (match-string 1 script)
                     script))))
      (cons nil (+agent-shell-drive--program line)))))

(defun +agent-shell-drive--ran-p (line commands)
  "Return whether a process with command LINE runs one of COMMANDS.
Its own command, past any shell wrapper, must be one of COMMANDS from
start to end, past theirs.  It may instead be one of a command's simple
commands, as a background piece or a link of a chain is; a shell is
one only with its -c, as a quoted sh -c in the command shows it.  A
command that merely holds one, such as a hook a shell pipes to or an
MCP server's launcher, does not count."
  (pcase-let ((`(,script . ,own) (+agent-shell-drive--own line)))
    (and (not (string-empty-p own))
         (seq-some
          (lambda (command)
            (or (equal own (cdr (+agent-shell-drive--own command)))
                (member (if script
                            (+agent-shell-drive--program
                             (+agent-shell-drive--normalize line))
                          own)
                        (mapcar #'+agent-shell-drive--program
                                (+agent-shell-drive--pieces command)))))
          commands))))

(defun +agent-shell-drive-commands (shell commands)
  "Return (PID . COMMAND) for each live process running one of COMMANDS.
COMMANDS are what SHELL's tool calls ran, as `turn-complete' events
report them.  The processes are those the walk
`+agent-shell-drive-stop-unconfined' makes finds whose own command, past
any shell wrapper, is one of COMMANDS or one of its simple commands, as
`+agent-shell-drive--ran-p' says, plus their descendants, in pid order;
the rest, such as MCP servers, hooks and execution hosts its adapter
started, are left out.  COMMAND is the process's command line, or its
name when the system hides that.  Return nil while SHELL has no live
adapter or COMMANDS is nil."
  (when-let* ((commands)
              (pid (+agent-shell-drive--adapter shell)))
    (let* ((processes (+agent-shell-drive--processes))
           (tree (+agent-shell-drive--tree pid nil processes))
           (candidates (delq pid (+agent-shell-drive--tree
                                  pid (+agent-shell-drive--groups
                                       pid tree processes)
                                  processes)))
           (lines (mapcar (lambda (candidate)
                            (cons candidate (+agent-shell-drive--command-line
                                             candidate)))
                          candidates))
           (parents (mapcar (lambda (process)
                              (cons (nth 0 process) (nth 1 process)))
                            processes))
           (found nil))
      (dolist (entry lines)
        (when (+agent-shell-drive--ran-p (cdr entry) commands)
          (dolist (member (cons (car entry)
                                (+agent-shell-sandbox--descendants
                                 (car entry) parents)))
            (cl-pushnew member found))))
      (mapcar (lambda (member)
                (or (assq member lines)
                    (cons member (+agent-shell-drive--command-line member))))
              (sort (seq-filter (lambda (member)
                                  (let ((process (assq member processes)))
                                    (and process (not (nth 3 process)))))
                                found)
                    #'<)))))

(defun +agent-shell-drive-stop-unconfined (shell &optional reason callback)
  "Stop unconfined SHELL and every process its adapter started.
Kill the adapter's descendants and every process in their process groups,
which holds what they left behind after their parents ended, then kill
SHELL as `+agent-shell-drive-kill' does with REASON.  Nothing outside
the adapter's tree and groups is signalled.  Recheck from a timer until
none remain, then call CALLBACK with the pids that survived.  The work
runs from a timer, so SHELL's own event callbacks may call this."
  (run-at-time
   0 nil
   (lambda ()
     (let* ((pid (+agent-shell-drive--adapter shell))
            (processes (and pid (+agent-shell-drive--processes)))
            (tree (and pid (+agent-shell-drive--tree pid nil processes)))
            (groups (and pid (+agent-shell-drive--groups pid tree processes)))
            (finish (lambda (survivors)
                      (when callback (funcall callback survivors)))))
       (cl-labels ((sweep (rounds)
                     (let ((pids (and pid (+agent-shell-drive--tree
                                           pid groups
                                           (+agent-shell-drive--processes)))))
                       (if (or (null pids) (<= rounds 0))
                           (funcall finish pids)
                         (+agent-shell-sandbox--kill pids)
                         (run-at-time 0.2 nil #'sweep (1- rounds))))))
         (+agent-shell-sandbox--kill tree)
         (+agent-shell-drive-kill shell reason)
         (sweep +agent-shell-sandbox--sweeps))))))

(defun +agent-shell-drive-pinned-p (shell directory)
  "Return whether live SHELL works in DIRECTORY."
  (and directory (buffer-live-p shell)
       (condition-case nil
           (file-equal-p directory
                         (with-current-buffer shell (agent-shell-cwd)))
         (error nil))))

(defun +agent-shell-drive-send (shell text)
  "Submit TEXT to SHELL, returning non-nil only when it was submitted.
Refuse while SHELL has no session, is not ready, or holds an unsent draft."
  (when (and (buffer-live-p shell)
             (agent-shell-session-id :shell-buffer shell)
             (eq (agent-shell-status :shell-buffer shell) 'ready)
             (with-current-buffer shell
               (let ((process (get-buffer-process shell)))
                 (or (not process)
                     (string-empty-p
                      (string-trim
                       (buffer-substring-no-properties
                        (process-mark process) (point-max))))))))
    (agent-shell-insert :text text :shell-buffer shell :submit t :no-focus t)))

(defun +agent-shell-drive-interrupt (shell)
  "Cancel SHELL's turn without asking, rejecting its pending permissions."
  (when (and (buffer-live-p shell)
             (agent-shell-session-id :shell-buffer shell))
    (with-current-buffer shell (agent-shell-interrupt t))))

(defun +agent-shell-drive--text (chunks)
  "Return the text of CHUNKS, which lists text newest first."
  (apply #'concat (reverse chunks)))

(defun +agent-shell-drive-partial-text (shell)
  "Return the agent text of watched SHELL's current turn."
  (+agent-shell-drive--text
   (plist-get (gethash shell +agent-shell-drive--shells) :turn)))

(defun +agent-shell-drive--forget (shell)
  "Drop SHELL's upstream subscription and turn state."
  (when-let* ((state (gethash shell +agent-shell-drive--shells)))
    (remhash shell +agent-shell-drive--shells)
    (when (buffer-live-p shell)
      (with-current-buffer shell
        (agent-shell-unsubscribe
         :subscription (plist-get state :subscription))))))

(defun +agent-shell-drive--watching (shell)
  "Return the (KEY . ENTRY) watchers of SHELL."
  (let (watchers)
    (maphash (lambda (key entry)
               (when (eq (car entry) shell) (push (cons key entry) watchers)))
             +agent-shell-drive--watchers)
    watchers))

(defun +agent-shell-drive--remove (key)
  "Remove watcher KEY, forgetting its shell once nothing watches it."
  (when-let* ((entry (gethash key +agent-shell-drive--watchers)))
    (remhash key +agent-shell-drive--watchers)
    (remhash key +agent-shell-drive--progress)
    (unless (+agent-shell-drive--watching (car entry))
      (+agent-shell-drive--forget (car entry)))))

(defvar +agent-shell-drive--completion nil
  "Dynamically bound (SHELL . STATE) while emitting a completed turn.")

(defun +agent-shell-drive--finish-output (&rest arguments)
  "Save the finished turn before output callbacks can submit ARGUMENTS.
ARGUMENTS belong to `agent-shell--finish-output'."
  (when (plist-get arguments :success)
    (when-let* ((state (gethash (current-buffer) +agent-shell-drive--shells)))
      (plist-put state :completed (copy-sequence state)))))

(defun +agent-shell-drive--emit-event (emit &rest arguments)
  "Call EMIT with ARGUMENTS, freezing completion before subscribers run."
  (if-let* (((eq (plist-get arguments :event) 'turn-complete))
            (state (gethash (current-buffer) +agent-shell-drive--shells)))
      (let ((+agent-shell-drive--completion
             (cons (current-buffer)
                   (or (plist-get state :completed) (copy-sequence state)))))
        (plist-put state :completed nil)
        (apply emit arguments))
    (apply emit arguments)))

(defun +agent-shell-drive--note-command (state data)
  "Record in turn STATE the command of the tool call event DATA names.
Only execute tool calls count.  Adapters send the command in rawInput as
a string or an argv vector, and may send it only in a later update."
  (when-let* ((call (map-elt data :tool-call))
              ((equal (map-elt call :kind) "execute"))
              (command (map-elt call :command))
              ((stringp command))
              ((not (string-blank-p command))))
    (let ((commands (plist-get state :commands))
          (id (map-elt data :tool-call-id)))
      (setf (alist-get id commands nil nil #'equal) command)
      (plist-put state :commands commands))))

(defun +agent-shell-drive--parse (shell state event)
  "Update SHELL's turn STATE from upstream EVENT; return a plist or nil."
  (let ((kind (map-elt event :event))
        (data (map-elt event :data)))
    (when (and (eq kind 'turn-complete)
               (eq shell (car +agent-shell-drive--completion)))
      (setq state (cdr +agent-shell-drive--completion)))
    (when (memq kind '(tool-call tool-call-update))
      (+agent-shell-drive--note-command state data))
    (pcase kind
      ('agent-message-chunk
       (when-let* ((chunk (map-elt data :text-chunk)))
         (plist-put state :turn (cons chunk (plist-get state :turn)))
         (plist-put state :final (cons chunk (plist-get state :final))))
       nil)
      ('tool-call-update
       (let ((part (+agent-shell-drive--text (plist-get state :final))))
         (plist-put state :final nil)
         (unless (string-blank-p part)
           (list :kind kind :shell shell
                 :session (ignore-errors
                            (agent-shell-session-id :shell-buffer shell))
                 :text part
                 :title (map-nested-elt data '(:tool-call :title))))))
      (_
       (when (eq kind 'input-submitted)
         (plist-put state :turn nil)
         (plist-put state :final nil)
         (plist-put state :provider-errors nil)
         (plist-put state :commands nil))
       (unless (and (eq kind 'init-finished)
                    (+agent-shell-drive--refusal shell))
         (append
          (list :kind kind :shell shell
                :session (ignore-errors
                           (agent-shell-session-id :shell-buffer shell)))
          (pcase kind
            ('turn-complete
             (append
              (list :stop-reason (map-elt data :stop-reason)
                    :turn-text (+agent-shell-drive--text
                                (plist-get state :turn))
                    :text (let ((final (+agent-shell-drive--text
                                        (plist-get state :final))))
                            (if (string-blank-p final)
                                (+agent-shell-drive--text (plist-get state :turn))
                              final))
                    :provider-errors (plist-get state :provider-errors)
                    :commands (seq-uniq (reverse
                                         (mapcar #'cdr (plist-get state
                                                                  :commands)))))
              (when-let* ((plan (buffer-local-value
                                 '+agent-shell-drive--plan shell)))
                (with-current-buffer shell
                  (setq +agent-shell-drive--plan nil))
                (list :plan plan))))
            ('clean-up
             (when-let* ((reason (buffer-local-value
                                  '+agent-shell-drive--reason shell)))
               (list :reason reason)))
            ('error (list :message (map-elt data :message)
                          :data +agent-shell-drive--error-data
                          :provider-errors
                          (plist-get state :provider-errors))))))))))

(defun +agent-shell-drive--progress-event (shell event)
  "Return the `progress' event upstream EVENT from SHELL makes, or nil.
Agent text chunks and tool calls make one even when no agent text came
before them, which the parsed events leave out."
  (let ((data (map-elt event :data)))
    (pcase (map-elt event :event)
      ('agent-message-chunk
       (when-let* ((chunk (map-elt data :text-chunk))
                   ((not (string-empty-p chunk))))
         (list :kind 'progress :shell shell :part 'assistant :text chunk)))
      ((or 'tool-call 'tool-call-update)
       (when-let* ((title (map-nested-elt data '(:tool-call :title))))
         (list :kind 'progress :shell shell :part 'tool :text title
               :status (map-nested-elt data '(:tool-call :status))))))))

(defun +agent-shell-drive--dispatch (shell event)
  "Deliver upstream EVENT from SHELL to its watchers."
  (when-let* ((progress (and (gethash shell +agent-shell-drive--shells)
                             (+agent-shell-drive--progress-event shell event))))
    (dolist (watcher (+agent-shell-drive--watching shell))
      (when (and (gethash (car watcher) +agent-shell-drive--progress)
                 (eq (gethash (car watcher) +agent-shell-drive--watchers)
                     (cdr watcher)))
        (condition-case err (funcall (cddr watcher) progress)
          (error (message "agent-shell-drive: watcher %S errored on \
progress: %S" (car watcher) err))))))
  (when-let* ((state (gethash shell +agent-shell-drive--shells))
              (parsed (+agent-shell-drive--parse shell state event)))
    (unwind-protect (+agent-shell-drive--deliver shell parsed)
      (when (eq (plist-get parsed :kind) 'clean-up)
        (dolist (watcher (+agent-shell-drive--watching shell))
          (remhash (car watcher) +agent-shell-drive--watchers))
        (+agent-shell-drive--forget shell)))))

(defun +agent-shell-drive--deliver (shell parsed)
  "Call each of SHELL's watchers with PARSED, an event plist."
  (dolist (watcher (+agent-shell-drive--watching shell))
    (when (eq (gethash (car watcher) +agent-shell-drive--watchers)
              (cdr watcher))
      (condition-case err (funcall (cddr watcher) parsed)
        (error (message "agent-shell-drive: watcher %S errored on %s: %S"
                        (car watcher) (plist-get parsed :kind) err))))))

(defun +agent-shell-drive--error-handler (make &rest args)
  "Advise MAKE, `agent-shell--make-error-handler', called with ARGS.
The handler it returns binds `+agent-shell-drive--error-data' to the
JSON-RPC error's data while a watched shell handles it, so that shell's
`error' event carries it."
  (let ((handler (apply make args))
        (shell (plist-get args :shell-buffer)))
    (lambda (acp-error raw-message)
      (let ((+agent-shell-drive--error-data
             (and (gethash shell +agent-shell-drive--shells)
                  (map-elt acp-error 'data))))
        (funcall handler acp-error raw-message)))))

(defun +agent-shell-drive--note-notification (&rest args)
  "Record a watched shell's provider error from ARGS, a notification's.
ARGS are those of `agent-shell--on-notification'.  A `session_info_update'
carrying `_meta.codex.error' joins the turn's :provider-errors, and the
shell's watchers get it as a `provider-warning' event."
  (let* ((shell (map-elt (plist-get args :state) :buffer))
         (state (and shell (gethash shell +agent-shell-drive--shells)))
         (update (map-nested-elt (plist-get args :acp-notification)
                                 '(params update))))
    (when-let* ((state)
                ((equal (map-elt update 'sessionUpdate) "session_info_update"))
                (error (map-nested-elt update '(_meta codex error)))
                ((map-elt error 'message)))
      (plist-put state :provider-errors
                 (cons error (plist-get state :provider-errors)))
      (+agent-shell-drive--deliver
       shell (list :kind 'provider-warning :shell shell
                   :session (ignore-errors
                              (agent-shell-session-id :shell-buffer shell))
                   :message (map-elt error 'message)
                   :info (map-elt error 'codexErrorInfo)
                   :will-retry (and (map-elt error 'willRetry) t))))))

(defun +agent-shell-drive--advise ()
  "Advise the agent-shell functions whose events watchers need.
Each advice acts only on shells this library watches."
  (advice-add 'agent-shell--finish-output :before
              #'+agent-shell-drive--finish-output)
  (advice-add 'agent-shell--emit-event :around
              #'+agent-shell-drive--emit-event)
  (advice-add 'agent-shell--make-error-handler :around
              #'+agent-shell-drive--error-handler)
  (advice-add 'agent-shell--on-notification :after
              #'+agent-shell-drive--note-notification))

;; At load, so shells watched before a reload are covered too.
(+agent-shell-drive--advise)

(defun +agent-shell-drive-watch (shell owner id function)
  "Call FUNCTION with SHELL's parsed events under key (OWNER . ID).
Watching the same key on the same shell again does nothing; on another shell
it moves the watcher."
  (let* ((key (cons owner id))
         (entry (gethash key +agent-shell-drive--watchers)))
    (unless (eq (car entry) shell)
      (when entry (+agent-shell-drive--remove key))
      (unless (gethash shell +agent-shell-drive--shells)
        (puthash shell
                 (list :subscription
                       (agent-shell-subscribe-to
                        :shell-buffer shell
                        :on-event (lambda (event)
                                    (+agent-shell-drive--dispatch shell event)))
                       :turn nil :final nil :completed nil :commands nil)
                 +agent-shell-drive--shells))
      (puthash key (cons shell function) +agent-shell-drive--watchers))))

(defun +agent-shell-drive-watch-progress (shell owner id function)
  "Watch SHELL as `+agent-shell-drive-watch' does, adding `progress' events.
The watcher's key is (`progress' OWNER . ID), apart from OWNER's others."
  (let ((key (cons 'progress (cons owner id))))
    (+agent-shell-drive-watch shell 'progress (cons owner id) function)
    (puthash key t +agent-shell-drive--progress)))

(defun +agent-shell-drive-unwatch (owner &optional id)
  "Remove OWNER's watcher ID, or all of OWNER's watchers when ID is nil."
  (dolist (key (hash-table-keys +agent-shell-drive--watchers))
    (when (and (equal (car key) owner) (or (null id) (equal (cdr key) id)))
      (+agent-shell-drive--remove key))))

;;;; Provider failures

(defconst +agent-shell-drive-failure-kinds
  '((rate_limited "was rate limited" t)
    (quota_exhausted "ran out of quota" nil)
    (overloaded "was overloaded" t)
    (transport_lost "lost its connection" t)
    (auth_required "needs to log in again" nil)
    (context_exhausted "ran out of context" nil)
    (budget_exhausted "ran out of budget" nil)
    (policy_denied "was refused by policy" nil)
    (internal_error "had an internal error" t)
    (provider_error "had a provider error" nil))
  "Each provider failure KIND as (KIND PHRASE TRANSIENT).
PHRASE completes a sentence the agent's name starts.  A TRANSIENT kind
may succeed when the turn is tried again.")

(defconst +agent-shell-drive--failure-aliases
  '(("rate_limit" . rate_limited) ("rateLimitExceeded" . rate_limited)
    ("billing_error" . quota_exhausted) ("account_on_hold" . quota_exhausted)
    ("usageLimitExceeded" . quota_exhausted)
    ("serverOverloaded" . overloaded)
    ("httpConnectionFailed" . transport_lost)
    ("responseStreamConnectionFailed" . transport_lost)
    ("responseStreamDisconnected" . transport_lost)
    ("responseTooManyFailedAttempts" . transport_lost)
    ("authentication_failed" . auth_required)
    ("oauth_org_not_allowed" . auth_required)
    ("unauthorized" . auth_required) ("access_denied" . auth_required)
    ("verification_required" . auth_required)
    ("cloud_credential_error" . auth_required)
    ("max_output_tokens" . context_exhausted)
    ("contextWindowExceeded" . context_exhausted)
    ("sessionBudgetExceeded" . budget_exhausted)
    ("cyberPolicy" . policy_denied)
    ("server_error" . internal_error) ("internalServerError" . internal_error)
    ("no_result" . internal_error) ("worker_shutdown" . internal_error))
  "Adapter and API error kinds, by name, and the failure kind each means.
Claude Code's adapter sends the API's kinds and its own as errorKind;
Codex's sends codexErrorInfo.")

(defun +agent-shell-drive-failure-kind (value)
  "Return the failure kind in `+agent-shell-drive-failure-kinds' VALUE means.
VALUE is an errorKind string, or a codexErrorInfo string or object; an
object's single key names it, and HTTP status 429 or 401 inside it means
`rate_limited' or `auth_required'.  Anything unknown is `provider_error'."
  (let* ((name (cond ((stringp value) value)
                     ((symbolp value) (and value (symbol-name value)))
                     ((consp (car-safe value)) (symbol-name (caar value)))))
         (status (and (consp (car-safe value))
                      (map-elt (cdar value) 'httpStatusCode))))
    (cond ((eql status 429) 'rate_limited)
          ((eql status 401) 'auth_required)
          ((and name (assq (intern name) +agent-shell-drive-failure-kinds))
           (intern name))
          ((and name (cdr (assoc name +agent-shell-drive--failure-aliases))))
          (t 'provider_error))))

(defun +agent-shell-drive-failure-transient-p (kind)
  "Return whether failure KIND may succeed when the turn is tried again."
  (nth 2 (assq kind +agent-shell-drive-failure-kinds)))

(defun +agent-shell-drive-failure-phrase (kind)
  "Return how a sentence naming an agent goes on to say it failed as KIND."
  (or (nth 1 (assq kind +agent-shell-drive-failure-kinds)) "failed"))

(defun +agent-shell-drive-failure (event &optional patterns)
  "Return the provider failure that ended EVENT's turn, or nil.
EVENT is a parsed `turn-complete' or `error' event.  The result is
\(:kind KIND :message TEXT :source SOURCE), from the first of:

- `error-data': the JSON-RPC error's data names an errorKind or a
  codexErrorInfo.
- `codex-error': the turn saw a `_meta.codex.error' whose message is
  exactly its final text, once both are trimmed; Codex's adapter writes
  a final error it will not retry as the turn's text.
- `pattern': the whole trimmed final text matches a regexp in PATTERNS,
  a list of (KIND . REGEXP) such as an agent's :failure-patterns in
  `+agent-shell-sandbox-agents'."
  (let* ((data (plist-get event :data))
         (data (and (consp (car-safe data)) data))
         (code (or (map-elt data 'errorKind) (map-elt data 'codexErrorInfo)))
         (text (string-trim (or (plist-get event :text) ""))))
    (cond
     (code
      (list :kind (+agent-shell-drive-failure-kind code)
            :message (or (map-elt data 'message) (plist-get event :message)
                         "")
            :source 'error-data))
     ((string-empty-p text) nil)
     ((when-let* ((error (seq-find
                          (lambda (error)
                            (equal text (string-trim
                                         (or (map-elt error 'message) ""))))
                          (plist-get event :provider-errors))))
        (list :kind (+agent-shell-drive-failure-kind
                     (map-elt error 'codexErrorInfo))
              :message text :source 'codex-error)))
     ((when-let* ((pattern (seq-find
                            (lambda (pattern)
                              (let ((case-fold-search nil))
                                (string-match-p
                               (concat "\\`\\(?:" (cdr pattern) "\\)\\'")
                               text)))
                            patterns)))
        (list :kind (car pattern) :message text :source 'pattern))))))

(provide '+agent-shell-drive)
;;; +agent-shell-drive.el ends here
