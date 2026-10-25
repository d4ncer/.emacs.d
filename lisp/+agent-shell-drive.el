;;; +agent-shell-drive.el --- Drive agent shells from Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Start agent shells pinned to a directory, submit prompts without clobbering
;; user drafts, and watch their events as parsed plists.  Watchers are keyed by
;; (OWNER . ID); the library owns one upstream subscription per shell.
;;
;; A watcher receives (:kind KIND :shell BUFFER :session ID ...), where KIND
;; is the upstream event symbol.  `turn-complete' adds :stop-reason and :text,
;; `error' adds :message.  `agent-message-chunk' only feeds the turn text, and
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
;; sandbox-exec in the agent's full-access mode, rejects every permission
;; request and keeps no transcript.  Stop it with `+agent-shell-drive-stop',
;; which also ends every process it started.

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
CHUNKS lists text newest first; `+agent-shell-drive--text' joins them.")

(defvar-local +agent-shell-drive--policy nil
  "Policy this shell was started with; see `+agent-shell-drive-start'.")
(defvar-local +agent-shell-drive--mode nil
  "Session mode this shell's policy requires.")
(defvar-local +agent-shell-drive--plan nil
  "Plan from the last request this shell rejected during the turn.")
(defvar-local +agent-shell-drive--reason nil
  "Why Lisp killed this shell.")

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

(defun +agent-shell-drive--under-p (path directories)
  "Return non-nil if PATH is one of DIRECTORIES or inside one."
  (seq-some (lambda (directory)
              (or (equal path directory)
                  (string-prefix-p (file-name-as-directory directory) path)))
            directories))

(defun +agent-shell-drive--file-allowed-p (policy write path)
  "Return non-nil if POLICY lets the agent read, or with WRITE write, PATH.
agent-shell serves these requests itself, outside any sandbox-exec profile
and before any permission prompt, so the policy is enforced here too."
  (let ((path (+agent-shell-sandbox--path path)))
    (pcase policy
      ('nil t)
      ('read-only (not write))
      ((pred +agent-shell-sandbox-p)
       (if write
           (and (+agent-shell-drive--under-p
                 path (list (+agent-shell-sandbox-root policy)
                            (+agent-shell-sandbox-tmp policy)
                            (+agent-shell-sandbox-config policy)))
                (not (equal path (concat (+agent-shell-sandbox-root policy)
                                         "/.git"))))
         (not (+agent-shell-drive--under-p
               path (+agent-shell-sandbox-deny-reads policy)))))
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

(cl-defun +agent-shell-drive-start (&key config directory policy)
  "Start a new shell from CONFIG pinned to DIRECTORY, without displaying it.
POLICY nil starts an ordinary shell.  With `read-only', the shell runs in its
agent's mode from `+agent-shell-drive-read-only-modes' with a responder that
rejects everything but reads.  With a `+agent-shell-sandbox', its adapter
runs confined to the sandbox in the agent's full-access mode from
`+agent-shell-sandbox-agents', every permission request is rejected, and no
transcript is kept.  A shell with a policy is hidden from
`agent-shell-buffers'."
  (require 'agent-shell)
  (let* ((config (+agent-shell-drive-config config))
         (agent (map-elt config :identifier))
         (mode (pcase policy
                 ('nil nil)
                 ('read-only
                  (or (alist-get agent +agent-shell-drive-read-only-modes)
                      (user-error "No read-only mode for agent %s" agent)))
                 ((pred +agent-shell-sandbox-p)
                  (+agent-shell-sandbox-mode agent))
                 (_ (error "Unknown shell policy %S" policy))))
         (sandboxed (+agent-shell-sandbox-p policy))
         (config (if mode
                     (+agent-shell-drive--override
                      config :default-session-mode-id (lambda () mode))
                   config))
         (config (if sandboxed
                     (+agent-shell-drive--override
                      config :client-maker
                      (+agent-shell-sandbox-client-maker
                       policy agent (map-elt config :client-maker)))
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
                              (if sandboxed #'+agent-shell-drive--reject
                                #'+agent-shell-drive--respond)))
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

(defun +agent-shell-drive--adapter (shell)
  "Return the pid of live SHELL's agent adapter, or nil."
  (when-let* (((buffer-live-p shell))
              (process (map-nested-elt
                        (buffer-local-value 'agent-shell--state shell)
                        '(:client :process)))
              ((process-live-p process)))
    (process-id process)))

(defun +agent-shell-drive-stop (shell &optional reason callback)
  "Stop sandboxed SHELL and every process it started, then call CALLBACK.
Kill the adapter's descendants, kill SHELL as `+agent-shell-drive-kill'
does with REASON, sweep processes working in the sandbox, and finish its
agent's credentials.  CALLBACK gets the pids that survived the sweep.  The
work runs from a timer, so SHELL's own event callbacks may call this."
  (let ((sandbox (+agent-shell-drive-policy shell)))
    (unless (+agent-shell-sandbox-p sandbox)
      (error "Shell %s is not sandboxed" shell))
    (run-at-time
     0 nil
     (lambda ()
       (when-let* ((pid (+agent-shell-drive--adapter shell)))
         (+agent-shell-sandbox-kill-tree pid))
       (+agent-shell-drive-kill shell reason)
       (+agent-shell-sandbox-sweep
        sandbox (lambda (survivors)
                  (unwind-protect (+agent-shell-sandbox-finish sandbox)
                    (when callback (funcall callback survivors)))))))))

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
    (unless (+agent-shell-drive--watching (car entry))
      (+agent-shell-drive--forget (car entry)))))

(defun +agent-shell-drive--parse (shell state event)
  "Update SHELL's turn STATE from upstream EVENT; return a plist or nil."
  (let ((kind (map-elt event :event))
        (data (map-elt event :data)))
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
         (plist-put state :final nil))
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
                    :text (let ((final (+agent-shell-drive--text
                                        (plist-get state :final))))
                            (if (string-blank-p final)
                                (+agent-shell-drive--text (plist-get state :turn))
                              final)))
              (when-let* ((plan (buffer-local-value
                                 '+agent-shell-drive--plan shell)))
                (with-current-buffer shell
                  (setq +agent-shell-drive--plan nil))
                (list :plan plan))))
            ('clean-up
             (when-let* ((reason (buffer-local-value
                                  '+agent-shell-drive--reason shell)))
               (list :reason reason)))
            ('error (list :message (map-elt data :message))))))))))

(defun +agent-shell-drive--dispatch (shell event)
  "Deliver upstream EVENT from SHELL to its watchers."
  (when-let* ((state (gethash shell +agent-shell-drive--shells))
              (parsed (+agent-shell-drive--parse shell state event)))
    (unwind-protect
        (dolist (watcher (+agent-shell-drive--watching shell))
          (when (eq (gethash (car watcher) +agent-shell-drive--watchers)
                    (cdr watcher))
            (condition-case err (funcall (cddr watcher) parsed)
              (error (message "agent-shell-drive: watcher %S errored on %s: %S"
                              (car watcher) (plist-get parsed :kind) err)))))
      (when (eq (plist-get parsed :kind) 'clean-up)
        (dolist (watcher (+agent-shell-drive--watching shell))
          (remhash (car watcher) +agent-shell-drive--watchers))
        (+agent-shell-drive--forget shell)))))

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
                       :turn nil :final nil)
                 +agent-shell-drive--shells))
      (puthash key (cons shell function) +agent-shell-drive--watchers))))

(defun +agent-shell-drive-unwatch (owner &optional id)
  "Remove OWNER's watcher ID, or all of OWNER's watchers when ID is nil."
  (dolist (key (hash-table-keys +agent-shell-drive--watchers))
    (when (and (equal (car key) owner) (or (null id) (equal (cdr key) id)))
      (+agent-shell-drive--remove key))))

(provide '+agent-shell-drive)
;;; +agent-shell-drive.el ends here
