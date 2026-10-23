;;; +agent-shell-drive.el --- Drive agent shells from Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Start agent shells pinned to a directory, submit prompts without clobbering
;; user drafts, and watch their events as parsed plists.  Watchers are keyed by
;; (OWNER . ID); the library owns one upstream subscription per shell.
;;
;; A watcher receives (:kind KIND :shell BUFFER :session ID ...), where KIND
;; is the upstream event symbol.  `turn-complete' adds :stop-reason and :text,
;; `error' adds :message.  `agent-message-chunk' and `tool-call-update' only
;; feed the turn text: chunks append, a tool call ends the final part, and
;; `input-submitted' starts a new turn.
;;
;; A read-only shell runs in its agent's mode from
;; `+agent-shell-drive-read-only-modes' and answers permission requests itself:
;; it approves reads once and rejects everything else.  Its `turn-complete'
;; adds :plan when it rejected a request carrying a plan.  If the session is
;; not in that mode once initialized, the shell is killed and its watchers get
;; `clean-up' with :reason instead of `init-finished'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell--current-mode-id "agent-shell-config")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-session-id "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(defvar agent-shell-agent-configs)
(defvar agent-shell-session-strategy)
(defvar agent-shell-cwd-function)
(defvar agent-shell-mode-hook)
(defvar agent-shell-permission-responder-function)
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
  "Map a watched shell to (:subscription TOKEN :turn TEXT :final TEXT).")

(defvar-local +agent-shell-drive--mode nil
  "Read-only session mode this shell must run in.")
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

(defun +agent-shell-drive--respond (permission)
  "Approve PERMISSION once if it only reads, otherwise reject it.
Return nil, leaving the dialog to the user, only when no option rejects."
  (let* ((tool-call (map-elt permission :tool-call))
         (options (map-elt permission :options))
         (pick (lambda (&rest kinds)
                 (seq-some (lambda (kind)
                             (seq-find (lambda (option)
                                         (equal (map-elt option :kind) kind))
                                       options))
                           kinds)))
         (choice (if (member (map-elt tool-call :kind)
                             +agent-shell-drive--read-kinds)
                     (funcall pick "allow_once" "reject_once" "reject_always")
                   (funcall pick "reject_once" "reject_always"))))
    (when choice
      (when-let* (((string-prefix-p "reject" (map-elt choice :kind)))
                  (plan (map-elt (map-elt tool-call :raw-input) 'plan)))
        (setq +agent-shell-drive--plan plan))
      (funcall (map-elt permission :respond) (map-elt choice :option-id))
      t)))

(defun +agent-shell-drive--refusal (shell)
  "Return why read-only SHELL is not in its read-only mode, or nil."
  (require 'agent-shell-config)
  (with-current-buffer shell
    (when-let* ((mode +agent-shell-drive--mode))
      (let ((actual (agent-shell--current-mode-id agent-shell--state)))
        (unless (equal actual mode)
          (format "Session mode is %s, not the read-only mode %s"
                  (or actual "unset") mode))))))

(defun +agent-shell-drive-kill (shell &optional reason)
  "Kill SHELL without prompting; its watchers get `clean-up' with REASON."
  (when (buffer-live-p shell)
    (with-current-buffer shell (setq +agent-shell-drive--reason reason))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer shell))))

(cl-defun +agent-shell-drive-start (&key config directory read-only)
  "Start a new shell from CONFIG pinned to DIRECTORY, without displaying it.
With READ-ONLY, start it in its agent's read-only mode with a responder that
rejects everything but reads; see `+agent-shell-drive-read-only-modes'."
  (require 'agent-shell)
  (let* ((config (+agent-shell-drive-config config))
         (mode (and read-only
                    (or (alist-get (map-elt config :identifier)
                                   +agent-shell-drive-read-only-modes)
                        (user-error "No read-only mode for agent %s"
                                    (map-elt config :identifier)))))
         (config (if mode
                     (cons (cons :default-session-mode-id (lambda () mode))
                           (assq-delete-all :default-session-mode-id
                                            (copy-alist config)))
                   config))
         (directory (file-name-as-directory (file-truename directory)))
         (cwd-function (lambda () directory))
         (pin (lambda ()
                (setq-local agent-shell-cwd-function cwd-function
                            default-directory directory)
                (when mode
                  (setq-local +agent-shell-drive--mode mode
                              agent-shell-permission-responder-function
                              #'+agent-shell-drive--respond))))
         (shell
          ;; A caller's buffer-local values would shadow these bindings.
          (with-temp-buffer
            (let ((default-directory directory)
                  (agent-shell-cwd-function cwd-function)
                  (agent-shell-mode-hook
                   (append agent-shell-mode-hook (list pin)))
                  (agent-shell-session-strategy 'new))
              (save-window-excursion (agent-shell-start :config config))))))
    (with-current-buffer shell (funcall pin))
    (when mode
      (agent-shell-subscribe-to
       :shell-buffer shell :event 'init-finished
       :on-event (lambda (_event)
                   (when-let* ((reason (+agent-shell-drive--refusal shell)))
                     (run-at-time 0 nil #'+agent-shell-drive-kill
                                  shell reason)))))
    shell))

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

(defun +agent-shell-drive-partial-text (shell)
  "Return the agent text of watched SHELL's current turn."
  (plist-get (gethash shell +agent-shell-drive--shells) :turn))

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
         (plist-put state :turn (concat (plist-get state :turn) chunk))
         (plist-put state :final (concat (plist-get state :final) chunk)))
       nil)
      ('tool-call-update (plist-put state :final "") nil)
      (_
       (when (eq kind 'input-submitted)
         (plist-put state :turn "")
         (plist-put state :final ""))
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
                    :text (let ((final (plist-get state :final)))
                            (if (string-blank-p final) (plist-get state :turn)
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
                       :turn "" :final "")
                 +agent-shell-drive--shells))
      (puthash key (cons shell function) +agent-shell-drive--watchers))))

(defun +agent-shell-drive-unwatch (owner &optional id)
  "Remove OWNER's watcher ID, or all of OWNER's watchers when ID is nil."
  (dolist (key (hash-table-keys +agent-shell-drive--watchers))
    (when (and (equal (car key) owner) (or (null id) (equal (cdr key) id)))
      (+agent-shell-drive--remove key))))

(provide '+agent-shell-drive)
;;; +agent-shell-drive.el ends here
