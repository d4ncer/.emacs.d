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

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-session-id "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(defvar agent-shell-agent-configs)
(defvar agent-shell-session-strategy)
(defvar agent-shell-cwd-function)
(defvar agent-shell-mode-hook)

(defvar +agent-shell-drive--watchers (make-hash-table :test #'equal)
  "Map (OWNER . ID) to (SHELL . FUNCTION).")
(defvar +agent-shell-drive--shells (make-hash-table :test #'eq)
  "Map a watched shell to (:subscription TOKEN :turn TEXT :final TEXT).")

(defun +agent-shell-drive-config (spec)
  "Return the agent config for SPEC, an identifier symbol or config alist."
  (require 'agent-shell)
  (if (and (listp spec) (assq :identifier spec)) spec
    (or (seq-find
         (lambda (config) (eq (map-elt config :identifier) spec))
         (mapcar (lambda (entry) (if (functionp entry) (funcall entry) entry))
                 (if (functionp agent-shell-agent-configs)
                     (funcall agent-shell-agent-configs)
                   agent-shell-agent-configs)))
        (user-error "Unknown agent configuration %s" spec))))

(cl-defun +agent-shell-drive-start (&key config directory)
  "Start a new shell from CONFIG pinned to DIRECTORY, without displaying it."
  (require 'agent-shell)
  (let* ((directory (file-name-as-directory (file-truename directory)))
         (default-directory directory)
         (cwd-function (lambda () directory))
         (agent-shell-cwd-function cwd-function)
         (agent-shell-mode-hook
          (append agent-shell-mode-hook
                  (list (lambda ()
                          (setq-local agent-shell-cwd-function cwd-function
                                      default-directory directory)))))
         (agent-shell-session-strategy 'new)
         (shell (save-window-excursion (agent-shell-start :config config))))
    (with-current-buffer shell
      (setq-local agent-shell-cwd-function cwd-function
                  default-directory directory))
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
       (append
        (list :kind kind :shell shell
              :session (ignore-errors
                         (agent-shell-session-id :shell-buffer shell)))
        (pcase kind
          ('turn-complete
           (list :stop-reason (map-elt data :stop-reason)
                 :text (let ((final (plist-get state :final)))
                         (if (string-blank-p final) (plist-get state :turn)
                           final))))
          ('error (list :message (map-elt data :message)))))))))

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
