;;; agent-shell-notify.el --- System alerts for agent-shell -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") (agent-shell "0"))

;;; Commentary:
;; Announce agent-shell turns that need attention.  Delivery can be replaced
;; by setting `agent-shell-notify-send-function'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'agent-shell)

(defgroup agent-shell-notify nil
  "System alerts for agent-shell."
  :group 'agent-shell)

(defvar agent-shell-notify--subscriptions (make-hash-table :test #'eq)
  "Subscription token for each observed shell buffer.")

(defvar agent-shell-notify--states (make-hash-table :test #'eq)
  "Alert state for each shell buffer.")

(defcustom agent-shell-notify-send-function #'agent-shell-notify--macos-send
  "Function called with an alert title and body."
  :type 'function)

(defconst agent-shell-notify--script
  "on run argv\n  display notification (item 2 of argv) with title (item 1 of argv)\nend run"
  "Fixed AppleScript for sending a notification.")

(defun agent-shell-notify--macos-send (title body)
  "Send TITLE and BODY via macOS without blocking Emacs."
  (if-let* ((program (executable-find "osascript")))
      (condition-case err
          (make-process
           :name "agent-shell-notify" :buffer nil :noquery t
           :command (list program "-e" agent-shell-notify--script
                          "--" title body)
           :sentinel (lambda (process _event)
                       (when (and (eq (process-status process) 'exit)
                                  (not (zerop (process-exit-status process))))
                         (message "agent-shell-notify: osascript exited %d"
                                  (process-exit-status process)))))
        (error (message "agent-shell-notify: %s" (error-message-string err))))
    (message "agent-shell-notify: osascript is unavailable")))

(defun agent-shell-notify-send (title body &optional relevant-buffers)
  "Send TITLE and BODY unless a RELEVANT-BUFFERS buffer is focused."
  (unless (cl-some
           (lambda (frame)
             (and (frame-focus-state frame)
                  (memq (window-buffer (frame-selected-window frame))
                        relevant-buffers)))
           (frame-list))
    (funcall agent-shell-notify-send-function title body)))

(defun agent-shell-notify--title (shell-buffer)
  "Return a project and session title for SHELL-BUFFER."
  (with-current-buffer shell-buffer
    (format "%s · %s"
            (file-name-nondirectory
             (directory-file-name (agent-shell-cwd)))
            (buffer-name shell-buffer))))

(defun agent-shell-notify--cancel-pending (shell-buffer)
  "Cancel a pending ready alert for SHELL-BUFFER."
  (when-let* ((timer (plist-get (gethash shell-buffer agent-shell-notify--states)
                                :timer)))
    (cancel-timer timer)))

(defun agent-shell-notify--classify (event)
  "Return alert data for agent-shell EVENT, or nil."
  (let ((name (map-elt event :event))
        (data (map-elt event :data)))
    (pcase name
      ('permission-request
       (list :kind 'permission :body "Approval needed"))
      ('turn-complete
       (let ((reason (map-elt data :stop-reason)))
         (if (equal reason "end_turn")
             (list :kind 'ready :body "Ready for input")
           (list :kind 'stopped
                 :body (format "Stopped: %s" (or reason "unknown reason"))))))
      ('error
       (list :kind 'error
             :body (format "Failed: %s"
                           (or (map-elt data :message) "inspect the agent")))))))

(defun agent-shell-notify--handle (shell-buffer event)
  "Process EVENT from SHELL-BUFFER."
  (let ((event-name (map-elt event :event)))
    (cond
     ((eq event-name 'clean-up)
      (agent-shell-notify--detach shell-buffer))
     ((eq event-name 'input-submitted)
      (agent-shell-notify--cancel-pending shell-buffer)
      (remhash shell-buffer agent-shell-notify--states))
     ((eq event-name 'permission-response)
      (let ((state (gethash shell-buffer agent-shell-notify--states)))
        (when (and (eq (plist-get state :kind) 'permission)
                   (equal (plist-get state :request-id)
                          (map-elt (map-elt event :data) :request-id)))
          (remhash shell-buffer agent-shell-notify--states))))
     (t
      (when-let* ((alert (agent-shell-notify--classify event))
                  (kind (plist-get alert :kind)))
        (let ((old (gethash shell-buffer agent-shell-notify--states)))
        (unless (and (eq kind (plist-get old :kind))
                     (or (not (eq kind 'permission))
                         (equal (plist-get old :request-id)
                                (map-elt (map-elt event :data) :request-id))))
            (agent-shell-notify--cancel-pending shell-buffer)
            (let* ((generation (1+ (or (plist-get old :generation) 0)))
                 (state (list :kind kind :generation generation
                              :request-id (and (eq kind 'permission)
                                               (map-elt (map-elt event :data)
                                                        :request-id))))
                   (send (lambda ()
                           (agent-shell-notify-send
                            (agent-shell-notify--title shell-buffer)
                            (plist-get alert :body)
                            (list shell-buffer)))))
              (puthash shell-buffer state agent-shell-notify--states)
              (if (eq kind 'ready)
                  (plist-put
                   state :timer
                   (run-at-time
                    0.25 nil
                    (lambda ()
                      (when (and (buffer-live-p shell-buffer)
                                 (eq kind
                                     (plist-get
                                      (gethash shell-buffer
                                               agent-shell-notify--states)
                                      :kind))
                                 (eql generation
                                      (plist-get
                                       (gethash shell-buffer
                                                agent-shell-notify--states)
                                       :generation)))
                        (funcall send)))))
                (funcall send))))))))))

(defun agent-shell-notify--detach (shell-buffer)
  "Remove the event subscription from SHELL-BUFFER."
  (when-let* ((token (gethash shell-buffer agent-shell-notify--subscriptions)))
    (agent-shell-notify--cancel-pending shell-buffer)
    (remhash shell-buffer agent-shell-notify--states)
    (remhash shell-buffer agent-shell-notify--subscriptions)
    (when (buffer-live-p shell-buffer)
      (with-current-buffer shell-buffer
        (remove-hook 'kill-buffer-hook #'agent-shell-notify--on-kill t)
        (agent-shell-unsubscribe :subscription token)))))

(defun agent-shell-notify--on-kill ()
  "Remove the subscription for the shell being killed."
  (agent-shell-notify--detach (current-buffer)))

(defun agent-shell-notify--attach (&optional shell-buffer)
  "Subscribe to SHELL-BUFFER once, defaulting to the current buffer."
  (let ((shell (or shell-buffer (current-buffer))))
    (when (and (buffer-live-p shell)
               (not (gethash shell agent-shell-notify--subscriptions)))
      (puthash shell
               (agent-shell-subscribe-to
                :shell-buffer shell
                :on-event (lambda (event)
                            (agent-shell-notify--handle shell event)))
               agent-shell-notify--subscriptions)
      (with-current-buffer shell
        (add-hook 'kill-buffer-hook #'agent-shell-notify--on-kill nil t)))))

;;;###autoload
(define-minor-mode agent-shell-notify-mode
  "Notify when agent-shell agents finish or need attention."
  :global t
  :group 'agent-shell-notify
  (if agent-shell-notify-mode
      (progn
        (add-hook 'agent-shell-mode-hook #'agent-shell-notify--attach)
        (mapc #'agent-shell-notify--attach (agent-shell-buffers)))
    (remove-hook 'agent-shell-mode-hook #'agent-shell-notify--attach)
    (let (buffers)
      (maphash (lambda (buffer _token) (push buffer buffers))
               agent-shell-notify--subscriptions)
      (mapc #'agent-shell-notify--detach buffers))))

(provide 'agent-shell-notify)
;;; agent-shell-notify.el ends here
