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
  (if (eq (map-elt event :event) 'clean-up)
      (agent-shell-notify--detach shell-buffer)
    (when-let* ((alert (agent-shell-notify--classify event)))
      (agent-shell-notify-send
       (buffer-name shell-buffer) (plist-get alert :body)
       (list shell-buffer)))))

(defun agent-shell-notify--detach (shell-buffer)
  "Remove the event subscription from SHELL-BUFFER."
  (when-let* ((token (gethash shell-buffer agent-shell-notify--subscriptions)))
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
