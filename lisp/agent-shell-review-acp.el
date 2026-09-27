;;; agent-shell-review-acp.el --- Direct ACP transport for reviews -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") (acp "0"))

;;; Commentary:
;; Own one read-only ACP reviewer session without an agent-shell buffer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'acp)

(defgroup agent-shell-review-acp nil
  "ACP transport for structured reviews."
  :group 'tools)

(defcustom agent-shell-review-acp-command '("codex-acp")
  "Command and arguments used for the reviewer."
  :type '(repeat string))

(defcustom agent-shell-review-acp-environment
  '("OPENAI_API_KEY=" "DEFAULT_AUTH_REQUEST={\"methodId\":\"chat-gpt\"}")
  "Environment entries for the reviewer process."
  :type '(repeat string))

(defcustom agent-shell-review-acp-model-id nil
  "Model ID to select when advertised, or nil for the agent default."
  :type '(choice (const nil) string))

(cl-defstruct agent-shell-review-acp--transport
  root requirements-file on-event client session-id active closed
  diagnostics)

(defun agent-shell-review-acp-create (root requirements-file on-event)
  "Create an inert review transport in ROOT with ON-EVENT callback.
REQUIREMENTS-FILE may name one explicitly allowed external file."
  (make-agent-shell-review-acp--transport
   :root (file-name-as-directory (expand-file-name root))
   :requirements-file (and requirements-file
                           (expand-file-name requirements-file))
   :on-event on-event :diagnostics ""))

(defun agent-shell-review-acp--record (transport label value)
  "Record LABEL and VALUE in TRANSPORT diagnostics."
  (setf (agent-shell-review-acp--transport-diagnostics transport)
        (concat (agent-shell-review-acp--transport-diagnostics transport)
                (format "%s: %s\n" label value))))

(defun agent-shell-review-acp--emit (transport event)
  "Send EVENT while TRANSPORT is active."
  (when (agent-shell-review-acp--transport-active transport)
    (funcall (agent-shell-review-acp--transport-on-event transport) event)))

(defun agent-shell-review-acp--fail (transport reason)
  "Report terminal REASON from TRANSPORT."
  (when (agent-shell-review-acp--transport-active transport)
    (agent-shell-review-acp--record transport "Error" reason)
    (agent-shell-review-acp--emit
     transport (list :type 'error :message (format "%s" reason)))))

(defun agent-shell-review-acp--request (transport request on-success)
  "Send REQUEST on TRANSPORT and invoke ON-SUCCESS while active."
  (agent-shell-review-acp--record
   transport "Request" (map-elt request :method))
  (condition-case err
      (progn
        (acp-send-request
         :client (agent-shell-review-acp--transport-client transport)
         :request request
         :on-success (lambda (response)
                       (when (agent-shell-review-acp--transport-active transport)
                         (funcall on-success response)))
         :on-failure (lambda (failure)
                       (agent-shell-review-acp--fail transport failure)))
        t)
    (error (agent-shell-review-acp--fail transport (error-message-string err))
           nil)))

(defun agent-shell-review-acp--read-only-mode (response)
  "Return an advertised read-only mode ID from session RESPONSE."
  (let ((modes (map-nested-elt response '(modes availableModes))))
    (when-let* ((mode
                 (cl-find-if
                  (lambda (item)
                    (cl-some
                     (lambda (value)
                       (and (stringp value)
                            (string-match-p "read[ -]?only"
                                            (downcase value))))
                     (list (map-elt item 'id) (map-elt item 'name))))
                  (append modes nil))))
      (map-elt mode 'id))))

(defun agent-shell-review-acp--config-option (response category)
  "Return advertised config option in RESPONSE for CATEGORY."
  (cl-find-if
   (lambda (option)
     (or (equal (map-elt option 'category) category)
         (equal (map-elt option 'id) category)))
   (append (map-elt response 'configOptions) nil)))

(defun agent-shell-review-acp--config-value (option pattern)
  "Return OPTION value whose ID or name matches PATTERN."
  (when-let* ((entry
               (cl-find-if
                (lambda (item)
                  (cl-some
                   (lambda (value)
                     (and (stringp value)
                          (string-match-p pattern (downcase value))))
                   (list (map-elt item 'value) (map-elt item 'name))))
                (append (map-elt option 'options) nil))))
    (map-elt entry 'value)))

(defun agent-shell-review-acp--select-mode (transport response)
  "Select advertised read-only mode from session RESPONSE."
  (let* ((option (agent-shell-review-acp--config-option response "mode"))
         (config-mode (and option
                           (agent-shell-review-acp--config-value
                            option "read[ -]?only")))
         (legacy-mode (agent-shell-review-acp--read-only-mode response))
         (session-id (agent-shell-review-acp--transport-session-id transport)))
    (cond
     (config-mode
      (agent-shell-review-acp--request
       transport
       (acp-make-session-set-config-option-request
        :session-id session-id :config-id (map-elt option 'id)
        :value config-mode)
       (lambda (_result)
         (agent-shell-review-acp--emit transport '(:type ready)))))
     (legacy-mode
      (agent-shell-review-acp--request
       transport
       (acp-make-session-set-mode-request
        :session-id session-id :mode-id legacy-mode)
       (lambda (_result)
         (agent-shell-review-acp--emit transport '(:type ready)))))
     (t (agent-shell-review-acp--fail
         transport "Reviewer did not advertise a read-only mode")))))

(defun agent-shell-review-acp--select-model (transport response)
  "Select configured model for TRANSPORT from RESPONSE, then its mode."
  (if (not agent-shell-review-acp-model-id)
      (agent-shell-review-acp--select-mode transport response)
    (let* ((option (agent-shell-review-acp--config-option response "model"))
           (config-model
            (and option
                 (cl-find-if
                  (lambda (item)
                    (equal (map-elt item 'value)
                           agent-shell-review-acp-model-id))
                  (append (map-elt option 'options) nil))))
           (models (map-nested-elt response '(models availableModels)))
           (model (cl-find-if
                   (lambda (item)
                     (equal (map-elt item 'modelId)
                            agent-shell-review-acp-model-id))
                   (append models nil))))
      (cond
       (config-model
        (agent-shell-review-acp--request
         transport
         (acp-make-session-set-config-option-request
          :session-id (agent-shell-review-acp--transport-session-id transport)
          :config-id (map-elt option 'id)
          :value agent-shell-review-acp-model-id)
         (lambda (_result)
           (agent-shell-review-acp--select-mode transport response))))
       (model
          (agent-shell-review-acp--request
           transport
           (acp-make-session-set-model-request
            :session-id (agent-shell-review-acp--transport-session-id transport)
            :model-id agent-shell-review-acp-model-id)
           (lambda (_result)
             (agent-shell-review-acp--select-mode transport response))))
       (t (agent-shell-review-acp--fail
           transport (format "Reviewer did not advertise model %s"
                             agent-shell-review-acp-model-id)))))))

(defun agent-shell-review-acp--notification (transport notification)
  "Handle ACP NOTIFICATION from TRANSPORT."
  (when (and (agent-shell-review-acp--transport-active transport)
             (equal (map-elt notification 'method) "session/update")
             (equal (map-nested-elt notification '(params sessionId))
                    (agent-shell-review-acp--transport-session-id transport)))
    (let ((update (map-nested-elt notification '(params update))))
      (when (equal (map-elt update 'sessionUpdate) "agent_message_chunk")
        (let ((content (map-elt update 'content)))
          (when (and (equal (map-elt content 'type) "text")
                     (stringp (map-elt content 'text)))
            (agent-shell-review-acp--record
             transport "Agent" (map-elt content 'text))
            (agent-shell-review-acp--emit
             transport (list :type 'chunk :text (map-elt content 'text)))))))))

(defun agent-shell-review-acp--allowed-read-p (transport path)
  "Return non-nil if PATH is readable by TRANSPORT's reviewer."
  (and (stringp path)
       (let* ((expanded (expand-file-name path
                                          (agent-shell-review-acp--transport-root
                                           transport)))
              (root (file-truename
                     (agent-shell-review-acp--transport-root transport))))
         (and (file-regular-p expanded)
              (file-readable-p expanded)
              (or (file-in-directory-p (file-truename expanded) root)
                  (equal expanded
                         (agent-shell-review-acp--transport-requirements-file
                          transport)))))))

(defun agent-shell-review-acp--read (path line limit)
  "Read PATH from LINE for LIMIT lines when provided."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))
    (let ((start (point)))
      (if limit
          (forward-line limit)
        (goto-char (point-max)))
      (buffer-substring-no-properties start (point)))))

(defun agent-shell-review-acp--incoming (transport request)
  "Answer an incoming ACP REQUEST without allowing edits."
  (when (agent-shell-review-acp--transport-active transport)
    (let* ((method (map-elt request 'method))
           (id (map-elt request 'id))
           (client (agent-shell-review-acp--transport-client transport))
           (raw-path (map-nested-elt request '(params path)))
           (path (and (stringp raw-path)
                      (expand-file-name
                       raw-path
                       (agent-shell-review-acp--transport-root transport))))
           (response
            (cond
             ((equal method "session/request_permission")
              (acp-make-session-request-permission-response
               :request-id id :cancelled t))
             ((and (equal method "fs/read_text_file")
                   (agent-shell-review-acp--allowed-read-p transport path))
              (condition-case err
                  (acp-make-fs-read-text-file-response
                   :request-id id
                   :content (agent-shell-review-acp--read
                             path (map-nested-elt request '(params line))
                             (map-nested-elt request '(params limit))))
                (error
                 (acp-make-fs-read-text-file-response
                  :request-id id
                  :error (acp-make-error
                          :code -32603
                          :message (error-message-string err))))))
             (t
              (list (cons :request-id id)
                    (cons :error
                          (acp-make-error
                           :code -32601
                           :message (format "Review request denied: %s"
                                            method))))))))
      (agent-shell-review-acp--record transport "Incoming" method)
      (acp-send-response :client client :response response))))

(defun agent-shell-review-acp--watch-process (transport)
  "Report termination of TRANSPORT's process between ACP requests."
  (when-let* ((process (map-elt
                        (agent-shell-review-acp--transport-client transport)
                        :process))
              ((processp process)))
    (let ((original (process-sentinel process)))
      (set-process-sentinel
       process
       (lambda (ended event)
         (when original (funcall original ended event))
         (when (memq (process-status ended) '(exit signal))
           (agent-shell-review-acp--fail
            transport (format "Reviewer process exited: %s"
                              (string-trim event)))))))))

(defun agent-shell-review-acp-start (transport)
  "Start TRANSPORT's ACP client and initialize its review session."
  (unless (or (agent-shell-review-acp--transport-active transport)
              (agent-shell-review-acp--transport-closed transport))
    (let* ((command agent-shell-review-acp-command)
           (client (acp-make-client
                    :command (car command)
                    :command-params (cdr command)
                    :environment-variables agent-shell-review-acp-environment)))
      (setf (agent-shell-review-acp--transport-client transport) client
            (agent-shell-review-acp--transport-active transport) t)
      (acp-subscribe-to-notifications
       :client client
       :on-notification
       (lambda (notification)
         (agent-shell-review-acp--notification transport notification)))
      (acp-subscribe-to-requests
       :client client
       :on-request
       (lambda (request)
         (agent-shell-review-acp--incoming transport request)))
      (acp-subscribe-to-errors
       :client client
       :on-error
       (lambda (error)
         (agent-shell-review-acp--fail transport error)))
      (agent-shell-review-acp--request
       transport
       (acp-make-initialize-request
        :protocol-version 1
        :client-info '((name . "agent-shell-review")
                       (title . "Emacs Agent Review") (version . "1"))
        :read-text-file-capability t
        :write-text-file-capability nil)
       (lambda (_response)
         (agent-shell-review-acp--request
          transport
          (acp-make-session-new-request
           :cwd (agent-shell-review-acp--transport-root transport))
          (lambda (response)
            (if-let* ((session-id (map-elt response 'sessionId)))
                (progn
                  (setf (agent-shell-review-acp--transport-session-id transport)
                        session-id)
                  (agent-shell-review-acp--select-model transport response))
              (agent-shell-review-acp--fail
               transport "Reviewer did not return a session ID"))))))
      (agent-shell-review-acp--watch-process transport)))
  transport)

(defun agent-shell-review-acp-send (transport prompt)
  "Send text PROMPT to TRANSPORT's initialized session."
  (unless (and (agent-shell-review-acp--transport-active transport)
               (agent-shell-review-acp--transport-session-id transport))
    (user-error "Reviewer session is unavailable"))
  (agent-shell-review-acp--request
   transport
   (acp-make-session-prompt-request
    :session-id (agent-shell-review-acp--transport-session-id transport)
    :prompt (list `((type . "text") (text . ,prompt))))
   (lambda (response)
     (agent-shell-review-acp--emit
      transport (list :type 'complete
                      :stop-reason (map-elt response 'stopReason))))))

(defun agent-shell-review-acp-close (transport)
  "Shut down TRANSPORT once, preserving its diagnostics."
  (unless (agent-shell-review-acp--transport-closed transport)
    (setf (agent-shell-review-acp--transport-active transport) nil
          (agent-shell-review-acp--transport-closed transport) t)
    (when-let* ((client (agent-shell-review-acp--transport-client transport)))
      (acp-shutdown :client client))))

(defun agent-shell-review-acp-diagnostics (transport)
  "Return retained traffic and errors for TRANSPORT."
  (agent-shell-review-acp--transport-diagnostics transport))

(provide 'agent-shell-review-acp)
;;; agent-shell-review-acp.el ends here
