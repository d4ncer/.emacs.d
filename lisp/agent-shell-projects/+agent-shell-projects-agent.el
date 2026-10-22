;;; +agent-shell-projects-agent.el --- Agent authoring bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Agents write structured JSON operations to the local inbox.  Feedback is
;; queued until a shell can accept it; only accepted batches acknowledge review.

;;; Code:

(require '+agent-shell-projects-core)
(require '+agent-shell-projects-annotations)
(require 'map)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-session-id "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-shell-buffer "agent-shell")
(declare-function agent-shell-markdown-reconstruct "agent-shell-markdown")
(declare-function +agent-shell-projects--choose-project "+agent-shell-projects")
(declare-function +agent-shell-projects-enrol "+agent-shell-projects-core")
(declare-function org-map-entries "org")
(declare-function org-get-heading "org")
(declare-function +agent-shell-projects--project "+agent-shell-projects")
(declare-function +agent-shell-projects--refresh-ui "+agent-shell-projects")
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-session-strategy)
(defvar +agent-shell-projects-sidebar-functions)
(defvar +agent-shell-projects-change-hook)
(defvar-local +agent-shell-projects--project-id nil)
(defvar-local +agent-shell-projects--artefact-id nil)
(defvar-local +agent-shell-projects--version-id nil)
(defvar-local +agent-shell-projects--observed-event 0)
(defvar-local +agent-shell-projects--side-question nil)

(defcustom +agent-shell-projects-agent-policy 'default
  "Whether fresh question shells use a default agent or prompt."
  :type '(choice (const default) (const ask)) :group '+agent-shell-projects)
(defcustom +agent-shell-projects-agent-config nil
  "Default configuration or identifier, inheriting agent-shell when nil."
  :type 'sexp :group '+agent-shell-projects)
(defcustom +agent-shell-projects-context-function
  #'+agent-shell-projects-default-context
  "Function called with PROJECT and a structured REQUEST to build context."
  :type 'function :group '+agent-shell-projects)
(defcustom +agent-shell-projects-send-function #'+agent-shell-projects--send
  "Function of SHELL and TEXT, returning non-nil only after acceptance."
  :type 'function :group '+agent-shell-projects)
(defcustom +agent-shell-projects-inbox-directory
  (expand-file-name (format "agent-shell-projects-%s/inbox/" (user-uid))
                    temporary-file-directory)
  "Writable agent operation inbox, independent of persistent project storage."
  :type 'directory :group '+agent-shell-projects)

(defvar +agent-shell-projects--subscriptions (make-hash-table :test #'equal))
(defvar +agent-shell-projects--shells (make-hash-table :test #'equal))
(defvar +agent-shell-projects--timer nil)
(defvar +agent-shell-projects-operation-handlers nil)
(defvar +agent-shell-projects-request-ready-hook nil)
(defvar +agent-shell-projects--polling nil)

(defun +agent-shell-projects--inbox ()
  "Return the local agent inbox directory."
  (expand-file-name +agent-shell-projects-inbox-directory))

(defun +agent-shell-projects-agent (project id)
  "Return association ID from PROJECT."
  (or (seq-find (lambda (item) (equal (plist-get item :id) id))
                (plist-get project :agents))
      (error "Agent is not associated with this project")))

(defun +agent-shell-projects-shell (agent)
  "Return AGENT's live shell, validating persisted session identity."
  (or (let ((buffer (gethash (plist-get agent :id) +agent-shell-projects--shells)))
        (and (buffer-live-p buffer) buffer))
      (when-let* ((buffer (get-buffer (plist-get agent :buffer)))
                  ((fboundp 'agent-shell-session-id))
                  (session (ignore-errors (agent-shell-session-id
                                           :shell-buffer buffer)))
                  ((equal session (plist-get agent :session))))
        (puthash (plist-get agent :id) buffer +agent-shell-projects--shells)
        buffer)))

(defun +agent-shell-projects-associate (project shell &optional role)
  "Associate SHELL with PROJECT as ROLE, defaulting to author."
  (require 'agent-shell)
  (unless (and (buffer-live-p shell)
               (with-current-buffer shell (derived-mode-p 'agent-shell-mode)))
    (user-error "Choose an agent-shell buffer"))
  (let* ((role (or role "author"))
         (agent (or (seq-find
                     (lambda (item)
                       (eq shell (+agent-shell-projects-shell item)))
                     (plist-get project :agents))
                    (let ((item (list :id (+agent-shell-projects--id)
                                      :buffer (buffer-name shell) :role role
                                      :session nil :progress nil)))
                      (+agent-shell-projects--set
                       project :agents (append (plist-get project :agents)
                                               (list item)))
                      item))))
    (+agent-shell-projects--set agent :buffer (buffer-name shell))
    (+agent-shell-projects--set agent :role role)
    (+agent-shell-projects--set
     agent :session (agent-shell-session-id :shell-buffer shell))
    (puthash (plist-get agent :id) shell +agent-shell-projects--shells)
    (when (equal role "author")
      (+agent-shell-projects--set project :author (plist-get agent :id)))
    (+agent-shell-projects--attach project agent shell)
    (+agent-shell-projects--changed project nil "user")
    agent))

(defun +agent-shell-projects--attach (project agent shell)
  "Subscribe once to SHELL for PROJECT's AGENT association."
  (let ((key (plist-get agent :id)))
    (unless (gethash key +agent-shell-projects--subscriptions)
      (puthash
       key (cons shell
                 (agent-shell-subscribe-to
                  :shell-buffer shell
                  :on-event (lambda (event)
                              (+agent-shell-projects--agent-event
                               (plist-get project :id) key event))))
       +agent-shell-projects--subscriptions))))

(defun +agent-shell-projects--detach (id)
  "Remove the subscription for association ID."
  (when-let* ((entry (gethash id +agent-shell-projects--subscriptions)))
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (agent-shell-unsubscribe :subscription (cdr entry))))
    (remhash id +agent-shell-projects--subscriptions))
  (remhash id +agent-shell-projects--shells))

(defvar +agent-shell-projects--event-timers (make-hash-table :test #'equal))

(defun +agent-shell-projects--agent-event (project-id agent-id event)
  "Handle meaningful agent EVENTs with bounded work and coalesced capture."
  (let ((kind (map-elt event :event)))
    (unless (memq kind '(agent-message-chunk tool-call-update))
      (let* ((project (+agent-shell-projects-load project-id))
             (agent (+agent-shell-projects-agent project agent-id)))
        (when (eq kind 'clean-up) (+agent-shell-projects--detach agent-id))
        (when (memq kind '(init-finished turn-complete session-restored))
          (when-let* ((shell (+agent-shell-projects-shell agent)))
            (let ((session (agent-shell-session-id :shell-buffer shell)))
              (unless (equal session (plist-get agent :session))
                (+agent-shell-projects--set agent :session session)
                (+agent-shell-projects-save project))))
          (when (equal (plist-get agent :role) "author")
            (unless (gethash project-id +agent-shell-projects--event-timers)
              (puthash project-id
                       (+agent-shell-projects-run-at-time
                        0.3 nil
                        (lambda ()
                          (remhash project-id +agent-shell-projects--event-timers)
                          (dolist (artefact (plist-get project :artefacts))
                            (condition-case err
                                (+agent-shell-projects-capture project artefact agent-id)
                              (error (+agent-shell-projects--set artefact :capture-error
                                                                 (error-message-string err)))))))
                       +agent-shell-projects--event-timers))))
        (+agent-shell-projects--refresh-ui project nil t)
        (when (memq kind '(turn-complete prompt-ready init-finished))
          (+agent-shell-projects-run-at-time 0 nil #'+agent-shell-projects-flush project))))))

(defun +agent-shell-projects--select-config ()
  "Resolve the configurable default agent, or prompt when requested."
  (require 'agent-shell)
  (let* ((preferred (or +agent-shell-projects-agent-config
                        agent-shell-preferred-agent-config))
         (prompt (or (eq +agent-shell-projects-agent-policy 'ask)
                     (null preferred)
                     (eq (car-safe preferred) 'preselect)))
         (identifier (if (eq (car-safe preferred) 'auto)
                         (cdr preferred) preferred)))
    (if prompt (agent-shell-select-config :prompt "Project agent: ")
      (if (and (listp identifier) (assq :identifier identifier)) identifier
        (or (seq-find
             (lambda (config) (eq (map-elt config :identifier) identifier))
             (mapcar (lambda (entry) (if (functionp entry) (funcall entry) entry))
                     (if (functionp agent-shell-agent-configs)
                         (funcall agent-shell-agent-configs)
                       agent-shell-agent-configs)))
            (user-error "Unknown agent configuration %s" identifier))))))

(defun +agent-shell-projects-start-agent (project &optional role)
  "Start a fresh shell for PROJECT with ROLE using configured selection."
  (let* ((default-directory (file-name-as-directory
                             (or (plist-get project :root) default-directory)))
         (agent-shell-session-strategy 'new)
         (config (+agent-shell-projects--select-config))
         (shell (agent-shell-start :config config)))
    (+agent-shell-projects-associate project shell role)
    shell))

(defun +agent-shell-projects-choose-author (&optional new)
  "Choose an existing authoring shell; with NEW, create a fresh one."
  (interactive "P")
  (require 'agent-shell)
  (let ((project (+agent-shell-projects--project)))
    (if new (+agent-shell-projects-start-agent project)
      (let* ((choices (cons "[New shell]" (mapcar #'buffer-name
                                                 (agent-shell-buffers))))
             (choice (completing-read "Author: " choices nil t)))
        (if (equal choice "[New shell]")
            (+agent-shell-projects-start-agent project)
          (+agent-shell-projects-associate project (get-buffer choice)))))))

(defun +agent-shell-projects-default-context (project request)
  "Build automatic context for PROJECT and REQUEST from immutable versions."
  (let* ((agent (plist-get request :agent))
         (versions (or (plist-get request :versions)
                       (when (plist-get request :artefact)
                         (list (list :artefact (plist-get request :artefact)
                                     :version (plist-get request :version))))))
         (proposal (and (plist-get project :proposal)
                        (+agent-shell-projects-artefact
                         project (plist-get project :proposal))))
         (versions (if (and proposal
                            (not (equal (plist-get request :kind) "explanation"))
                            (not (seq-find
                                  (lambda (v) (equal (plist-get v :artefact)
                                                     (plist-get proposal :id)))
                                  versions)))
                       (append versions
                               (list (list :artefact (plist-get proposal :id)
                                           :version (plist-get proposal :current))))
                     versions))
         (bundle
          (list :project (list :id (plist-get project :id)
                                :name (plist-get project :name)
                                :root (plist-get project :root))
                :request request :artefacts
                (mapcar
                 (lambda (reference)
                   (let* ((artefact (+agent-shell-projects-artefact
                                     project (plist-get reference :artefact)))
                          (version (+agent-shell-projects-version
                                    artefact
                                    (or (plist-get reference :version)
                                        (user-error
                                         "Wait for the first capture of %s"
                                         (plist-get artefact :name))))))
                     (list :id (plist-get artefact :id)
                           :name (plist-get artefact :name)
                           :kind (plist-get artefact :kind)
                           :path (plist-get artefact :path)
                           :version (plist-get version :id)
                           :content (if (equal (plist-get artefact :kind) "code")
                                        (+agent-shell-projects--code-context version)
                                      (+agent-shell-projects-content version)))))
                 versions)
                :annotations (plist-get request :feedback)
                :project-discussion (plist-get project :annotations))))
    (concat
     "You are working in an Emacs agent-shell project.\n"
     (pcase (plist-get request :kind)
       ("feedback" "Address this feedback by revising the live artefacts. Reply to each item with changes, evidence, or an explanation. Keep the proposal current. Only the user resolves comments.\n")
       ("question" "Explore the question with the user. Keep your conclusions in this conversation; the user will explicitly capture feedback.\n")
       ("outcome" "Draft a concise annotation capturing the agreed outcome of this discussion. Return it using a draft operation; do not attach it yourself.\n")
       (_ "Return the requested structured result using the operation interface.\n"))
     "The supplied versions are the versions under review. Live source may have advanced. Inspect current files before editing; preserve unrelated and unsaved user work.\n"
     (+agent-shell-projects--json bundle)
     "\n\nOperation transport: write a complete JSON file to a temporary path, then atomically rename it into "
     (+agent-shell-projects--inbox) " with a unique .json filename.\n"
     "Every operation needs unique id, project, agent, and op fields.\n"
     (format "Use project=%s and agent=%s.\n" (plist-get project :id) agent)
     "Supported ops:\n"
     "annotate: artefact, version, start/end (zero-based text offsets), body. Authoring agents only.\n"
     "reply: annotation (existing ID), body.\n"
     "progress: body (short current activity).\n"
     "draft: request (the exact outcome request ID), body.\n"
     "explanation: request, artefact, version, summary, walkthrough. Follow the request's schema.\n"
     "Resolution is reserved for the user. Each operation receives a .result file next to its inbox path containing its result or error.\n")))

(defun +agent-shell-projects--code-context (version)
  "Expand VERSION's saved code contents for an agent context bundle."
  (let* ((manifest (json-parse-string (+agent-shell-projects-content version)
                                     :object-type 'plist :array-type 'list
                                     :null-object nil :false-object :false))
         (bases (make-hash-table :test #'equal)))
    (dolist (base (plist-get manifest :base-files))
      (puthash (plist-get base :path) base bases))
    (list :base (plist-get manifest :base)
          :changes (plist-get manifest :changes)
          :files
          (mapcar
           (lambda (file)
             (let* ((path (plist-get file :path))
                    (base (gethash path bases)))
               (list :path path :binary (plist-get file :binary)
                     :before (+agent-shell-projects--context-file-text base)
                     :after (+agent-shell-projects--context-file-text file)
                     :old-mode (plist-get base :mode) :mode (plist-get file :mode))))
           (plist-get manifest :files)))))

(defun +agent-shell-projects--context-file-text (file)
  "Return saved FILE text for context, representing binary content explicitly."
  (cond ((plist-get file :binary) "[Binary content]")
        ((plist-get file :content)
         (decode-coding-string
          (base64-decode-string (+agent-shell-projects-content
                                (list :blob (plist-get file :content)))) 'utf-8-unix))))

(defun +agent-shell-projects-request (project kind agent &rest properties)
  "Create a KIND request to AGENT in PROJECT with PROPERTIES."
  (let ((request (append (list :id (+agent-shell-projects--id) :kind kind
                               :agent (plist-get agent :id) :status "queued"
                               :time (float-time)) properties)))
    (make-directory (+agent-shell-projects--inbox) t)
    (set-file-modes (+agent-shell-projects--inbox) #o700)
    (+agent-shell-projects--set
     request :prompt (+agent-shell-projects-store-content
                       (funcall +agent-shell-projects-context-function
                                project request)))
    (let ((previous (plist-get project :requests)))
      (+agent-shell-projects--set
       project :requests (append previous (list request)))
      (condition-case err
          (+agent-shell-projects-save project)
        (error (+agent-shell-projects--set project :requests previous)
               (signal (car err) (cdr err)))))
    request))

(defun +agent-shell-projects--request (project id)
  "Return request ID in PROJECT."
  (or (seq-find (lambda (request) (equal (plist-get request :id) id))
                (plist-get project :requests))
      (error "Unknown request")))

(defun +agent-shell-projects--send (shell text)
  "Send TEXT to SHELL when ready and without overwriting an unsent draft."
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

(defun +agent-shell-projects-observations (project)
  "Return versions and activity actually displayed for PROJECT."
  (let (observations)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and +agent-shell-projects--version-id
                   (not (derived-mode-p '+agent-shell-projects-sidebar-mode))
                   (equal +agent-shell-projects--project-id (plist-get project :id))
                   (not (assoc +agent-shell-projects--artefact-id observations)))
          (push (list +agent-shell-projects--artefact-id
                      +agent-shell-projects--version-id
                      +agent-shell-projects--observed-event) observations))))
    observations))

(defun +agent-shell-projects-acknowledge (project observations)
  "Record reviewed OBSERVATIONS in PROJECT without acknowledging newer work."
  (dolist (observation observations)
    (let ((artefact (+agent-shell-projects-artefact project (nth 0 observation))))
      (+agent-shell-projects-version artefact (nth 1 observation))
      (+agent-shell-projects--set artefact :ack (nth 1 observation))
      (+agent-shell-projects--set artefact :ack-event (nth 2 observation))))
  (+agent-shell-projects--set
   project :checkpoints
   (append (plist-get project :checkpoints)
           (list (list :id (+agent-shell-projects--id) :time (float-time)
                       :observations observations))))
  (+agent-shell-projects--changed project nil "user"))

(defun +agent-shell-projects-mark-reviewed ()
  "Acknowledge the versions actually displayed in the current project."
  (interactive)
  (let ((project (+agent-shell-projects--project)))
    (+agent-shell-projects-acknowledge
     project (+agent-shell-projects-observations project))))

(defun +agent-shell-projects--feedback (project)
  "Collect unsubmitted user comments and replies in PROJECT."
  (let (items)
    (dolist (annotation (plist-get project :annotations))
      (when (and (equal (plist-get annotation :author) "user")
                 (not (plist-get annotation :submitted))
                 (not (plist-get annotation :queued)))
        (push (list :id (plist-get annotation :id) :type "annotation"
                    :annotation (copy-tree annotation)) items))
      (dolist (reply (plist-get annotation :replies))
        (when (and (equal (plist-get reply :author) "user")
                   (not (plist-get reply :submitted))
                   (not (plist-get reply :queued)))
          (push (list :id (plist-get reply :id) :type "reply"
                      :annotation (plist-get annotation :id)
                      :reply (copy-tree reply)) items))))
    (nreverse items)))

(defun +agent-shell-projects-submit ()
  "Submit a frozen batch of user feedback to the project's author."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (agent (+agent-shell-projects-agent project (plist-get project :author)))
         (feedback (+agent-shell-projects--feedback project)))
    (when (seq-some
           (lambda (buffer)
             (with-current-buffer buffer
               (and buffer-file-name (buffer-modified-p)
                    (or (equal +agent-shell-projects--project-id (plist-get project :id))
                        (equal +agent-shell-projects--source-project (plist-get project :id))
                        (seq-some
                         (lambda (a) (and (equal (plist-get a :kind) "code")
                                          (file-in-directory-p buffer-file-name (plist-get a :path))))
                         (plist-get project :artefacts))))))
           (buffer-list))
      (user-error "Save edited project source buffers before submitting feedback"))
    (unless feedback (user-error "No new feedback; use Mark reviewed"))
    (let* ((observations (+agent-shell-projects-observations project))
           (versions (mapcar (lambda (o) (list :artefact (car o) :version (cadr o)))
                             observations))
           (request (+agent-shell-projects-request
                     project "feedback" agent :feedback feedback
                     :versions versions :observations observations)))
      (+agent-shell-projects--set project :batches
                                  (append (plist-get project :batches)
                                          (list (plist-get request :id))))
      (dolist (item feedback)
        (dolist (annotation (plist-get project :annotations))
          (when (equal (plist-get annotation :id) (plist-get item :id))
            (+agent-shell-projects--set annotation :queued (plist-get request :id)))
          (dolist (reply (plist-get annotation :replies))
            (when (equal (plist-get reply :id) (plist-get item :id))
              (+agent-shell-projects--set reply :queued (plist-get request :id))))))
      (+agent-shell-projects-save project)
      (+agent-shell-projects-flush project)
      (message "Feedback %s" (plist-get request :status))
      request)))

(defun +agent-shell-projects--accepted (project request)
  "Record successful delivery of REQUEST in PROJECT."
  (+agent-shell-projects--set request :status "sent")
  (when (equal (plist-get request :kind) "feedback")
    (dolist (item (plist-get request :feedback))
      (dolist (annotation (plist-get project :annotations))
        (when (equal (plist-get annotation :id) (plist-get item :id))
          (+agent-shell-projects--set annotation :queued nil)
          (+agent-shell-projects--set annotation :submitted (plist-get request :id)))
        (dolist (reply (plist-get annotation :replies))
          (when (equal (plist-get reply :id) (plist-get item :id))
            (+agent-shell-projects--set reply :queued nil)
            (+agent-shell-projects--set reply :submitted (plist-get request :id))))))
    (+agent-shell-projects-acknowledge
     project (plist-get request :observations)))
  (+agent-shell-projects-save project))

(defun +agent-shell-projects-flush (project)
  "Attempt queued deliveries in PROJECT without duplicating accepted requests."
  (dolist (request (plist-get project :requests))
    (when (equal (plist-get request :status) "queued")
      (let* ((agent (+agent-shell-projects-agent project (plist-get request :agent)))
             (shell (+agent-shell-projects-shell agent)))
        (when (and shell (eq (agent-shell-status :shell-buffer shell) 'ready))
          (condition-case err
              (when (funcall +agent-shell-projects-send-function shell
                             (+agent-shell-projects-content
                              (list :blob (plist-get request :prompt))))
                (+agent-shell-projects--accepted project request))
            (error (+agent-shell-projects--set
                    request :delivery-error (error-message-string err))
                   (+agent-shell-projects-save project))))))))

(defun +agent-shell-projects--apply-operation (operation)
  "Apply a structured agent OPERATION, returning its idempotent result."
  (let* ((project (+agent-shell-projects-load (plist-get operation :project)))
         (agent (+agent-shell-projects-agent project (plist-get operation :agent)))
         (id (plist-get operation :id))
         (previous (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                             (plist-get project :operations)))
         (op (plist-get operation :op))
         result)
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "Missing operation id"))
    (when (member op '("resolve" "reopen"))
      (error "Only the user controls resolution"))
    (if previous (plist-get previous :result)
      (setq result
            (pcase op
              ("annotate"
               (unless (equal (plist-get agent :role) "author")
                 (error "Side discussions require explicit user capture"))
               (let ((artefact (+agent-shell-projects-artefact
                                project (plist-get operation :artefact))))
                 (plist-get
                  (+agent-shell-projects-annotate
                   project artefact
                   (+agent-shell-projects-version
                    artefact (plist-get operation :version))
                   (plist-get operation :start) (plist-get operation :end)
                   (plist-get operation :body) (plist-get agent :id)
                   (plist-get operation :context)) :id)))
              ("reply"
               (unless (equal (plist-get agent :role) "author")
                 (error "Side discussions require explicit user capture"))
               (plist-get
                (+agent-shell-projects-reply
                 project (+agent-shell-projects-annotation
                          project (plist-get operation :annotation))
                 (plist-get operation :body) (plist-get agent :id)) :id))
              ("progress"
               (unless (stringp (plist-get operation :body)) (error "Invalid progress"))
               (+agent-shell-projects--set agent :progress (plist-get operation :body))
               t)
              ("draft"
               (let ((request (+agent-shell-projects--request
                               project (plist-get operation :request))))
                 (unless (and (equal (plist-get request :agent) (plist-get agent :id))
                              (member (plist-get request :kind) '("outcome" "proposal"))
                              (equal (plist-get request :status) "sent")
                              (stringp (plist-get operation :body))
                              (not (string-empty-p
                                    (string-trim (plist-get operation :body)))))
                   (error "Draft does not match a pending outcome request"))
                 (+agent-shell-projects--set request :result (plist-get operation :body))
                 (+agent-shell-projects--set request :status "ready")
                 (run-hook-with-args '+agent-shell-projects-request-ready-hook
                                     project request)
                 (plist-get request :id)))
              (_ (if-let* ((handler (cdr (assoc op +agent-shell-projects-operation-handlers))))
                     (funcall handler project agent operation)
                   (error "Unsupported operation %s" op)))))
      (+agent-shell-projects--set
       project :operations
       (append (plist-get project :operations)
               (list (list :id id :result result))))
      (+agent-shell-projects--changed project nil)
      result)))

(defun +agent-shell-projects--merge-record (target source)
  "Update TARGET from SOURCE, preserving existing records identified by ID."
  (cl-loop for (key value) on source by #'cddr do
           (+agent-shell-projects--set
            target key
            (if (and (consp value) (listp (car value))
                     (plist-get (car value) :id))
                (mapcar
                 (lambda (record)
                   (let ((existing (seq-find
                                    (lambda (old) (equal (plist-get old :id)
                                                         (plist-get record :id)))
                                    (plist-get target key))))
                     (if existing
                         (progn (+agent-shell-projects--merge-record existing record)
                                existing)
                       (copy-tree record)))) value)
              (copy-tree value))))
  target)

(defun +agent-shell-projects-apply-operation (operation)
  "Commit structured OPERATION atomically before displaying its result."
  (when (or (equal (plist-get operation :op) "explanation")
            (equal (plist-get operation :op) "draft"))
    (require '+agent-shell-projects-review))
  (let* ((project-id (plist-get operation :project))
         (project (+agent-shell-projects-load project-id))
         (working (copy-tree project))
         pending result)
    (condition-case err
        (progn
          (puthash project-id working +agent-shell-projects--projects)
          (let ((+agent-shell-projects--defer-save t)
                (+agent-shell-projects-change-hook nil)
                (+agent-shell-projects-request-ready-hook
                 (list (lambda (p r) (push (cons p r) pending)))))
            (setq result (+agent-shell-projects--apply-operation operation)))
          (+agent-shell-projects-save working)
          (+agent-shell-projects--merge-record project working)
          (puthash project-id project +agent-shell-projects--projects))
      (error (puthash project-id project +agent-shell-projects--projects)
             (signal (car err) (cdr err))))
    (run-hook-with-args '+agent-shell-projects-change-hook project nil)
    (dolist (entry pending)
      (condition-case err
          (run-hook-with-args '+agent-shell-projects-request-ready-hook
                              project (+agent-shell-projects--request
                                       project (plist-get (cdr entry) :id)))
        (error (message "Project result saved; reopen its draft: %s"
                        (error-message-string err)))))
    result))

(defun +agent-shell-projects-cancel-request ()
  "Cancel a queued delivery or pending generated result in this project."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (items (seq-filter
                 (lambda (r)
                   (or (equal (plist-get r :status) "queued")
                       (and (not (equal (plist-get r :kind) "feedback"))
                            (member (plist-get r :status) '("sent" "ready")))))
                 (plist-get project :requests)))
         (choices (mapcar (lambda (r)
                            (cons (format "%s · %s [%s]"
                                          (plist-get r :kind) (plist-get r :status)
                                          (substring (plist-get r :id) 0 6)) r)) items)))
    (unless choices (user-error "No pending requests"))
    (let* ((request (cdr (assoc (completing-read "Cancel: " choices nil t) choices)))
           (id (plist-get request :id)))
      (+agent-shell-projects--set request :status "cancelled")
      (dolist (annotation (plist-get project :annotations))
        (when (equal (plist-get annotation :queued) id)
          (+agent-shell-projects--set annotation :queued nil))
        (dolist (reply (plist-get annotation :replies))
          (when (equal (plist-get reply :queued) id)
            (+agent-shell-projects--set reply :queued nil))))
      (+agent-shell-projects--changed project nil "user"))))

(defun +agent-shell-projects-receive-file (file)
  "Process JSON operation FILE and write a structured result beside it."
  (let ((response
         (condition-case err
             (list :ok t :result (+agent-shell-projects-apply-operation
                                 (+agent-shell-projects--read-json file)))
           (error (list :ok :false :error (error-message-string err))))))
    (+agent-shell-projects--atomic-write
     (concat file ".result") (+agent-shell-projects--json response))
    (rename-file file (concat file ".processed") t)
    response))

(defun +agent-shell-projects-poll ()
  "Drain complete inbox operations, reconnect shells, and flush pending work."
  (unless +agent-shell-projects--polling
    (let ((+agent-shell-projects--polling t))
      (when (file-directory-p (+agent-shell-projects--inbox))
        (dolist (file (directory-files (+agent-shell-projects--inbox) t "\\.json\\'"))
          (condition-case err (+agent-shell-projects-receive-file file)
            (error (message "Project inbox: %s" (error-message-string err))))))
      (dolist (project (+agent-shell-projects-list))
        (dolist (agent (plist-get project :agents))
          (when-let* ((shell (+agent-shell-projects-shell agent)))
            (+agent-shell-projects--attach project agent shell)))
        (+agent-shell-projects-flush project)))))

(defun +agent-shell-projects--start-agent ()
  "Start inbox polling once."
  (require 'agent-shell)
  (unless (timerp +agent-shell-projects--timer)
    (setq +agent-shell-projects--timer
          (+agent-shell-projects-run-at-time 1 1 #'+agent-shell-projects-poll))))

(defun +agent-shell-projects--stop-agent ()
  "Stop polling, pending event captures and all agent subscriptions."
  (when (timerp +agent-shell-projects--timer)
    (+agent-shell-projects-cancel-timer +agent-shell-projects--timer))
  (setq +agent-shell-projects--timer nil)
  (maphash (lambda (_id timer) (+agent-shell-projects-cancel-timer timer))
           +agent-shell-projects--event-timers)
  (clrhash +agent-shell-projects--event-timers)
  (mapc #'+agent-shell-projects--detach
        (hash-table-keys +agent-shell-projects--subscriptions)))

(defun +agent-shell-projects--activity (project _artefact _view)
  "Insert live agent activity for PROJECT in a sidebar."
  (dolist (agent (plist-get project :agents))
    (when (equal (plist-get agent :id) (plist-get project :author))
      (let ((shell (+agent-shell-projects-shell agent)))
        (insert-text-button
         (if shell (buffer-name shell) (plist-get agent :buffer))
         'follow-link t 'project-target shell
         'action (lambda (_button)
                   (if (buffer-live-p shell) (pop-to-buffer shell)
                     (user-error "Author is disconnected"))))
        (insert (format " · %s\n%s\n\n"
                        (if shell (agent-shell-status :shell-buffer shell)
                          "disconnected")
                        (or (plist-get agent :progress) "")))))))

(+agent-shell-projects-register-hook '+agent-shell-projects-sidebar-functions #'+agent-shell-projects--activity)

(defun +agent-shell-projects-ask (&optional target question)
  "Start a fresh side conversation about TARGET, asking QUESTION."
  (interactive)
  (let* ((selection (if target (+agent-shell-projects--target-context target)
                      (+agent-shell-projects--selection)))
         (project (+agent-shell-projects-load (plist-get selection :project)))
         (question (or question (read-string "Question: "))))
    (when (string-empty-p (string-trim question)) (user-error "Question is empty"))
    (+agent-shell-projects-mode 1)
    (let* ((shell (+agent-shell-projects-start-agent project "side"))
           (agent (seq-find (lambda (a) (eq (+agent-shell-projects-shell a) shell))
                            (plist-get project :agents)))
           (request (+agent-shell-projects-request
                     project "question" agent :selection selection
                     :artefact (plist-get selection :artefact)
                     :version (plist-get selection :version) :question question)))
      (with-current-buffer shell
        (setq +agent-shell-projects--project-id (plist-get project :id)
              +agent-shell-projects--side-question (plist-get request :id)))
      (+agent-shell-projects-flush project)
      (pop-to-buffer shell)
      request)))

(defun +agent-shell-projects-target-ask (target)
  "Ask a side agent about Embark TARGET."
  (interactive "sPassage target: ")
  (let* ((context (+agent-shell-projects--target-context target))
         (annotation-id (plist-get context :annotation)))
    (if (not annotation-id) (+agent-shell-projects-ask target)
      (let* ((project (+agent-shell-projects-load (plist-get context :project)))
             (annotation (+agent-shell-projects-annotation project annotation-id))
             (anchor (or (plist-get annotation :anchor) annotation))
             (selection (list :project (plist-get project :id)
                              :artefact (plist-get annotation :artefact)
                              :version (plist-get anchor :version)
                              :start (plist-get anchor :start)
                              :end (plist-get anchor :end)
                              :context (plist-get anchor :context))))
        (+agent-shell-projects-ask
         (cdr (+agent-shell-projects--register-target
               '+agent-shell-projects-passage selection)))))))

(defun +agent-shell-projects-open-outcome (project request)
  "Open REQUEST's draft for explicit user attachment in PROJECT."
  (unless (equal (plist-get request :status) "ready")
    (user-error "This draft is not ready or was already captured"))
  (let ((selection (plist-get request :selection)))
    (+agent-shell-projects-compose
     selection (plist-get request :result)
     (lambda (body)
       (unless (equal (plist-get request :status) "ready")
         (user-error "This outcome was already captured"))
       (let* ((artefact (+agent-shell-projects-artefact
                         project (plist-get selection :artefact)))
              (annotation (+agent-shell-projects-annotate
                           project artefact
                           (+agent-shell-projects-version
                            artefact (plist-get selection :version))
                           (plist-get selection :start) (plist-get selection :end)
                           body "user" (plist-get selection :context))))
         (+agent-shell-projects--set annotation :origin (plist-get request :id))
         (+agent-shell-projects--set request :status "captured")
         (+agent-shell-projects-save project))))))

(defun +agent-shell-projects--outcome-ready (project request)
  "Present a completed outcome REQUEST for user editing in PROJECT."
  (when (equal (plist-get request :kind) "outcome")
    (+agent-shell-projects-open-outcome project request)))

(defun +agent-shell-projects-capture-outcome ()
  "Request an editable annotation draft from the current side conversation."
  (interactive)
  (let* ((candidate (if +agent-shell-projects--side-question (current-buffer)
                  (and (fboundp 'agent-shell-shell-buffer)
                       (agent-shell-shell-buffer :no-error t :no-create t))))
         (shell (and candidate (buffer-local-value
                                '+agent-shell-projects--side-question candidate) candidate))
         (project (if shell
                      (+agent-shell-projects-load
                       (buffer-local-value '+agent-shell-projects--project-id shell))
                    (+agent-shell-projects--project)))
         (questions (seq-filter (lambda (r) (equal (plist-get r :kind) "question"))
                                (plist-get project :requests)))
         (question-id (or (and shell (buffer-local-value
                                      '+agent-shell-projects--side-question shell))
                          (cdr (assoc
                                (completing-read
                                 "Side question: "
                                 (mapcar (lambda (q) (cons (plist-get q :question)
                                                          (plist-get q :id)))
                                         questions) nil t)
                                (mapcar (lambda (q) (cons (plist-get q :question)
                                                         (plist-get q :id)))
                                        questions)))))
         (question (+agent-shell-projects--request project question-id))
         (existing (seq-find
                    (lambda (r)
                      (and (equal (plist-get r :parent) question-id)
                           (member (plist-get r :status) '("queued" "sent" "ready"))))
                    (reverse (plist-get project :requests)))))
    (if existing
        (if (equal (plist-get existing :status) "ready")
            (+agent-shell-projects-open-outcome project existing)
          (message "An outcome draft is already pending"))
      (let ((request (+agent-shell-projects-request
                      project "outcome"
                      (+agent-shell-projects-agent project (plist-get question :agent))
                      :parent question-id :selection (plist-get question :selection)
                      :artefact (plist-get question :artefact)
                      :version (plist-get question :version)
                      :question (plist-get question :question))))
        (+agent-shell-projects-flush project)
        (message "Outcome draft %s" (plist-get request :status))
        request))))

(defun +agent-shell-projects-markdown-to-org (text)
  "Convert common Markdown prose in TEXT to Org, preserving fenced code."
  (let (fenced lines)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-match "^[ \t]*```\\([^ `]*\\).*$" line)
        (if fenced (push "#+end_src" lines)
          (push (concat "#+begin_src " (match-string 1 line)) lines))
        (setq fenced (not fenced)))
       (fenced (push line lines))
       (t
        (when (string-match "^\\(#+\\)[ \t]+" line)
          (setq line (concat (make-string (length (match-string 1 line)) ?*)
                             " " (substring line (match-end 0)))))
        (setq line (replace-regexp-in-string
                    "\\[\\([^]\n]+\\)\\](\\([^ )\n]+\\))" "[[\\2][\\1]]" line))
        (setq line (replace-regexp-in-string "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" line))
        (setq line (replace-regexp-in-string "`\\([^`\n]+\\)`" "~\\1~" line))
        (push line lines))))
    (when fenced (push "#+end_src" lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defcustom +agent-shell-projects-org-convert-function
  #'+agent-shell-projects-markdown-to-org
  "Function converting extracted Markdown prose to Org syntax."
  :type 'function :group '+agent-shell-projects)

(defun +agent-shell-projects-extract-prose (project text file &optional heading)
  "Extract TEXT into FILE and enrol it in PROJECT.
HEADING is a new heading title or a marker at an existing Org heading."
  (let* ((file (expand-file-name file))
         (orgp (string-match-p "\\.org\\'" file))
         (text (if orgp (funcall +agent-shell-projects-org-convert-function text) text))
         (buffer (find-file-noselect file))
         root)
    (make-directory (file-name-directory file) t)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (if (and orgp heading)
            (progn
              (require 'org)
              (if (markerp heading)
                  (progn (goto-char heading) (org-back-to-heading t))
                (insert "* " heading "\n") (forward-line -1))
              (setq root (point-marker))
              (let ((level (org-outline-level)))
                (org-end-of-subtree t t)
                (unless (bolp) (insert "\n"))
                (insert (replace-regexp-in-string
                         "^\\(\\*+\\) "
                         (lambda (match) (concat (make-string level ?*) match)) text)
                        "\n")))
          (insert text "\n"))
        (save-buffer)
        (when root (goto-char root))
        (+agent-shell-projects-enrol
         project file (if (stringp heading) heading (file-name-nondirectory file))
         (and root t))))))

(defun +agent-shell-projects-extract (start end file)
  "Extract conversation prose from START to END into FILE."
  (interactive (progn
                 (unless (use-region-p) (user-error "Select conversation prose"))
                 (list (region-beginning) (region-end)
                       (read-file-name "Save prose to: "))))
  (let* ((project (if +agent-shell-projects--project-id
                      (+agent-shell-projects--project)
                    (+agent-shell-projects--choose-project)))
         (text (if (fboundp 'agent-shell-markdown-reconstruct)
                   (agent-shell-markdown-reconstruct start end)
                 (buffer-substring-no-properties start end)))
         (heading (when (string-match-p "\\.org\\'" file)
                    (+agent-shell-projects-choose-org-heading file)))
         (artefact (+agent-shell-projects-extract-prose project text file heading)))
    (+agent-shell-projects-open project (plist-get artefact :id))))

(defun +agent-shell-projects-choose-org-heading (file)
  "Choose a heading in FILE, a new heading, or the complete file."
  (require 'org)
  (let* ((buffer (find-file-noselect file))
         (headings (with-current-buffer buffer
                     (save-restriction
                       (widen)
                       (org-map-entries
                        (lambda () (cons (format "%s:%s" (line-number-at-pos)
                                                 (org-get-heading t t t t))
                                         (point-marker)))))))
         (choice (completing-read "Org destination: "
                                  (append '("[New heading]" "[Whole file]") headings)
                                  nil t)))
    (cond ((equal choice "[Whole file]") nil)
          ((equal choice "[New heading]") (read-string "Heading title: "))
          (t (cdr (assoc choice headings))))))

(define-key +agent-shell-projects-passage-map (kbd "q")
            #'+agent-shell-projects-target-ask)
(define-key +agent-shell-projects-annotation-map (kbd "q")
            #'+agent-shell-projects-target-ask)
(+agent-shell-projects-register-hook '+agent-shell-projects-request-ready-hook
          #'+agent-shell-projects--outcome-ready)

(provide '+agent-shell-projects-agent)
;;; +agent-shell-projects-agent.el ends here
