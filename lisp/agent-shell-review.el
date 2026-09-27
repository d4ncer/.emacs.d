;;; agent-shell-review.el --- Review changes with a fresh agent -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") (agent-shell "0"))

;;; Commentary:
;; Capture a project changeset and requirements, then manage one review pass
;; through agent-shell.  `agent-shell-review-ui' supplies the optional sidebar.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'agent-shell)
(require 'agent-shell-review-git)
(require 'agent-shell-review-spec)
(require 'agent-shell-review-protocol)

(defgroup agent-shell-review nil
  "Structured code reviews through agent-shell."
  :group 'agent-shell)

(defcustom agent-shell-review-agent-config nil
  "Agent config for fresh reviewer sessions, or nil for the preferred agent."
  :type '(choice (const nil) sexp))

(defcustom agent-shell-review-base-ref nil
  "Integration base ref to use for review snapshots, or nil to discover it."
  :type '(choice (const nil) string))

(defvar agent-shell-review-status-change-hook nil
  "Hook called with a review run and its new status.")

(defvar agent-shell-review--reviewers (make-hash-table :test #'eq)
  "Map live reviewer shell buffers to review runs.")

(defvar-local agent-shell-review--current-run nil
  "Review run shown in the current sidebar buffer.")

(cl-defstruct agent-shell-review--run
  project implementation-shell reviewer-shell snapshot requirements
  clarifications status text repair-attempt result error-message
  sidebar-buffer subscription pending-prompt read-only-configured
  answers marks expanded stale-result)

(defun agent-shell-review-reviewer-shell-p (buffer)
  "Return non-nil when BUFFER belongs to a review agent."
  (and (buffer-live-p buffer)
       (gethash buffer agent-shell-review--reviewers)))

(defun agent-shell-review-sidebar-for-shell (buffer)
  "Return the review sidebar buffer associated with reviewer BUFFER."
  (when-let* ((run (agent-shell-review-reviewer-shell-p buffer)))
    (agent-shell-review--run-sidebar-buffer run)))

(defun agent-shell-review--set-status (run status)
  "Set RUN to STATUS and announce a real transition."
  (unless (eq status (agent-shell-review--run-status run))
    (setf (agent-shell-review--run-status run) status)
    (run-hook-with-args 'agent-shell-review-status-change-hook run status))
  (when (fboundp 'agent-shell-review-ui-render)
    (agent-shell-review-ui-render run)))

(defun agent-shell-review--present (run)
  "Show RUN if the optional UI package is available."
  (when (fboundp 'agent-shell-review-ui-show)
    (agent-shell-review-ui-show run)))

(defun agent-shell-review--project-root ()
  "Return the Git project root for the current buffer."
  (or (locate-dominating-file default-directory ".git")
      (user-error "This buffer is not in a Git project")))

(defun agent-shell-review--matching-shells (root)
  "Return implementation shells whose project is ROOT."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (not (agent-shell-review-reviewer-shell-p buffer))
          (equal (file-truename root)
                 (file-truename
                  (with-current-buffer buffer (agent-shell-cwd))))))
   (agent-shell-buffers)))

(defun agent-shell-review--choose-shell (root)
  "Choose an implementation shell for ROOT, if any."
  (let ((matches (agent-shell-review--matching-shells root)))
    (pcase (length matches)
      (0 nil)
      (1 (car matches))
      (_ (get-buffer
          (completing-read "Implementation shell: "
                           (mapcar #'buffer-name matches) nil t))))))

(defun agent-shell-review--start-shell (root)
  "Start a fresh review agent rooted at ROOT without taking focus."
  (let ((config (if agent-shell-review-agent-config
                    (or (agent-shell--resolve-config-designator
                         agent-shell-review-agent-config)
                        (user-error "Unknown review agent config: %s"
                                    agent-shell-review-agent-config))
                  (or (agent-shell--resolve-preferred-config)
                      (agent-shell-select-config
                       :prompt "Review with agent: ")))))
    (unless config
      (user-error "No agent-shell reviewer config is available"))
    (let ((default-directory root))
      (agent-shell--start :config config :no-focus t :new-session t
                          :session-strategy 'new))))

(defun agent-shell-review--configure-read-only (run continuation)
  "Select a read-only mode for RUN when advertised, then call CONTINUATION."
  (let ((shell (agent-shell-review--run-reviewer-shell run)))
    (with-current-buffer shell
      (let* ((modes (agent-shell--get-available-modes
                     (agent-shell--state)))
             (read-only
              (cl-find-if
               (lambda (mode)
                 (cl-some
                  (lambda (value)
                    (and (stringp value)
                         (string-match-p "read[ -]?only"
                                         (downcase value))))
                  (list (map-elt mode :id) (map-elt mode :name))))
               modes)))
        (if read-only
            (agent-shell--config-option-set-mode-id
             :mode-id (map-elt read-only :id)
             :on-success continuation
             :on-failure
             (lambda (_error _message)
               (setf (agent-shell-review--run-error-message run)
                     "Could not select the reviewer's read-only mode")
               (agent-shell-review--set-status run 'error)))
          (funcall continuation))))))

(defun agent-shell-review--insert (run text)
  "Submit TEXT to RUN's reviewer shell without taking focus."
  (let ((shell (agent-shell-review--run-reviewer-shell run)))
    (unless (buffer-live-p shell)
      (user-error "Reviewer shell is no longer available"))
    (agent-shell-insert :text text :submit t :no-focus t
                        :shell-buffer shell)))

(defun agent-shell-review--submit-pending (run)
  "Submit RUN's initial prompt after the agent is ready."
  (when-let* ((prompt (agent-shell-review--run-pending-prompt run)))
    (setf (agent-shell-review--run-pending-prompt run) nil)
    (agent-shell-review--insert run prompt)
    (agent-shell-review--set-status run 'reviewing)))

(defun agent-shell-review--parse-result (run)
  "Parse RUN's complete turn, attempting one reformat on failure."
  (condition-case err
      (let ((result (agent-shell-review-protocol-parse
                     (agent-shell-review--run-text run)
                     (agent-shell-review--run-project run))))
        (setf (agent-shell-review--run-result run) result
              (agent-shell-review--run-stale-result run) nil)
        (agent-shell-review--set-status run (plist-get result :kind)))
    (error
     (if (agent-shell-review--run-repair-attempt run)
         (progn
           (setf (agent-shell-review--run-error-message run)
                 (format "Reviewer result could not be parsed: %s"
                         (error-message-string err)))
           (agent-shell-review--set-status run 'error))
       (setf (agent-shell-review--run-repair-attempt run) t
             (agent-shell-review--run-text run) "")
       (agent-shell-review--insert
        run (concat "Reformat your previous review answer as exactly one "
                    "JSON object matching the requested schema. Do not "
                    "redo the analysis or edit files."))))))

(defun agent-shell-review--handle-event (run event)
  "Handle one agent-shell EVENT for RUN."
  (let ((name (map-elt event :event))
        (data (map-elt event :data)))
    (pcase name
      ('init-finished
       (when (agent-shell-review--run-pending-prompt run)
         (if (agent-shell-review--run-read-only-configured run)
             (agent-shell-review--submit-pending run)
           (setf (agent-shell-review--run-read-only-configured run) t)
           (agent-shell-review--configure-read-only
            run (lambda () (agent-shell-review--submit-pending run))))))
      ('agent-message-chunk
       (when-let* ((chunk (map-elt data :text-chunk)))
         (setf (agent-shell-review--run-text run)
               (concat (or (agent-shell-review--run-text run) "") chunk))))
      ('turn-complete
       (if (equal (map-elt data :stop-reason) "end_turn")
           (agent-shell-review--parse-result run)
         (setf (agent-shell-review--run-error-message run)
               (format "Reviewer stopped: %s"
                       (or (map-elt data :stop-reason) "unknown reason")))
         (agent-shell-review--set-status run 'error)))
      ('error
       (setf (agent-shell-review--run-error-message run)
             (format "Reviewer failed: %s"
                     (or (map-elt data :message) "inspect its shell")))
       (agent-shell-review--set-status run 'error))
      ('clean-up
       (remhash (agent-shell-review--run-reviewer-shell run)
                agent-shell-review--reviewers)
       (when (memq (agent-shell-review--run-status run)
                   '(starting reviewing))
         (setf (agent-shell-review--run-error-message run)
               "Reviewer shell closed before producing a result")
         (agent-shell-review--set-status run 'error))))))

(defun agent-shell-review--submit-answers (run answers)
  "Continue RUN in its reviewer shell with question ANSWERS."
  (unless (eq (agent-shell-review--run-status run) 'questions)
    (user-error "This review is not waiting for answers"))
  (when (cl-some (lambda (answer)
                   (string-empty-p (string-trim (or (cdr answer) ""))))
                 answers)
    (user-error "Every review question needs an answer or unknown"))
  (setf (agent-shell-review--run-clarifications run)
        (append (agent-shell-review--run-clarifications run) answers)
        (agent-shell-review--run-text run) ""
        (agent-shell-review--run-repair-attempt run) nil)
  (agent-shell-review--insert
   run (concat "Use these clarified success criteria to finish the same "
               "review. Ask further questions if needed. Return the "
               "requested JSON result only.\n\n"
               (mapconcat (lambda (answer)
                            (format "Q: %s\nA: %s"
                                    (car answer) (cdr answer)))
                          answers "\n\n")))
  (agent-shell-review--set-status run 'reviewing))

(defun agent-shell-review--begin (root implementation-shell &optional spec-file
                                       clarifications stale-result)
  "Start a review in ROOT using IMPLEMENTATION-SHELL and optional SPEC-FILE.
CLARIFICATIONS are carried into a fresh pass; STALE-RESULT remains visible."
  (let ((run (make-agent-shell-review--run
              :project root :implementation-shell implementation-shell
              :clarifications clarifications :status 'collecting
              :stale-result stale-result :text "")))
    (agent-shell-review--present run)
    (condition-case err
        (progn
          (setf (agent-shell-review--run-snapshot run)
                (agent-shell-review-git-snapshot
                 root agent-shell-review-base-ref))
          (agent-shell-review--set-status run 'discovering)
          (setf (agent-shell-review--run-requirements run)
                (agent-shell-review-spec-resolve
                 root implementation-shell spec-file))
          (agent-shell-review--set-status run 'starting)
          (setf (agent-shell-review--run-pending-prompt run)
                (agent-shell-review-protocol-prompt
                 (agent-shell-review--run-snapshot run)
                 (agent-shell-review--run-requirements run)
                 clarifications))
          (let ((reviewer (agent-shell-review--start-shell root)))
            (setf (agent-shell-review--run-reviewer-shell run) reviewer)
            (puthash reviewer run agent-shell-review--reviewers)
            (setf (agent-shell-review--run-subscription run)
                  (agent-shell-subscribe-to
                   :shell-buffer reviewer
                   :on-event (lambda (event)
                               (agent-shell-review--handle-event run event))))))
      (error
       (setf (agent-shell-review--run-error-message run)
             (error-message-string err))
       (agent-shell-review--set-status run 'error)))
    (agent-shell-review--present run)
    run))

(defun agent-shell-review--start-implementation-shell (root)
  "Start a replacement implementation shell in ROOT."
  (let ((config (or (agent-shell--resolve-preferred-config)
                    (agent-shell-select-config
                     :prompt "Fix with agent: "))))
    (unless config
      (user-error "No implementation agent config is available"))
    (let ((default-directory root))
      (agent-shell--start :config config :no-focus t :new-session t
                          :session-strategy 'new))))

;;;###autoload
(defun agent-shell-review (&optional arg)
  "Review the active changeset with a fresh agent.
With prefix ARG, select a requirements file explicitly."
  (interactive "P")
  (let* ((prior (and (derived-mode-p 'agent-shell-review-mode)
                     agent-shell-review--current-run))
         (root (or (and prior (agent-shell-review--run-project prior))
                   (agent-shell-review--project-root)))
         (implementation
          (cond
           (prior (agent-shell-review--run-implementation-shell prior))
           ((derived-mode-p 'agent-shell-mode) (current-buffer))
           (t (agent-shell-review--choose-shell root))))
         (file (and arg (read-file-name "Review spec: " nil nil t))))
    (agent-shell-review--begin root implementation file
                               (and prior
                                    (agent-shell-review--run-clarifications
                                     prior)))))

(require 'agent-shell-review-ui)

(provide 'agent-shell-review)
;;; agent-shell-review.el ends here
