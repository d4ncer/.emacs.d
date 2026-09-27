;;; agent-shell-review.el --- Review changes with direct ACP -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1") (acp "0"))

;;; Commentary:
;; Capture project changes and requirements, then coordinate an ACP review.
;; The optional UI and origin provider do not affect the reviewer transport.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-shell-review-git)
(require 'agent-shell-review-spec)
(require 'agent-shell-review-protocol)
(require 'agent-shell-review-acp)

(defgroup agent-shell-review nil
  "Structured code reviews through ACP."
  :group 'tools)

(defcustom agent-shell-review-base-ref nil
  "Integration base ref to use for review snapshots, or nil to discover it."
  :type '(choice (const nil) string))

(defcustom agent-shell-review-origin-provider nil
  "Optional function that returns implementation context for a project root.
It runs in the invoking buffer and returns a plist with :context and :send."
  :type '(choice (const nil) function))

(defvar agent-shell-review-status-change-hook nil
  "Hook called with a review run and its new status.")

(defvar-local agent-shell-review--current-run nil
  "Review run shown in the current sidebar buffer.")

(cl-defstruct agent-shell-review--run
  project origin send-fixes transport snapshot requirements clarifications
  status text repair-attempt result error-message sidebar-buffer
  pending-prompt answers marks expanded stale-result cancelled fix-prompt)

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

(defun agent-shell-review--active-p (run)
  "Return non-nil if RUN may still receive reviewer events."
  (and (not (agent-shell-review--run-cancelled run))
       (let ((buffer (agent-shell-review--run-sidebar-buffer run)))
         (or (null buffer)
             (and (buffer-live-p buffer)
                  (eq (buffer-local-value 'agent-shell-review--current-run
                                          buffer)
                      run))))))

(defun agent-shell-review--cancel (run)
  "Cancel RUN and shut down its reviewer process."
  (unless (agent-shell-review--run-cancelled run)
    (setf (agent-shell-review--run-cancelled run) t)
    (when-let* ((transport (agent-shell-review--run-transport run)))
      (agent-shell-review-acp-close transport))))

(defun agent-shell-review--terminal-error (run message)
  "Show terminal MESSAGE for RUN and stop its reviewer."
  (setf (agent-shell-review--run-error-message run) message)
  (agent-shell-review--set-status run 'error)
  (agent-shell-review--cancel run))

(defun agent-shell-review--send (run prompt)
  "Submit PROMPT to RUN's reviewer process."
  (unless (agent-shell-review-acp-send
           (agent-shell-review--run-transport run) prompt)
    (unless (agent-shell-review--run-error-message run)
      (agent-shell-review--terminal-error
       run "Reviewer prompt was not sent"))))

(defun agent-shell-review--parse-result (run)
  "Parse RUN's complete turn, requesting one reformat when needed."
  (condition-case err
      (let ((result (agent-shell-review-protocol-parse
                     (agent-shell-review--run-text run)
                     (agent-shell-review--run-project run))))
        (setf (agent-shell-review--run-result run) result
              (agent-shell-review--run-stale-result run) nil)
        (agent-shell-review--set-status run (plist-get result :kind))
        (when (memq (plist-get result :kind) '(findings clear))
          (agent-shell-review-acp-close
           (agent-shell-review--run-transport run))))
    (error
     (if (agent-shell-review--run-repair-attempt run)
         (agent-shell-review--terminal-error
          run (format "Reviewer result could not be parsed: %s"
                      (error-message-string err)))
       (setf (agent-shell-review--run-repair-attempt run) t
             (agent-shell-review--run-text run) "")
       (agent-shell-review--send
        run (concat "Reformat your previous review answer as exactly one "
                    "JSON object matching the requested schema. Do not "
                    "redo the analysis or edit files."))))))

(defun agent-shell-review--handle-event (run event)
  "Handle a direct ACP EVENT for RUN."
  (when (agent-shell-review--active-p run)
    (pcase (plist-get event :type)
      ('ready
       (when-let* ((prompt (agent-shell-review--run-pending-prompt run)))
         (setf (agent-shell-review--run-pending-prompt run) nil)
         (agent-shell-review--set-status run 'reviewing)
         (agent-shell-review--send run prompt)))
      ('chunk
       (when-let* ((chunk (plist-get event :text)))
         (setf (agent-shell-review--run-text run)
               (concat (or (agent-shell-review--run-text run) "") chunk))))
      ('complete
       (if (equal (plist-get event :stop-reason) "end_turn")
           (agent-shell-review--parse-result run)
         (agent-shell-review--terminal-error
          run (format "Reviewer stopped: %s"
                      (or (plist-get event :stop-reason) "unknown reason")))))
      ('error
       (agent-shell-review--terminal-error
        run (format "Reviewer failed: %s"
                    (or (plist-get event :message) "inspect diagnostics")))))))

(defun agent-shell-review--submit-answers (run answers)
  "Continue RUN with question ANSWERS on its existing ACP session."
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
  (agent-shell-review--set-status run 'reviewing)
  (agent-shell-review--send
   run (concat "Use these clarified success criteria to finish the same "
               "review. Ask further questions if needed. Return the "
               "requested JSON result only.\n\n"
               (mapconcat (lambda (answer)
                            (format "Q: %s\nA: %s"
                                    (car answer) (cdr answer)))
                          answers "\n\n"))))

(defun agent-shell-review--begin (root origin &optional spec-file
                                       clarifications stale-result
                                       requirements-override)
  "Start a review in ROOT using ORIGIN and optional SPEC-FILE.
CLARIFICATIONS carry into a fresh pass; STALE-RESULT remains visible.
REQUIREMENTS-OVERRIDE reuses manually entered criteria on a rerun."
  (let ((run (make-agent-shell-review--run
              :project root :origin origin
              :send-fixes (plist-get origin :send)
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
                (or requirements-override
                    (agent-shell-review-spec-resolve
                     root (plist-get origin :context) spec-file)))
          (agent-shell-review--set-status run 'starting)
          (setf (agent-shell-review--run-pending-prompt run)
                (agent-shell-review-protocol-prompt
                 (agent-shell-review--run-snapshot run)
                 (agent-shell-review--run-requirements run)
                 clarifications))
          (let* ((requirements (agent-shell-review--run-requirements run))
                 (file (and (eq (plist-get requirements :kind) 'file)
                            (plist-get requirements :source)))
                 (transport
                  (agent-shell-review-acp-create
                   root file
                   (lambda (event)
                     (agent-shell-review--handle-event run event)))))
            (setf (agent-shell-review--run-transport run) transport)
            (agent-shell-review-acp-start transport)))
      (error
       (agent-shell-review--terminal-error
        run (error-message-string err))))
    (agent-shell-review--present run)
    run))

;;;###autoload
(defun agent-shell-review (&optional arg)
  "Review the active changeset through a dedicated ACP process.
With prefix ARG, select a requirements file explicitly."
  (interactive "P")
  (let* ((prior (and (derived-mode-p 'agent-shell-review-mode)
                     agent-shell-review--current-run))
         (root (or (and prior (agent-shell-review--run-project prior))
                   (agent-shell-review--project-root)))
         (origin (if prior
                     (agent-shell-review--run-origin prior)
                   (when agent-shell-review-origin-provider
                     (funcall agent-shell-review-origin-provider root))))
         (file (and arg (read-file-name "Review spec: " nil nil t)))
         (clarifications (and prior
                              (agent-shell-review--run-clarifications prior))))
    (when prior (agent-shell-review--cancel prior))
    (agent-shell-review--begin root origin file clarifications
                               (and prior
                                    (eq (agent-shell-review--run-status prior)
                                        'findings)
                                    (agent-shell-review--run-result prior)))))

(require 'agent-shell-review-ui)

(provide 'agent-shell-review)
;;; agent-shell-review.el ends here
