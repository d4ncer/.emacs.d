;;; +agent-shell-projects-manage.el --- Project lifecycle actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Detach enrolments without losing review history; archive projects reversibly.

;;; Code:

(require '+agent-shell-projects)
(require '+agent-shell-projects-agent)
(declare-function +agent-shell-projects-stop-project-workers "+agent-shell-projects-jobs")
(declare-function +agent-shell-projects--code-source-saved "+agent-shell-projects-code")

(defun +agent-shell-projects--cancel-pending (project &optional agent-id)
  "Cancel pending requests in PROJECT, optionally only for AGENT-ID."
  (dolist (request (plist-get project :requests))
    (when (and (or (not agent-id) (equal agent-id (plist-get request :agent)))
               (or (equal (plist-get request :status) "queued")
                   (and (not (equal (plist-get request :kind) "feedback"))
                        (member (plist-get request :status) '("sent" "ready")))))
      (+agent-shell-projects--cancel-request-record project request))))

(defun +agent-shell-projects--cancel-event (project)
  "Stop deferred agent capture for PROJECT."
  (let ((id (plist-get project :id)))
    (when-let* ((timer (gethash id +agent-shell-projects--event-timers)))
      (+agent-shell-projects-cancel-timer timer)
      (remhash id +agent-shell-projects--event-timers))))

(defun +agent-shell-projects-remove-agent (project agent-id)
  "Unenrol AGENT-ID from PROJECT while keeping its shell alive."
  (+agent-shell-projects-assert-active project)
  (let* ((agent (+agent-shell-projects-agent project agent-id))
         (shell (+agent-shell-projects-shell agent)))
    (+agent-shell-projects--cancel-pending project agent-id)
    (+agent-shell-projects--cancel-event project)
    (+agent-shell-projects--detach agent-id)
    (+agent-shell-projects--set project :agents
				(delq agent (plist-get project :agents)))
    (+agent-shell-projects--set project :unenrolled-agents
				(cons agent (plist-get project :unenrolled-agents)))
    (when (equal agent-id (plist-get project :author))
      (+agent-shell-projects--set project :author nil))
    (when (buffer-live-p shell)
      (with-current-buffer shell
        (when (equal +agent-shell-projects--project-id (plist-get project :id))
          (setq +agent-shell-projects--project-id nil
                +agent-shell-projects--side-question nil))))
    (+agent-shell-projects--changed project nil "user")))

(defun +agent-shell-projects--untrack-artefact (project artefact)
  "Remove PROJECT ARTEFACT's live source tracking."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (equal +agent-shell-projects--source-project (plist-get project :id))
        (setq +agent-shell-projects--source-artefacts
              (remove (plist-get artefact :id) +agent-shell-projects--source-artefacts))
        (unless +agent-shell-projects--source-artefacts
          (setq +agent-shell-projects--source-project nil)
          (remove-hook 'after-save-hook #'+agent-shell-projects--source-saved t)))
      (when (and buffer-file-name
                 (equal +agent-shell-projects--project-id (plist-get project :id))
                 (equal +agent-shell-projects--artefact-id (plist-get artefact :id)))
        (remove-hook 'after-save-hook #'+agent-shell-projects--code-source-saved t)
        (+agent-shell-projects-artefact-mode -1)
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'project-annotation-id)
            (delete-overlay overlay)))
        (setq +agent-shell-projects--project-id nil
              +agent-shell-projects--artefact-id nil
              +agent-shell-projects--version-id nil))
      (when (and (derived-mode-p '+agent-shell-projects-sidebar-mode)
                 (equal +agent-shell-projects--project-id (plist-get project :id))
                 (equal +agent-shell-projects--artefact-id (plist-get artefact :id)))
        (setq +agent-shell-projects--view-buffer nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Artefact unenrolled.\n\n")
          (when (plist-get artefact :current)
            (insert-text-button
             "Open saved review history" 'follow-link t
             'action (lambda (_button)
                       (+agent-shell-projects-open project (plist-get artefact :id))))
            (insert "\n")))))))

(defun +agent-shell-projects--delete-source (project artefact)
  "Delete document ARTEFACT's saved source after retaining its last version."
  (when (equal (plist-get artefact :kind) "code")
    (user-error "Unenrol Git workspaces; delete individual files in source buffers"))
  (pcase-let ((`(,buffer ,start ,end) (+agent-shell-projects-source artefact)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (user-error "Save or discard source edits before deleting this artefact"))
      (+agent-shell-projects-capture project artefact "user")
      (if (plist-get artefact :org-id)
          (let ((+agent-shell-projects--source-artefacts
                 (remove (plist-get artefact :id) +agent-shell-projects--source-artefacts)))
            (save-restriction
              (widen)
              (atomic-change-group
                (delete-region start end)
                (save-buffer))))
        (delete-file buffer-file-name delete-by-moving-to-trash)))))

(defun +agent-shell-projects-remove-artefact (project artefact-id &optional delete-source)
  "Unenrol ARTEFACT-ID from PROJECT, optionally DELETE-SOURCE too.
Saved review history and annotations remain available to existing references."
  (+agent-shell-projects-assert-active project)
  (let ((artefact (+agent-shell-projects-artefact project artefact-id)))
    (+agent-shell-projects-assert-active project artefact)
    (when delete-source (+agent-shell-projects--delete-source project artefact))
    (+agent-shell-projects--set artefact :unenrolled (float-time))
    (+agent-shell-projects--set project :artefacts
				(delq artefact (plist-get project :artefacts)))
    (+agent-shell-projects--set project :unenrolled-artefacts
				(cons artefact (plist-get project :unenrolled-artefacts)))
    (when (equal artefact-id (plist-get project :proposal))
      (+agent-shell-projects--set project :proposal nil))
    (+agent-shell-projects--cancel-pending project)
    (+agent-shell-projects--cancel-event project)
    (+agent-shell-projects--untrack-artefact project artefact)
    (when (fboundp '+agent-shell-projects-stop-project-workers)
      (+agent-shell-projects-stop-project-workers project artefact-id))
    (+agent-shell-projects--changed project nil "user")))

(defun +agent-shell-projects-set-archived (project archived)
  "Set PROJECT's ARCHIVED state, suspending extension activity when archived."
  (+agent-shell-projects--set project :archived (and archived (float-time)))
  (when archived
    (+agent-shell-projects--cancel-pending project)
    (+agent-shell-projects--cancel-event project)
    (dolist (agent (plist-get project :agents))
      (+agent-shell-projects--detach (plist-get agent :id)))
    (when (fboundp '+agent-shell-projects-stop-project-workers)
      (+agent-shell-projects-stop-project-workers project)))
  (+agent-shell-projects--changed project nil "user")
  project)

(defun +agent-shell-projects--manage-selection (target kind)
  "Return (PROJECT . ID) from TARGET or a current enrolment of KIND."
  (let* ((context (and target (+agent-shell-projects--target-context target)))
         (project (if context (+agent-shell-projects-load (plist-get context :project))
                    (+agent-shell-projects--project)))
         (id (or (plist-get context kind)
                 (and (eq kind :artefact) +agent-shell-projects--artefact-id)
                 (let ((choices
                        (mapcar (lambda (item)
                                  (cons (format "%s [%s]"
                                                (or (plist-get item :name)
                                                    (plist-get item :buffer) "Agent")
                                                (substring (plist-get item :id) 0 6))
                                        (plist-get item :id)))
                                (plist-get project (if (eq kind :agent) :agents :artefacts)))))
                   (unless choices (user-error "No enrolled items"))
                   (cdr (assoc (completing-read "Unenrol: " choices nil t) choices))))))
    (cons project id)))

;;;###autoload
(defun +agent-shell-projects-unenrol-agent (&optional target)
  "Detach an agent selected at TARGET or from the current project."
  (interactive)
  (pcase-let ((`(,project . ,id) (+agent-shell-projects--manage-selection target :agent)))
    (+agent-shell-projects-remove-agent project id)
    (message "Agent unenrolled; shell kept open")))

;;;###autoload
(defun +agent-shell-projects-unenrol-artefact (&optional target)
  "Detach an artefact selected at TARGET or from the current project."
  (interactive)
  (pcase-let ((`(,project . ,id) (+agent-shell-projects--manage-selection target :artefact)))
    (+agent-shell-projects-remove-artefact project id)
    (message "Artefact unenrolled; source and history kept")))

;;;###autoload
(defun +agent-shell-projects-delete-artefact (&optional target)
  "Delete the document source at TARGET and unenrol it, after confirmation."
  (interactive)
  (pcase-let* ((`(,project . ,id) (+agent-shell-projects--manage-selection target :artefact))
               (artefact (+agent-shell-projects-artefact project id)))
    (when (equal (plist-get artefact :kind) "code")
      (user-error "Unenrol Git workspaces; delete individual files in source buffers"))
    (when (yes-or-no-p (format "Delete %s%s and unenrol? "
                               (if (plist-get artefact :org-id) "the enrolled Org subtree in " "")
                               (plist-get artefact :path)))
      (+agent-shell-projects-remove-artefact project id t)
      (message "Source deleted; review history kept"))))

;;;###autoload
(defun +agent-shell-projects-archive-project ()
  "Archive the current project, retaining its sources and review history."
  (interactive)
  (+agent-shell-projects-set-archived (+agent-shell-projects--project) t)
  (message "Project archived; existing agent shells remain open"))

;;;###autoload
(defun +agent-shell-projects-restore-project (&optional no-display)
  "Restore an archived project, opening its home unless NO-DISPLAY."
  (interactive)
  (let* ((choices (mapcar (lambda (p)
                            (cons (format "%s [%s]" (plist-get p :name)
                                          (substring (plist-get p :id) 0 6)) p))
                          (seq-filter (lambda (p) (plist-get p :archived))
                                      (+agent-shell-projects-list t)))))
    (unless choices (user-error "No archived projects"))
    (let ((project (cdr (assoc (completing-read "Restore project: " choices nil t) choices))))
      (+agent-shell-projects-set-archived project nil)
      (+agent-shell-projects-mode 1)
      (unless no-display (+agent-shell-projects project))
      project)))

(provide '+agent-shell-projects-manage)
;;; +agent-shell-projects-manage.el ends here
