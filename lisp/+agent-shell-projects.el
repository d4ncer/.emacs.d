;;; +agent-shell-projects.el --- Project authoring and review UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Open `+agent-shell-projects' to create or select a project.  RET visits an
;; artefact; C-c C-p/C-n browse saved versions; C-c C-e edits live source.

;;; Code:

(require '+agent-shell-projects-core)
(require 'magit-section)
(require 'button)
(declare-function markdown-mode "markdown-mode")
(declare-function diff-buffer-with-file "diff")
(declare-function +agent-shell-projects-shell "+agent-shell-projects-agent")
(declare-function +agent-shell-projects-mode "+agent-shell-projects-agent")
(autoload '+agent-shell-projects-code-edit "+agent-shell-projects-code")
(autoload '+agent-shell-projects-capture-current "+agent-shell-projects-code")
(autoload '+agent-shell-projects-add-code "+agent-shell-projects-code")
(autoload '+agent-shell-projects-review-code "+agent-shell-projects-review")
(declare-function +agent-shell-projects-render-code "+agent-shell-projects-code")
(declare-function +agent-shell-projects-choose-org-heading "+agent-shell-projects-agent")
(defvar-local +agent-shell-projects--code-view "diff")
(defvar-local +agent-shell-projects--comparison "base")

(defvar-local +agent-shell-projects--project-id nil)
(defvar-local +agent-shell-projects--artefact-id nil)
(defvar-local +agent-shell-projects--version-id nil)
(defvar-local +agent-shell-projects--observed-event 0)
(defvar-local +agent-shell-projects--show-resolved nil)
(defvar-local +agent-shell-projects--view-buffer nil)
(defvar +agent-shell-projects-sidebar-functions nil)
(defvar +agent-shell-projects-visit-functions nil)
(defvar +agent-shell-projects-view-functions nil)

(defface +agent-shell-projects-attention
  '((t :inherit warning)) "Face for new material." :group '+agent-shell-projects)

(defun +agent-shell-projects--project ()
  "Return the project associated with this buffer."
  (unless +agent-shell-projects--project-id (user-error "No project here"))
  (+agent-shell-projects-load +agent-shell-projects--project-id))

(defun +agent-shell-projects--artefact ()
  "Return this buffer's artefact."
  (+agent-shell-projects-artefact
   (+agent-shell-projects--project) +agent-shell-projects--artefact-id))

(defun +agent-shell-projects--choose-project ()
  "Choose an existing project or create one."
  (let* ((projects (+agent-shell-projects-list))
         (choices (mapcar (lambda (p)
                            (cons (format "%s [%s]" (plist-get p :name)
                                          (substring (plist-get p :id) 0 6)) p))
                          projects))
         (choice (completing-read "Project (new name to create): " choices)))
    (or (cdr (assoc choice choices))
        (+agent-shell-projects-create choice default-directory))))

(defun +agent-shell-projects--button (text target action)
  "Insert TEXT with TARGET and button ACTION."
  (insert-text-button text 'follow-link t 'project-target target
                      'action action))

(defun +agent-shell-projects--visit-button (button)
  "Visit the artefact identified by BUTTON."
  (+agent-shell-projects-open
   (+agent-shell-projects--project) (button-get button 'project-target)))

(define-derived-mode +agent-shell-projects-home-mode magit-section-mode
  "Agent Projects" "Project home with artefacts and associated agents."
  (setq-local truncate-lines t))

(defun +agent-shell-projects--render-home ()
  "Render this project's home, preserving point."
  (let* ((project (+agent-shell-projects--project))
         (inhibit-read-only t)
         (position (point)))
    (erase-buffer)
    (magit-insert-section (project-home)
      (magit-insert-heading (plist-get project :name))
      (insert "\n")
      (dolist (agent (plist-get project :agents))
        (unless (equal (plist-get agent :role) "side")
        (let ((buffer (+agent-shell-projects-shell agent)))
          (+agent-shell-projects--button
           (or (and buffer (buffer-name buffer))
               (plist-get agent :buffer) "Disconnected agent")
           buffer (lambda (button)
                    (if-let* ((buf (button-get button 'project-target))
                              ((buffer-live-p buf)))
                        (pop-to-buffer buf)
                      (user-error "Choose an author to reconnect"))))
          (insert (format " · %s\n"
                          (if (and buffer (fboundp 'agent-shell-status))
                              (condition-case nil
                                  (agent-shell-status :shell-buffer buffer)
                                (error "disconnected"))
                            "disconnected"))))))
      (when (plist-get project :agents) (insert "\n"))
      (dolist (artefact (plist-get project :artefacts))
        (magit-insert-section (artefact (plist-get artefact :id))
          (insert (if (+agent-shell-projects-needs-attention-p artefact)
                      (propertize "● " 'face '+agent-shell-projects-attention)
                    "  "))
          (+agent-shell-projects--button
           (plist-get artefact :name) (plist-get artefact :id)
           #'+agent-shell-projects--visit-button)
          (insert "\n"))))
    (goto-char (min position (point-max)))))

;;;###autoload
(defun +agent-shell-projects (&optional project)
  "Open PROJECT's home, prompting when omitted."
  (interactive)
  (+agent-shell-projects-mode 1)
  (let* ((project (or project (+agent-shell-projects--choose-project)))
         (buffer (get-buffer-create
                  (format "*Project: %s [%s]*" (plist-get project :name)
                          (substring (plist-get project :id) 0 6)))))
    (with-current-buffer buffer
      (+agent-shell-projects-home-mode)
      (setq +agent-shell-projects--project-id (plist-get project :id))
      (+agent-shell-projects--render-home))
    (pop-to-buffer buffer)))

(defun +agent-shell-projects-enrol-file (file name &optional subtree)
  "Add FILE named NAME to the current project, optionally its SUBTREE."
  (interactive
   (let ((file (read-file-name "Artefact: " nil nil t)))
     (list file (read-string "Name: " (file-name-nondirectory file))
           current-prefix-arg)))
  (let* ((project (+agent-shell-projects--project))
         (_ (when subtree
              (let ((heading (+agent-shell-projects-choose-org-heading file)))
                (unless (markerp heading) (user-error "Choose an existing subtree to enrol"))
                (with-current-buffer (marker-buffer heading) (goto-char heading)))))
         (artefact (+agent-shell-projects-enrol project file name subtree)))
    (+agent-shell-projects-open project (plist-get artefact :id))))

(defun +agent-shell-projects--modeline ()
  "Describe this artefact's saved version."
  (when +agent-shell-projects--artefact-id
    (let* ((artefact (+agent-shell-projects--artefact))
           (history (plist-get artefact :history))
           (index (cl-position +agent-shell-projects--version-id history
                               :key (lambda (v) (plist-get v :id))
                               :test #'equal)))
      (format " Project v%s%s" (if index (1+ index) "?")
              (if (equal +agent-shell-projects--version-id
                         (plist-get artefact :current)) "" " · newer available")))))

(defvar +agent-shell-projects-artefact-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") #'+agent-shell-projects-previous)
    (define-key map (kbd "C-c C-n") #'+agent-shell-projects-next)
    (define-key map (kbd "C-c C-g") #'+agent-shell-projects-current)
    (define-key map (kbd "C-c C-e") #'+agent-shell-projects-edit)
    (define-key map (kbd "C-c C-h") #'+agent-shell-projects-home)
    map))

(define-minor-mode +agent-shell-projects-artefact-mode
  "Review a stable project artefact while authoring continues."
  :lighter (:eval (+agent-shell-projects--modeline))
  :keymap +agent-shell-projects-artefact-mode-map)

(define-derived-mode +agent-shell-projects-sidebar-mode magit-section-mode
  "Project Comments" "Comments and agent activity for an artefact.")

(defun +agent-shell-projects-sidebar (&optional view no-display)
  "Show the annotation sidebar for VIEW, defaulting to this buffer."
  (let* ((view (or view (current-buffer)))
         (project (with-current-buffer view (+agent-shell-projects--project)))
         (artefact (with-current-buffer view (+agent-shell-projects--artefact)))
         (buffer (get-buffer-create
                  (format "*Comments: %s*" (plist-get artefact :id)))))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-projects-sidebar-mode)
        (+agent-shell-projects-sidebar-mode))
      (setq +agent-shell-projects--project-id (plist-get project :id)
            +agent-shell-projects--artefact-id (plist-get artefact :id)
            +agent-shell-projects--version-id
            (buffer-local-value '+agent-shell-projects--version-id view))
      (setq +agent-shell-projects--view-buffer view)
      (let ((inhibit-read-only t) (position (point)))
        (erase-buffer)
        (when (fboundp '+agent-shell-projects-insert-job-status)
          (+agent-shell-projects-insert-job-status project artefact))
        (when (plist-get artefact :capture-error)
          (insert (format "Capture failed: %s\nRefresh to retry.\n\n"
                          (plist-get artefact :capture-error))))
        (when (plist-get artefact :missing)
          (insert "Source file is missing. Earlier contents remain in history.\n\n"))
        (when (plist-get artefact :conflict)
          (insert-text-button
           "Source conflict · inspect differences" 'follow-link t
           'action (lambda (_button)
                     (require 'diff)
                     (pcase-let ((`(,source ,_ ,_) (+agent-shell-projects-source artefact)))
                       (diff-buffer-with-file source))))
          (insert "\n\n"))
        (run-hook-with-args '+agent-shell-projects-sidebar-functions
                            project artefact view)
        (when (get-buffer-window view t)
          (with-current-buffer view
            (setq +agent-shell-projects--observed-event (plist-get artefact :event))))
        (goto-char (min position (point-max)))))
    (unless no-display
      (display-buffer-in-side-window
       buffer '((side . right) (slot . 1) (window-width . 0.33))))))

(defun +agent-shell-projects-open (project artefact-id &optional version-id)
  "Open ARTEFACT-ID in PROJECT at VERSION-ID, defaulting to current."
  (let* ((artefact (+agent-shell-projects-artefact project artefact-id))
         (version (if version-id
                      (+agent-shell-projects-version artefact version-id)
                    (if (equal (plist-get artefact :kind) "code")
                        (when (plist-get artefact :current)
                          (+agent-shell-projects-version artefact (plist-get artefact :current)))
                      (+agent-shell-projects-capture project artefact))))
         (buffer (get-buffer-create
                  (format "*Artefact: %s [%s]*" (plist-get artefact :name)
                          (substring artefact-id 0 6)))))
    (unless (run-hook-with-args-until-success
             '+agent-shell-projects-visit-functions project artefact version)
      (with-current-buffer buffer
        (let ((inhibit-read-only t) (position (point)))
          (erase-buffer)
          (insert (+agent-shell-projects-content version))
          (if (member (plist-get artefact :kind) '("org" "org-heading"))
              (progn (require 'org) (org-mode))
            (if (require 'markdown-mode nil t) (markdown-mode) (text-mode)))
          (setq-local default-directory
                      (file-name-directory (plist-get artefact :path)))
          (setq +agent-shell-projects--project-id (plist-get project :id)
                +agent-shell-projects--artefact-id artefact-id
                +agent-shell-projects--version-id (plist-get version :id)
                +agent-shell-projects--observed-event
                (plist-get artefact :event))
          (+agent-shell-projects-artefact-mode 1)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (goto-char (min position (point-max)))))
      (pop-to-buffer buffer)
      (run-hook-with-args '+agent-shell-projects-view-functions
                          project artefact version)
      (+agent-shell-projects-sidebar buffer))
    (if (equal (plist-get artefact :kind) "code") (current-buffer) buffer)))

(defun +agent-shell-projects--history (offset)
  "Move OFFSET positions in this artefact's saved history."
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--artefact))
         (history (plist-get artefact :history))
         (index (cl-position +agent-shell-projects--version-id history
                             :key (lambda (v) (plist-get v :id)) :test #'equal))
         (next (+ (or index 0) offset)))
    (unless (< -1 next (length history)) (user-error "End of saved history"))
    (if (equal (plist-get artefact :kind) "code")
        (+agent-shell-projects-render-code project artefact (nth next history)
                                           +agent-shell-projects--code-view
                                           +agent-shell-projects--comparison)
      (+agent-shell-projects-open
       project (plist-get artefact :id) (plist-get (nth next history) :id)))))

(defun +agent-shell-projects-previous ()
  "Preview the previous saved version."
  (interactive) (+agent-shell-projects--history -1))

(defun +agent-shell-projects-next ()
  "Preview the next saved version."
  (interactive) (+agent-shell-projects--history 1))

(defun +agent-shell-projects-current ()
  "Capture and show this artefact's current source."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--artefact)))
    (if (equal (plist-get artefact :kind) "code")
        (+agent-shell-projects-capture-current)
      (+agent-shell-projects-open project +agent-shell-projects--artefact-id))))

(defun +agent-shell-projects-home ()
  "Return to the current project's home."
  (interactive) (+agent-shell-projects (+agent-shell-projects--project)))

(defun +agent-shell-projects-edit ()
  "Visit this artefact's live source in its ordinary editing mode."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (artefact (+agent-shell-projects--artefact))
         (offset (- (point) (point-min)))
         (saved (+agent-shell-projects-content
                 (+agent-shell-projects-version
                  artefact +agent-shell-projects--version-id))))
    (if (equal (plist-get artefact :kind) "code")
        (+agent-shell-projects-code-edit)
    (pcase-let ((`(,buffer ,start ,end) (+agent-shell-projects-source artefact)))
      (pop-to-buffer buffer)
      (save-restriction
        (widen)
        (unless (equal saved (buffer-substring-no-properties start end))
          (message "Source has advanced; editing its current contents")
          (setq offset 0))
        (goto-char (min end (+ start offset))))
      (+agent-shell-projects-track-source project artefact)
      (setq +agent-shell-projects--project-id (plist-get project :id)
            +agent-shell-projects--artefact-id (plist-get artefact :id)
            +agent-shell-projects--version-id (plist-get artefact :current))
      (+agent-shell-projects-artefact-mode 1)
      (run-hook-with-args '+agent-shell-projects-view-functions project artefact
                          (+agent-shell-projects-version
                           artefact +agent-shell-projects--version-id))
      (+agent-shell-projects-sidebar buffer)))))

(defun +agent-shell-projects-refresh ()
  "Capture project source and update home metadata."
  (interactive)
  (let ((project (+agent-shell-projects--project)))
    (dolist (artefact (plist-get project :artefacts))
      (+agent-shell-projects-capture project artefact))
    (if (derived-mode-p '+agent-shell-projects-home-mode)
        (+agent-shell-projects--render-home)
      (+agent-shell-projects-current))))

(defvar +agent-shell-projects--ui-pending (make-hash-table :test #'equal))
(defvar +agent-shell-projects--ui-timer nil)
(defvar-local +agent-shell-projects--ui-dirty nil)

(defun +agent-shell-projects--refresh-ui (project _artefact &optional light)
  "Coalesce PROJECT's UI changes without displaying windows."
  (let* ((id (plist-get project :id))
         (old (gethash id +agent-shell-projects--ui-pending)))
    (puthash id (list :project project :decorate (or (plist-get old :decorate) (not light)))
             +agent-shell-projects--ui-pending))
  (unless (timerp +agent-shell-projects--ui-timer)
    (setq +agent-shell-projects--ui-timer
          (run-at-time 0.1 nil #'+agent-shell-projects--refresh-visible))))

(defun +agent-shell-projects--refresh-visible ()
  "Refresh visible project buffers and leave hidden buffers marked dirty."
  (setq +agent-shell-projects--ui-timer nil)
  (let ((pending +agent-shell-projects--ui-pending))
    (setq +agent-shell-projects--ui-pending (make-hash-table :test #'equal))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((update (and +agent-shell-projects--project-id
                                (gethash +agent-shell-projects--project-id pending)))
                    (project (plist-get update :project)))
          (setq +agent-shell-projects--ui-dirty t)
          (when (get-buffer-window buffer t)
            (cond
             ((derived-mode-p '+agent-shell-projects-home-mode)
              (+agent-shell-projects--render-home))
             ((and (derived-mode-p '+agent-shell-projects-sidebar-mode)
                   (buffer-live-p +agent-shell-projects--view-buffer))
              (+agent-shell-projects-sidebar +agent-shell-projects--view-buffer t))
             ((and (plist-get update :decorate)
                   +agent-shell-projects-artefact-mode +agent-shell-projects--version-id)
              (run-hook-with-args '+agent-shell-projects-view-functions project
                                  (+agent-shell-projects--artefact)
                                  (+agent-shell-projects-version
                                   (+agent-shell-projects--artefact) +agent-shell-projects--version-id))))
            (setq +agent-shell-projects--ui-dirty nil)
            (force-mode-line-update)))))))

(defun +agent-shell-projects--window-changed (frame)
  "Schedule dirty project views displayed in FRAME."
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (and +agent-shell-projects--ui-dirty +agent-shell-projects--project-id)
        (+agent-shell-projects--refresh-ui (+agent-shell-projects--project) nil)))))

(add-hook 'window-buffer-change-functions #'+agent-shell-projects--window-changed)

(add-hook '+agent-shell-projects-change-hook #'+agent-shell-projects--refresh-ui)

(define-key +agent-shell-projects-home-mode-map (kbd "g")
            #'+agent-shell-projects-refresh)
(define-key +agent-shell-projects-home-mode-map (kbd "a")
            #'+agent-shell-projects-enrol-file)
(define-key +agent-shell-projects-home-mode-map (kbd "RET") #'push-button)

(defun +agent-shell-projects-rename ()
  "Rename this project without changing its identity."
  (interactive)
  (let* ((project (+agent-shell-projects--project))
         (name (read-string "Project name: " (plist-get project :name))))
    (when (string-empty-p (string-trim name)) (user-error "Name is empty"))
    (+agent-shell-projects--set project :name name)
    (+agent-shell-projects--changed project nil "user")))

(require '+agent-shell-projects-annotations)
(require '+agent-shell-projects-agent)
(require '+agent-shell-projects-menu)

(provide '+agent-shell-projects)
;;; +agent-shell-projects.el ends here
