;;; +life.el --- Life knowledge base helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; Capture, navigation, and zoom views for the life knowledge base.
;; Built on vulpea. Loaded via `with-eval-after-load' in mod-org.el.

;;; Code:

(eval-when-compile
  (require 'vulpea)
  (require 'vulpea-db))

;;; Agenda

(defun +life/agenda-files ()
  "Return list of org files for agenda: active initiatives + inbox."
  (delete-dups
   (cons org-default-notes-file
         (mapcar #'vulpea-note-path
                 (seq-filter #'+life/--active-p
                             (vulpea-db-query-by-tags-every '("initiative")))))))

(defvar +life/--initiative-files-cache nil
  "Cached list of initiative file paths.")

(defvar +life/--initiative-files-cache-time nil
  "Timestamp of last cache fill.")

(defconst +life/--initiative-files-cache-ttl 300
  "Cache TTL in seconds.")

(defun +life/initiative-files-cached ()
  "Return initiative files from cache, refreshing if stale (5-minute TTL)."
  (when (or (null +life/--initiative-files-cache-time)
            (> (float-time (time-subtract (current-time)
                                          +life/--initiative-files-cache-time))
               +life/--initiative-files-cache-ttl))
    (setq +life/--initiative-files-cache (+life/agenda-files)
          +life/--initiative-files-cache-time (current-time)))
  +life/--initiative-files-cache)

;;;###autoload
(defun +life/invalidate-agenda-cache ()
  "Force refresh of the initiative files cache."
  (interactive)
  (setq +life/--initiative-files-cache nil
        +life/--initiative-files-cache-time nil)
  (message "Agenda files cache invalidated"))

(defun +life/agenda-files-update (&rest _)
  "Set `org-agenda-files' from cached initiative files."
  (setq org-agenda-files (+life/initiative-files-cached)))

;;;###autoload
(defun +life/refresh-agenda-files ()
  "Refresh agenda files by invalidating cache and updating."
  (interactive)
  (+life/invalidate-agenda-cache)
  (+life/agenda-files-update)
  (message "Agenda files: %d entries" (length org-agenda-files)))

;;; Helpers

(defun +life/--active-p (note)
  "Return non-nil if NOTE does not have a terminal status."
  (not (member (vulpea-note-meta-get note "status")
               '("complete" "abandoned"))))

(defun +life/--children-of (parent-id &optional level-tag)
  "Find notes whose parent meta links to PARENT-ID.
Optionally filter to those tagged with LEVEL-TAG."
  (seq-filter
   (lambda (note)
     (when-let* ((pid (vulpea-note-meta-get note "parent" 'link)))
       (and (string= pid parent-id)
            (or (null level-tag)
                (vulpea-note-tagged-all-p note level-tag)))))
   (vulpea-db-query-by-links-some (list parent-id))))

;;; Capture commands (Phase 3.2)

;;;###autoload
(defun +life/capture-initiative ()
  "Create a new initiative (pillar, goal, or project) note."
  (interactive)
  (let* ((title (read-string "Initiative title: "))
         (level (completing-read "Level: " '("pillar" "goal" "project") nil t))
         (parent (unless (string= level "pillar")
                   (let ((sel (vulpea-select "Parent initiative"
                                             :filter-fn (lambda (n)
                                                          (and (vulpea-note-id n)
                                                               (vulpea-note-tagged-all-p n "initiative")))
                                             :require-match t)))
                     (when (vulpea-note-id sel) sel))))
         (status (completing-read "Status: "
                                  '("not started" "in progress" "blocked")
                                  nil t nil nil "not started"))
         (meta (append
                `(("level" . ,level)
                  ("status" . ,status))
                (when parent
                  `(("parent" . ,parent)))))
         (body (pcase level
                 ("pillar" "* Vision\n\n* Active Goals\n")
                 ("goal" "* Description\n\n* Artefacts\n\n* Timeline\n")
                 ("project" "* Description\n\n* Tasks\n** TODO Fill out description\n")))
         (note (vulpea-create title nil
                              :tags (list level "initiative")
                              :meta meta
                              :body body)))
    (vulpea-visit note)
    (+life/refresh-agenda-files)))

;;;###autoload
(defun +life/capture-person ()
  "Create a new person note."
  (interactive)
  (let* ((name (read-string "Person name: "))
         (handle (concat "@" (replace-regexp-in-string " " "" name)))
         (org-sel (let ((sel (vulpea-select "Organisation"
                                            :filter-fn (lambda (n)
                                                         (and (vulpea-note-id n)
                                                              (vulpea-note-tagged-all-p n "org")))
                                            :require-match nil)))
                    (when (vulpea-note-id sel) sel)))
         (role (read-string "Role: "))
         (tags (delete-dups (list "person" handle)))
         (meta (append
                '(("type" . "person"))
                (unless (string-empty-p role)
                  `(("role" . ,role)))
                (when org-sel
                  `(("org" . ,org-sel)))))
         (note (vulpea-create name nil
                              :tags tags
                              :meta meta
                              :body "* Notes\n\n* Events\n")))
    (vulpea-visit note)))

;;;###autoload
(defun +life/capture-org ()
  "Create a new org/team note."
  (interactive)
  (let* ((name (read-string "Org/Team name: "))
         (parent-org (let ((sel (vulpea-select "Parent org"
                                               :filter-fn (lambda (n)
                                                            (and (vulpea-note-id n)
                                                                 (vulpea-note-tagged-all-p n "org")))
                                               :require-match nil)))
                       (when (vulpea-note-id sel) sel)))
         (meta (append
                '(("type" . "org"))
                (when parent-org
                  `(("parent-org" . ,parent-org)))))
         (note (vulpea-create name nil
                              :tags '("org")
                              :meta meta
                              :body "* Description\n\n* People\n\n* Notes\n")))
    (vulpea-visit note)))

;;;###autoload
(defun +life/capture-idea ()
  "Create a new idea note."
  (interactive)
  (let* ((title (read-string "Idea title: "))
         (note (vulpea-create title nil
                              :tags '("idea" "seedling")
                              :meta '(("type" . "idea"))
                              :body "* Abstract\n\n* Notes\n\n* Related reading\n")))
    (vulpea-visit note)))

;;; Zoom views (Phase 3.3)

;;;###autoload
(defun +life/view-pillars ()
  "Select and visit a pillar note."
  (interactive)
  (let* ((pillars (vulpea-db-query-by-tags-every '("pillar")))
         (sel (vulpea-select-from "Pillar" pillars :require-match t)))
    (when (vulpea-note-id sel)
      (vulpea-visit sel))))

;;;###autoload
(defun +life/view-goals ()
  "Select and visit an active goal."
  (interactive)
  (let* ((goals (seq-filter #'+life/--active-p
                            (vulpea-db-query-by-tags-every '("goal"))))
         (sel (vulpea-select-from "Goal" goals :require-match t)))
    (when (vulpea-note-id sel)
      (vulpea-visit sel))))

;;;###autoload
(defun +life/view-projects ()
  "Select and visit an active project."
  (interactive)
  (let* ((projects (seq-filter #'+life/--active-p
                               (vulpea-db-query-by-tags-every '("project"))))
         (sel (vulpea-select-from "Project" projects :require-match t)))
    (when (vulpea-note-id sel)
      (vulpea-visit sel))))

;;;###autoload
(defun +life/view-today ()
  "Show today's journal alongside today's agenda."
  (interactive)
  (delete-other-windows)
  (vulpea-journal-today)
  (let ((journal-buf (current-buffer)))
    (let ((org-agenda-window-setup 'other-window))
      (org-agenda nil "d"))
    (pop-to-buffer journal-buf)))

;;; Navigation (Phase 3.4)

;;;###autoload
(defun +life/go-to-parent ()
  "Navigate to the parent note of the current buffer."
  (interactive)
  (if-let* ((_id (org-entry-get (point-min) "ID"))
            (parent (vulpea-buffer-meta-get "parent" 'note)))
      (vulpea-visit parent)
    (user-error "No parent link found")))

;;;###autoload
(defun +life/show-children ()
  "Show all notes parented to the current note."
  (interactive)
  (if-let* ((id (org-entry-get (point-min) "ID")))
      (let ((children (+life/--children-of id)))
        (if children
            (let ((sel (vulpea-select-from "Children" children :require-match t)))
              (when (vulpea-note-id sel)
                (vulpea-visit sel)))
          (user-error "No children found")))
    (user-error "Current buffer has no ID property")))

;;;###autoload
(defun +life/set-initiative-status ()
  "Set the status of the current initiative buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (if-let* ((id (org-entry-get (point-min) "ID"))
            (note (vulpea-db-get-by-id id))
            (_ (vulpea-note-tagged-all-p note "initiative")))
      (let* ((statuses '("not started" "in progress" "blocked" "complete" "abandoned"))
             (current (vulpea-buffer-meta-get "status" 'string))
             (new (completing-read (format "Status [%s]: " (or current "none"))
                                   statuses nil t nil nil current)))
        (vulpea-buffer-meta-set "status" new)
        (when (member new '("complete" "abandoned"))
          (+life/invalidate-agenda-cache))
        (message "Status: %s → %s" (or current "none") new))
    (user-error "Not in an initiative buffer")))

;;;###autoload
(defun +life/add-stakeholder ()
  "Add a stakeholder to the current initiative.
Prompts for a role and a person note, then sets the meta key."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (if-let* ((id (org-entry-get (point-min) "ID"))
            (note (vulpea-db-get-by-id id))
            (_ (vulpea-note-tagged-all-p note "initiative")))
      (let* ((roles '("owner" "coach" "sponsor" "advisor" "collaborator"))
             (role (completing-read "Role: " roles nil t))
             (people (vulpea-db-query-by-tags-every '("person")))
             (person (vulpea-select-from "Person" people :require-match t)))
        (when (vulpea-note-id person)
          (if (string= role "collaborator")
              (let ((existing (vulpea-buffer-meta-get-list "collaborator" 'note)))
                (vulpea-buffer-meta-set role (append existing (list person))))
            (vulpea-buffer-meta-set role person))
          (message "Added %s as %s" (vulpea-note-title person) role)))
    (user-error "Not in an initiative buffer")))

;;;###autoload
(defun +life/remove-stakeholder ()
  "Remove a stakeholder from the current initiative."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (if-let* ((id (org-entry-get (point-min) "ID"))
            (note (vulpea-db-get-by-id id))
            (_ (vulpea-note-tagged-all-p note "initiative")))
      (let* ((single-roles '("owner" "coach" "sponsor" "advisor"))
             (entries
              (append
               (seq-filter
                #'identity
                (mapcar (lambda (role)
                          (when-let* ((person (vulpea-buffer-meta-get role 'note)))
                            (cons role person)))
                        single-roles))
               (mapcar (lambda (person)
                         (cons "collaborator" person))
                       (vulpea-buffer-meta-get-list "collaborator" 'note))))
             (candidates
              (mapcar (lambda (entry)
                        (cons (format "%s: %s" (car entry) (vulpea-note-title (cdr entry)))
                              entry))
                      entries)))
        (if (null candidates)
            (user-error "No stakeholders to remove")
          (let* ((choice (completing-read "Remove stakeholder: "
                                          (mapcar #'car candidates) nil t))
                 (entry (alist-get choice candidates nil nil #'equal))
                 (role (car entry))
                 (person (cdr entry)))
            (if (string= role "collaborator")
                (let ((remaining (seq-remove
                                  (lambda (p)
                                    (string= (vulpea-note-id p)
                                             (vulpea-note-id person)))
                                  (vulpea-buffer-meta-get-list "collaborator" 'note))))
                  (vulpea-buffer-meta-remove "collaborator")
                  (when remaining
                    (vulpea-buffer-meta-set "collaborator" remaining)))
              (vulpea-buffer-meta-remove role))
            (message "Removed %s as %s" (vulpea-note-title person) role))))
    (user-error "Not in an initiative buffer")))

;;;###autoload
(defun +life/show-stakeholders ()
  "Show stakeholders of the current initiative."
  (interactive)
  (if-let* ((_id (org-entry-get (point-min) "ID")))
      (let* ((roles '("owner" "coach" "sponsor" "advisor" "collaborator"))
             (stakeholders
              (seq-filter #'identity
                          (mapcar (lambda (role)
                                    (vulpea-buffer-meta-get role 'note))
                                  roles))))
        (if stakeholders
            (let ((sel (vulpea-select-from "Stakeholder" stakeholders :require-match t)))
              (when (vulpea-note-id sel)
                (vulpea-visit sel)))
          (user-error "No stakeholders found")))
    (user-error "Current buffer has no ID property")))

;;;###autoload
(defun +life/person-initiatives ()
  "Show all initiatives referencing the current person note."
  (interactive)
  (if-let* ((id (org-entry-get (point-min) "ID")))
      (let* ((linked (vulpea-db-query-by-links-some (list id)))
             (initiatives (seq-filter
                           (lambda (n)
                             (vulpea-note-tagged-all-p n "initiative"))
                           linked)))
        (if initiatives
            (let ((sel (vulpea-select-from "Initiative" initiatives :require-match t)))
              (when (vulpea-note-id sel)
                (vulpea-visit sel)))
          (user-error "No initiatives reference this person")))
    (user-error "Current buffer has no ID property")))

;;; Agenda helpers

(defun +life/agenda-category (&optional len)
  "Get agenda category for the current item, padded/truncated to LEN."
  (let* ((len (or len 24))
         (file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result (or (if (and title (string-equal category file-name))
                         title
                       category)
                     ""))
         (trimmed (if (> (length result) len)
                      (truncate-string-to-width result (- len 1))
                    result)))
    (string-pad trimmed len)))

;;;###autoload
(defun +life/refile ()
  "Refile current entry to an initiative's Tasks heading."
  (interactive)
  (let* ((initiatives (vulpea-db-query-by-tags-every '("initiative")))
         (sel (vulpea-select-from "Refile to" initiatives :require-match t)))
    (when (vulpea-note-id sel)
      (let* ((file (vulpea-note-path sel))
             (pos (with-current-buffer (find-file-noselect file)
                    (org-find-exact-headline-in-buffer "Tasks"))))
        (if pos
            (org-refile nil nil (list "Tasks" file nil pos))
          (user-error "No '* Tasks' heading found in %s"
                      (vulpea-note-title sel)))))))

;;;###autoload
(defun +life/agenda-refile ()
  "Refile current agenda item to an initiative's Tasks heading."
  (interactive)
  (let* ((initiatives (vulpea-db-query-by-tags-every '("initiative")))
         (sel (vulpea-select-from "Refile to" initiatives :require-match t)))
    (when (vulpea-note-id sel)
      (let* ((file (vulpea-note-path sel))
             (pos (with-current-buffer (find-file-noselect file)
                    (org-find-exact-headline-in-buffer "Tasks"))))
        (if pos
            (org-agenda-refile nil (list "Tasks" file nil pos))
          (user-error "No '* Tasks' heading found in %s"
                      (vulpea-note-title sel)))))))

;;;###autoload
(defun +life/agenda-person ()
  "Show agenda items related to a person."
  (interactive)
  (let* ((people (vulpea-db-query-by-tags-every '("person")))
         (sel (vulpea-select-from "Person" people :require-match t)))
    (when (vulpea-note-id sel)
      (let* ((tags (vulpea-note-tags sel))
             (handle (seq-find (lambda (tag) (string-prefix-p "@" tag)) tags)))
        (if handle
            (org-tags-view nil handle)
          (user-error "Person %s has no handle tag"
                      (vulpea-note-title sel)))))))

;;; Ingress — HTTP client for the Life BEAM backend

(defcustom +life/ingress-url "http://127.0.0.1:4848"
  "Base URL for the Life ingress HTTP endpoint."
  :type 'string
  :group 'life)

(defun +life/signal (type data &optional callback)
  "Send signal TYPE with DATA to the Life BEAM via HTTP POST.
CALLBACK is the notification mechanism: \"emacs\" (default) or nil.
Runs asynchronously; result is delivered via `+life/ingress-result'."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `(:type ,type
             :data ,data
             :source "/emacs"
             :callback ,(or callback "emacs")))
          'utf-8)))
    (url-retrieve
     (concat +life/ingress-url "/signal")
     (lambda (status &rest _)
       (if-let* ((err (plist-get status :error)))
           (message "[Life] Signal failed: %s" err)
         (goto-char url-http-end-of-headers)
         (let ((body (json-parse-buffer :object-type 'alist)))
           (message "[Life] Signal accepted: %s (id: %s)"
                    (alist-get 'type body)
                    (alist-get 'request_id body)))))
     nil t)))

;;;###autoload
(defun +life/summarize ()
  "Summarize the current org buffer via the Life BEAM backend.
Sends an async request; the abstract is inserted when the result arrives."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (+life/signal "enrichment.summarize"
                `(:file_path ,buffer-file-name))
  (message "[Life] Summarize requested for %s"
           (file-name-nondirectory buffer-file-name)))

(defun +life/ingress-result (plist)
  "Handle a result callback from the Life BEAM ingress layer.
PLIST has keys :request-id, :type, :status, :detail.
DETAIL is either a string or a plist with :ops."
  (let ((type (plist-get plist :type))
        (status (plist-get plist :status))
        (detail (plist-get plist :detail)))
    (pcase status
      ('completed
       (message "[Life] %s completed" type)
       (run-hook-with-args '+life/ingress-result-functions type status detail))
      ('failed
       (message "[Life] %s failed: %s" type detail)
       (run-hook-with-args '+life/ingress-result-functions type status detail))
      (_ (message "[Life] %s: %s" type status)))))

(defvar +life/ingress-result-functions nil
  "Abnormal hook run when an ingress result arrives.
Each function receives TYPE, STATUS, and DETAIL.")

;;; Ingress — operation dispatcher

;; Clean up stale hooks/advice from previous versions
(remove-hook '+life/ingress-result-functions #'+life/--on-summarize-complete)
(advice-remove 'ask-user-about-supersession-threat '+life/auto-revert)

(defun +life/--on-enrichment-complete (_type status detail)
  "Execute write operations from enrichment result.
STATUS is completed or failed.  DETAIL is a plist with :ops when
the action produced write operations, or a string otherwise."
  (message "[Life] debug: on-enrichment-complete status=%s detail-type=%s detail=%S"
           status (type-of detail) detail)
  (when (eq status 'completed)
    (when-let* ((ops (and (listp detail) (plist-get detail :ops))))
      (message "[Life] debug: scheduling %d ops" (length ops))
      ;; Defer to avoid running inside emacsclient eval context
      (run-with-timer 0 nil #'+life/--execute-ops ops))))

(defun +life/--execute-ops (ops)
  "Execute write OPS on org buffers, saving each modified file once."
  (message "[Life] debug: execute-ops called with %d ops" (length ops))
  (condition-case err
      (let ((modified-files nil))
        (dolist (op ops)
          (let ((file (plist-get op :file)))
            (message "[Life] debug: executing op %s on %s" (plist-get op :op) file)
            (vulpea-utils-with-file file
                                    (+life/--dispatch-op op))
            (cl-pushnew file modified-files :test #'string=)))
        (dolist (file modified-files)
          (when-let* ((buf (find-buffer-visiting file)))
            (with-current-buffer buf
              (let ((inhibit-message t))
                (save-buffer)))))
        (message "[Life] Applied %d op(s) across %d file(s)"
                 (length ops) (length modified-files)))
    (error (message "[Life] ERROR in execute-ops: %S" err))))

(defun +life/--dispatch-op (op)
  "Dispatch a single write OP in the current org buffer."
  (pcase (plist-get op :op)
    ('prepend-section  (+life/--op-prepend-section op))
    ('append-section   (+life/--op-append-section op))
    ('set-meta         (+life/--op-set-meta op))
    ('add-link         (+life/--op-add-link op))
    ('set-todo-state   (+life/--op-set-todo-state op))
    ('append-to-heading (+life/--op-append-to-heading op))
    ('insert-heading   (+life/--op-insert-heading op))
    (other (message "[Life] Unknown op: %s" other))))

(add-hook '+life/ingress-result-functions #'+life/--on-enrichment-complete)

;;; Ingress — operation handlers

(defun +life/--op-prepend-section (op)
  "Insert or replace a section before the first heading.
OP plist keys: :heading, :content."
  (let ((heading (plist-get op :heading))
        (content (plist-get op :content)))
    (+life/--remove-section heading)
    (goto-char (point-min))
    (if (re-search-forward "^\\* " nil t)
        (goto-char (line-beginning-position))
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (insert "* " heading "\n" content "\n")))

(defun +life/--op-append-section (op)
  "Insert or replace a section at the end of the buffer.
OP plist keys: :heading, :content."
  (let ((heading (plist-get op :heading))
        (content (plist-get op :content)))
    (+life/--remove-section heading)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " heading "\n" content "\n")))

(defun +life/--op-set-meta (op)
  "Set a bare description-list metadata entry.
OP plist keys: :key, :value."
  (let ((key (plist-get op :key))
        (value (plist-get op :value)))
    (vulpea-buffer-meta-set key value)))

(defun +life/--op-add-link (op)
  "Replace first plain-text occurrence with an org link.
OP plist keys: :plain-text, :link.
Skips occurrences inside existing [[...]] links."
  (let ((plain (plist-get op :plain-text))
        (link (plist-get op :link)))
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found)
                    (search-forward plain nil t))
          ;; Check we're not inside a link
          (unless (save-excursion
                    (goto-char (match-beginning 0))
                    (let ((ppss (syntax-ppss)))
                      (or (nth 3 ppss) (nth 4 ppss)))
                    ;; Also check for enclosing [[...]]
                    (save-excursion
                      (and (re-search-backward "\\[\\[" (line-beginning-position) t)
                           (not (re-search-forward "\\]\\]"
                                                   (match-beginning 0) t)))))
            (replace-match link t t)
            (setq found t)))))))

(defun +life/--op-set-todo-state (op)
  "Change a heading's TODO state keyword.
OP plist keys: :heading, :from, :to."
  (let ((heading (plist-get op :heading))
        (from-state (plist-get op :from))
        (to-state (plist-get op :to)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^\\(\\*+ \\)" (regexp-quote from-state)
                     " " (regexp-quote heading) "[[:space:]]*$")
             nil t)
        (replace-match (concat (match-string 1) to-state " " heading))
        ;; Add CLOSED timestamp when transitioning to DONE
        (when (string= to-state "DONE")
          (forward-line 1)
          (insert (format "CLOSED: [%s]\n"
                          (format-time-string "%Y-%m-%d %a %H:%M"))))))))

(defun +life/--op-append-to-heading (op)
  "Append content to an existing heading's body.
OP plist keys: :heading, :content."
  (let ((heading (plist-get op :heading))
        (content (plist-get op :content)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^\\*+ .*" (regexp-quote heading) "[[:space:]]*$")
             nil t)
        ;; Move to end of this subtree (before next same-or-higher heading)
        (let ((level (save-excursion
                       (goto-char (match-beginning 0))
                       (org-outline-level))))
          (if (re-search-forward
               (concat "^\\*\\{1," (number-to-string level) "\\} ")
               nil t)
              (goto-char (line-beginning-position))
            (goto-char (point-max))))
        (unless (bolp) (insert "\n"))
        (insert content "\n")))))

(defun +life/--op-insert-heading (op)
  "Insert a new heading at the end of the buffer.
OP plist keys: :level, :title, :body (optional)."
  (let ((level (plist-get op :level))
        (title (plist-get op :title))
        (body (plist-get op :body)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (make-string level ?*) " " title "\n")
    (when body
      (insert body "\n"))))

(defun +life/--remove-section (heading)
  "Remove the section with HEADING from the current org buffer.
Deletes from the heading line through to (but not including)
the next heading at the same or higher level, or end of buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\* " (regexp-quote heading) "[[:space:]]*$")
           nil t)
      (let ((beg (line-beginning-position))
            (end (if (re-search-forward "^\\* " nil t)
                     (line-beginning-position)
                   (point-max))))
        (delete-region beg end)))))

;;; Migration

(defun +life/migrate-remove-metadata-heading (file)
  "Remove the `* Metadata' heading from FILE.
Promotes description list items underneath to file-level meta,
which is where vulpea expects them."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Metadata[[:space:]]*$" nil t)
      (let ((bol (line-beginning-position)))
        (forward-line 1)
        (delete-region bol (point))
        (write-region (point-min) (point-max) file nil 'silent)
        t))))

(defun +life/migrate-remove-metadata-headings (directory)
  "Remove `* Metadata' headings from all org files in DIRECTORY.
Returns count of files modified."
  (let ((count 0))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (when (+life/migrate-remove-metadata-heading file)
        (cl-incf count)))
    (message "Migrated %d files" count)
    count))

(provide '+life)
;;; +life.el ends here
