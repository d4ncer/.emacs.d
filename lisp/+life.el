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
  (let ((org-agenda-window-setup 'other-window))
    (org-agenda nil "d")))

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
