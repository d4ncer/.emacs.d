;;; mod-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains org-mode and related packages configuration including:
;; - org (the main org-mode package)
;; - org-agenda (agenda views and custom commands)
;; - vulpea (note-taking library built on org-id)
;; - vulpea-journal (daily note interface)
;; - vulpea-ui (sidebar and UI enhancements)
;; - citar (citation management backed by vulpea notes)
;; - citar-embark (contextual actions on citations)
;; - org-modern (modern styling for org-mode)
;;
;; IMPORTANT: This module is designed to be deferred and must NOT be loaded eagerly.
;; The org package uses :after-call to ensure it only loads when needed.

;;; Code:

;;; Org mode

(use-package org
  :ensure t
  :after-call +first-file-hook
  :hook ((org-mode-hook . visual-line-mode)
         (org-mode-hook . auto-fill-mode)
         (org-mode-hook . (lambda () (setq-local fill-column 80))))
  :general (:keymaps 'org-mode-map :states '(normal visual motion)
                     "RET" (general-predicate-dispatch #'evil-ret
                             (org-in-regexp org-link-any-re) #'org-open-at-point)
                     "TAB" #'org-cycle
                     "S-TAB" #'org-shifttab
                     "gb" #'org-mark-ring-goto)
           (:keymaps 'org-read-date-minibuffer-local-map
                     "C-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
                     "C-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  :custom
  (org-id-locations-file (file-name-concat user-emacs-directory "var" "org-id-locations"))
  (org-directory org-directory)
  (org-default-notes-file org-default-notes-file)
  (org-startup-folded 'content)
  (org-startup-indented nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts nil)
  (org-insert-heading-respect-content t)
  (org-support-shift-select nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-link-descriptive t)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-istrstrst . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                       (sequence "SOMEDAY(o)" "|")
                       (sequence "SCHEDULE(s)" "|")))
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq org-capture-templates
        `(("i" "Inbox" entry (file ,org-default-notes-file)
           "* TODO %?\n%U\n" :prepend t)))
  (require '+org-format)
  (add-hook 'org-mode-hook #'+org-format-on-save-mode)
  (defun +org/link-dwim ()
    "DWIM link insertion/editing.
If point is on a link, prompt to change it to a vulpea note link or org link.
If no link at point, prompt to insert a vulpea note link [v] or org link [o]."
    (interactive)
    (let* ((link-bounds (when (org-in-regexp org-link-any-re)
                          (cons (match-beginning 0) (match-end 0))))
           (prompt (if link-bounds
                       "Change link: [v]ulpea note  [o]rg link"
                     "Insert link: [v]ulpea note  [o]rg link"))
           (choice (read-char-choice prompt '(?v ?o))))
      (pcase choice
        (?v
         (when link-bounds
           (delete-region (car link-bounds) (cdr link-bounds)))
         (vulpea-insert))
        (?o
         (org-insert-link)))))
  (+local-leader-set-key 'org-mode-map
    "," '(vulpea-ui-sidebar-toggle :wk "toggle sidebar")
    "c" '(org-cite-insert :wk "cite")
    "d" '(org-deadline :wk "deadline")
    "i" '(vulpea-insert :wk "insert link (note)")
    "l" '(+org/link-dwim :wk "link dwim")
    "r" '(+life/refile :wk "refile to initiative")
    "s" '(+life/summarize :wk "summarize")
    "e" '(+life/extract-entities :wk "extract entities")
    "t" '(+life/manage-todos :wk "manage todos")

    "n"  '(nil :wk "navigate")
    "np" '(+life/go-to-parent :wk "go to parent")
    "nc" '(+life/show-children :wk "show children")
    "ns" '(+life/show-stakeholders :wk "stakeholders")
    "nS" '(+life/set-initiative-status :wk "set status")
    "na" '(+life/add-stakeholder :wk "add stakeholder")
    "nr" '(+life/remove-stakeholder :wk "remove stakeholder")
    "ni" '(+life/person-initiatives :wk "person initiatives")

    "k"  '(nil :wk "clock")
    "ki" '(org-clock-in :wk "clock in")
    "kd" '(org-clock-display :wk "display")

    "j"  '(nil :wk "journal")
    "jn" '(vulpea-journal-next :wk "next journal")
    "jp" '(vulpea-journal-previous :wk "prev journal")))

;;; Org Agenda

(use-package org-agenda
  :after org
  :preface
  (defun +life/--skip-scheduled-or-deadlined ()
    "Skip entries that are scheduled or have a deadline."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        subtree-end)))

  (defun +life/agenda-delete-empty-blocks ()
    "Remove empty agenda blocks."
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (pos-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (pos-eol))))))
    (setq buffer-read-only t))

  :custom
  (org-agenda-inhibit-startup t)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up urgency-down category-keep)
     (todo urgency-down category-keep)
     (tags urgency-down category-keep)
     (search category-keep)))
  (org-agenda-prefix-format
   '((agenda . " %i %(+life/agenda-category 24)%?-24t% s")
     (todo   . " %i %(+life/agenda-category 24) ")
     (tags   . " %i %(+life/agenda-category 24) ")
     (search . " %i %(+life/agenda-category 24) ")))
  (org-agenda-custom-commands
   `((" " "General"
      ((alltodo ""
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files (list ,org-default-notes-file))))
       (todo "NEXT|TODO"
             ((org-agenda-overriding-header "Unplanned NEXT/TODO")
              (org-agenda-skip-function #'+life/--skip-scheduled-or-deadlined)))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting")))
       (agenda "" ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)))))
     ("p" "Tasks"
      ((todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
       (todo "TODO" ((org-agenda-overriding-header "To Do")))))
     ("d" "Today"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-start-day nil)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))))
     ("y" "Past 14 days"
      ((agenda ""))
      ((org-agenda-start-day "-14d")
       (org-agenda-span 16)
       (org-agenda-start-on-weekday 1)
       (org-agenda-archives-mode t)
       (org-agenda-start-with-log-mode '(closed))
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))))
  :config
  (+local-leader-set-key 'org-agenda-mode-map
    "d" '(org-agenda-deadline :wk "deadline")
    "," '(org-agenda-priority :wk "priority")
    "t" '(org-agenda-todo :wk "todo status")
    "r" '(+life/agenda-refile :wk "refile"))
  (defun +org--update-agenda-files (&rest _)
    "Ensure vulpea/+life are loaded and update agenda files."
    (unless (featurep '+life)
      (require 'vulpea)
      (require '+life))
    (+life/agenda-files-update))
  (advice-add 'org-agenda :before #'+org--update-agenda-files)
  (define-advice org-agenda-prepare-buffers
      (:around (orig-fn files) suppress-expensive-hooks)
    "Suppress expensive per-buffer hooks when loading agenda files.
Agenda buffer preparation only needs org data (TODOs, tags, schedules),
not git status, visual pulsing, treesit grammars, or direnv."
    (let ((vc-handled-backends nil)
          (pulsar-global-mode nil)
          (global-treesit-auto-mode nil)
          (envrc-global-mode nil))
      (funcall orig-fn files)))
  (add-hook 'org-agenda-finalize-hook #'+life/agenda-delete-empty-blocks)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-segment +agenda-info
      "Agenda title and date."
      (concat
       (propertize "Agenda" 'face (doom-modeline-face 'doom-modeline-buffer-file))
       (doom-modeline-spc)
       (propertize (format-time-string "%A, %d %B %Y")
                   'face (doom-modeline-face 'doom-modeline-info))))

    (doom-modeline-def-modeline '+org-agenda
      '(bar +agenda-info)
      '(misc-info))

    (add-to-list 'doom-modeline-mode-alist '(org-agenda-mode . +org-agenda))))

;;; Vulpea

(use-package vulpea
  :ensure t
  :commands (vulpea-find vulpea-insert vulpea-buffer-tags-get
                         vulpea-buffer-tags-add vulpea-buffer-tags-remove
                         vulpea-db-sync-full-scan
                         +life/capture-initiative +life/capture-person
                         +life/capture-org +life/capture-idea
                         +life/view-pillars +life/view-goals
                         +life/view-projects +life/view-today
                         +life/go-to-parent +life/show-children
                         +life/show-stakeholders +life/set-initiative-status
                         +life/add-stakeholder
                         +life/remove-stakeholder +life/person-initiatives
                         +life/refresh-agenda-files +life/invalidate-agenda-cache
                         +life/refile +life/agenda-refile +life/agenda-person)
  :custom
  (vulpea-default-notes-directory (file-name-concat org-directory "roam"))
  (vulpea-db-location (expand-file-name "~/life/internals/vulpea.db"))
  :config
  (vulpea-db-autosync-mode 1)
  (require '+life))

(use-package vulpea-journal
  :ensure t
  :after vulpea
  :commands (vulpea-journal-today vulpea-journal-date
                                  vulpea-journal-next vulpea-journal-previous)
  :custom
  (vulpea-journal-tag "daily")
  (vulpea-journal-default-template
   '(:file-name "daily/%Y-%m-%d.org"
     :title "%Y-%m-%d"
     :tags ("daily")
     :head "#+created: %<[%Y-%m-%d]>"
     :body "* Notes\n** General\n** Work"))
  :config
  (vulpea-journal-setup))

(use-package vulpea-ui
  :ensure t
  :after vulpea
  :general
  (:keymaps 'vulpea-ui-sidebar-mode-map :states 'normal
            ;; journal day navigation
            "J" #'vulpea-journal-ui-next
            "K" #'vulpea-journal-ui-previous
            "." #'vulpea-journal-ui-today
            "D" #'vulpea-journal-ui-date
            ;; widget section navigation
            "]]" #'widget-forward
            "[[" #'widget-backward
            ;; link cycling
            "<tab>" #'widget-forward
            "<S-tab>" #'widget-backward
            ;; toggle sections
            "za" #'vulpea-ui-widget-toggle-at-point
            ;; follow link
            "<return>" #'vulpea-ui-follow-link-at-point
            "o" #'vulpea-ui-follow-link-at-point)
  :config
  (advice-add 'vulpea-find :after
              (lambda (&rest _) (vulpea-ui-sidebar-open)))
  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-segment +vulpea-sidebar-info
      "Vulpea sidebar: note title, tags, backlinks."
      (if-let* ((note vulpea-ui--current-note))
          (concat
           (propertize (or (vulpea-note-title note) "No note")
                       'face (doom-modeline-face 'doom-modeline-buffer-file))
           (when-let* ((tags (vulpea-note-tags note)))
             (concat (doom-modeline-spc)
                     (propertize (mapconcat (lambda (tag) (concat "#" tag)) tags " ")
                                 'face (doom-modeline-face 'doom-modeline-info))))
           (when-let* ((id (vulpea-note-id note))
                       (backlinks (vulpea-db-query-by-links-some
                                   (list (cons "id" id)))))
             (concat (doom-modeline-spc)
                     (propertize (format "%d backlinks" (length backlinks))
                                 'face (doom-modeline-face 'doom-modeline-buffer-minor-mode)))))
        (propertize "No note" 'face (doom-modeline-face 'doom-modeline-info))))

    (doom-modeline-def-modeline '+vulpea-sidebar
      '(bar +vulpea-sidebar-info)
      '(misc-info))

    (add-to-list 'doom-modeline-mode-alist '(vulpea-ui-sidebar-mode . +vulpea-sidebar))))

;;; Org Modern

(use-package org-modern
  :ensure t
  :hook (org-mode-hook . org-modern-mode)
  :custom
  (org-modern-list '((?+ . "◦")
                     (?* . "–")
                     (?- . "•")))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

;;; Citar (citation management)

(use-package citar
  :ensure t
  :commands (citar-open citar-open-entry citar-open-notes
             citar-insert-citation citar-open-files citar-refresh
             +citar/open-note)
  :custom
  (citar-bibliography
   (list (file-name-concat org-directory "bib/zotero-lib.bib")))
  (citar-at-point-function 'embark-act)
  (citar-notes-source 'vulpea)
  :config
  (require 'vulpea)

  ;; Vulpea-backed notes source for citar
  (defun +citar/--note-for-key (citekey)
    "Return the vulpea note for CITEKEY, or nil."
    (car (vulpea-db-query-by-property "ROAM_REFS" (concat "@" citekey))))

  (defun +citar/--has-notes ()
    "Return predicate testing whether a citekey has vulpea notes."
    (lambda (citekey) (when (+citar/--note-for-key citekey) t)))

  (defun +citar/--get-notes (&optional keys)
    "Return hash table of KEYS to lists of note file paths."
    (let ((result (make-hash-table :test 'equal)))
      (dolist (key (or keys
                       (mapcar (lambda (n)
                                 (string-remove-prefix
                                  "@" (or (cdr (assoc "ROAM_REFS" (vulpea-note-properties n))) "")))
                               (vulpea-db-query-by-property-key "ROAM_REFS"))))
        (when-let* ((note (+citar/--note-for-key key)))
          (puthash key (list (vulpea-note-path note)) result)))
      result))

  (defun +citar/--open-note (path)
    "Open the note at PATH."
    (find-file path))

  (defun +citar/--create-note (citekey _entry)
    "Create a reference note for CITEKEY."
    (let ((title (or (citar-get-value "title" citekey) citekey)))
      (vulpea-visit
       (vulpea-create
        title
        (file-name-concat org-directory "roam/references/${slug}.org")
        :tags '("reference")
        :properties `(("ROAM_REFS" . ,(concat "@" citekey)))
        :body "* First read\n\n* Key insights\n\n* Further thinking required\n\n* Suggested Links\n"))))

  (defun +citar/open-note (citekey &optional _entry)
    "Open or create a reference note for CITEKEY."
    (interactive (list (citar-select-ref)))
    (if-let* ((note (+citar/--note-for-key citekey)))
        (vulpea-visit note)
      (+citar/--create-note citekey nil)))

  ;; Register vulpea notes source and activate it
  (citar-register-notes-source
   'vulpea
   (list :name "Vulpea notes"
         :category 'org-heading
         :hasitems #'+citar/--has-notes
         :open #'+citar/--open-note
         :create #'+citar/--create-note
         :items #'+citar/--get-notes))

  ;; Wire org-cite processors to citar
  (require 'oc)
  (setq org-cite-global-bibliography citar-bibliography)
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar))

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :demand t
  :config
  (citar-embark-mode 1))

(provide 'mod-org)
;;; mod-org.el ends here
