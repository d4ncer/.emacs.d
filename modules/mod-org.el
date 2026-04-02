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
;; - org-modern (modern styling for org-mode)
;;
;; IMPORTANT: This module is designed to be deferred and must NOT be loaded eagerly.
;; The org package uses :after-call to ensure it only loads when needed.

;;; Code:

;;; Org mode

(use-package org
  :ensure t
  :after-call +first-file-hook
  :hook ((org-mode-hook . visual-line-mode))
  :general (:keymaps 'org-mode-map :states '(normal visual motion)
                     "RET" (general-predicate-dispatch #'evil-ret
                             (org-in-regexp org-link-any-re) #'org-open-at-point)
                     "TAB" #'org-cycle
                     "S-TAB" #'org-shifttab)
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
  (add-hook 'org-mode-hook #'+org-format-on-save-mode))

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
  :general (:keymaps 'org-agenda-mode-map
            :states '(normal motion visual)
            "SPC" nil)
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
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-agenda-mode 'normal))
  (with-eval-after-load 'nano-modeline
    (defun +modeline-agenda-title ()
      "Display the agenda view title."
      (propertize "Agenda"
                  'face 'nano-modeline-face-primary))

    (defun +modeline-agenda-date ()
      "Display the current date in the agenda."
      (propertize (format-time-string "%A, %d %B %Y")
                  'face 'nano-modeline-face-secondary))

    (defcustom nano-modeline-format-org-agenda
      (cons '(nano-modeline-element-buffer-status
              nano-modeline-element-space
              +modeline-agenda-title
              nano-modeline-element-space
              +modeline-agenda-date)
            '(nano-modeline-element-window-status
              nano-modeline-element-space))
      "Modeline format for org-agenda buffers."
      :type 'nano-modeline-type
      :group 'nano-modeline-modes)

    (defun +nano-modeline-org-agenda ()
      (nano-modeline nano-modeline-format-org-agenda))

    (add-hook 'org-agenda-mode-hook #'+nano-modeline-org-agenda)))

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
                         +life/show-stakeholders +life/add-stakeholder
                         +life/remove-stakeholder +life/person-initiatives
                         +life/refresh-agenda-files +life/invalidate-agenda-cache
                         +life/refile +life/agenda-refile +life/agenda-person)
  :custom
  (vulpea-default-notes-directory (file-name-concat org-directory "roam"))
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
     :head "#+created: %<[%Y-%m-%d]>"))
  :config
  (vulpea-journal-setup))

(use-package vulpea-ui
  :ensure t
  :after vulpea
  :config
  (advice-add 'vulpea-find :after
              (lambda (&rest _) (vulpea-ui-sidebar-open)))
  (with-eval-after-load 'nano-modeline
    (defun +modeline-vulpea-sidebar-spacer ()
      "Invisible spacer that preserves header line height."
      (nano-modeline-element-buffer-status " " 'nano-modeline-face-secondary))

    (defun +modeline-vulpea-sidebar-note-title ()
      "Display the current note title in the vulpea sidebar."
      (if-let* ((note vulpea-ui--current-note)
                (title (vulpea-note-title note)))
          (propertize title 'face 'nano-modeline-face-primary)
        (propertize "No note" 'face 'nano-modeline-face-secondary)))

    (defun +modeline-vulpea-sidebar-tags ()
      "Display tags for the current note in the vulpea sidebar."
      (when-let* ((note vulpea-ui--current-note)
                  (tags (vulpea-note-tags note)))
        (propertize (mapconcat (lambda (tag) (concat "#" tag)) tags " ")
                    'face 'nano-modeline-face-secondary)))

    (defun +modeline-vulpea-sidebar-backlinks ()
      "Display backlink count for the current note in the vulpea sidebar."
      (when-let* ((note vulpea-ui--current-note)
                  (id (vulpea-note-id note))
                  (backlinks (vulpea-db-query-by-links-some
                              (list (cons "id" id)))))
        (propertize (format "%d backlinks" (length backlinks))
                    'face 'nano-modeline-face-secondary)))

    (defcustom nano-modeline-format-vulpea-sidebar
      (cons '(+modeline-vulpea-sidebar-spacer
              +modeline-vulpea-sidebar-note-title
              nano-modeline-element-space
              +modeline-vulpea-sidebar-tags)
            '(+modeline-vulpea-sidebar-backlinks
              nano-modeline-element-space
              nano-modeline-element-window-status
              nano-modeline-element-space))
      "Modeline format for vulpea sidebar buffers."
      :type 'nano-modeline-type
      :group 'nano-modeline-modes)

    (defun +nano-modeline-vulpea-sidebar ()
      (nano-modeline nano-modeline-format-vulpea-sidebar))

    (add-hook 'vulpea-ui-sidebar-mode-hook #'+nano-modeline-vulpea-sidebar)))

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

(provide 'mod-org)
;;; mod-org.el ends here
