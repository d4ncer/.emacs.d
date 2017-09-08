;;; rk-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'rk-emacs)
  (autoload 'evil-define-key "evil-core")
  (defconst rk-org-load-path (concat rk-emacs-lisp-directory "/org-mode/lisp"))
  (defconst rk-org-contrib-load-path (concat rk-emacs-lisp-directory "/org-mode/contrib/lisp")))

(require 'spacemacs-keys)
(require 'evilified-state)
(require 'f)
(require 's)
(require 'dash)
(require 'subr-x)

(defvar org-directory "~/org")

(defconst rk-org-work-file (concat org-directory "/work_movio.org"))
(defconst rk-org-numero-file (concat org-directory "/numero.org"))
(defconst rk-org-recruitment-file (concat org-directory "/recruitment.org"))

(use-package org
  :load-path rk-org-load-path
  :defer t

  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ("C-c c" . org-columns))

  :preface
  (progn
    (autoload 'org-entry-get "org")
    (autoload 'org-get-scheduled-time "org")
    (autoload 'org-get-todo-state "org")
    (autoload 'org-heading-components "org")
    (autoload 'org-todo "org")
    (autoload 'org-up-heading-safe "org")
    (autoload 'outline-forward-same-level "outline")
    (autoload 's-matches? "s")

    ;; KLUDGE: Pre-declare dynamic variables used by orgmode.
    (defvar org-state)
    (defvar org-log-states)
    (defvar org-log-done)

    (defun rk-org--exit-minibuffer (&rest _)
      "Exit minibuffer before adding notes."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))

    (defun rk-org--toggle-heading-goto-eol (&rest _)
      "Prevent point from moving to BOL when toggling headings."
      (when (s-matches? (rx bol (+ "*") (* space) eol)
                        (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-end-position))))

    (defun rk-org--mark-next-parent-tasks-todo ()
      "Visit each parent task and change state to TODO."
      (when-let (mystate (or (bound-and-true-p org-state)
                             (nth 2 (org-heading-components))))
        (save-excursion
          (while (org-up-heading-safe)
            (when (-contains? '("NEXT" "WAITING" "MAYBE")
                              (nth 2 (org-heading-components)))
              (org-todo "TODO"))))))

    (defun rk-org--add-local-hooks ()
      "Set buffer-local hooks for orgmode."
      (add-hook 'org-after-todo-state-change-hook #'rk-org--mark-next-parent-tasks-todo nil t)
      (add-hook 'org-clock-in-hook #'rk-org--mark-next-parent-tasks-todo nil t))

    (defun rk-org--set-next-todo-state ()
      "When marking a todo to DONE, set the next TODO as NEXT.
Do not scheduled items or repeating todos."
      (when (equal org-state "DONE")
        (save-excursion
          (when (and (ignore-errors (outline-forward-same-level 1) t)
                     (equal (org-get-todo-state) "TODO"))
            (unless (or (org-entry-get (point) "STYLE")
                        (org-entry-get (point) "LAST_REPEAT")
                        (org-get-scheduled-time (point)))
              (org-todo "NEXT"))))))

    (defun rk-org--children-done-parent-done (_n-done n-todo)
      "Mark the parent task as done when all children are completed."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (zerop n-todo) "DONE" "TODO")))))

  :commands (org-refile
             org-deadline
             org-ctrl-c-ctrl-c
             org-fill-paragraph
             org-insert-link
             org-schedule)

  :init
  (progn
    (add-hook 'org-mode-hook #'rk-org--add-local-hooks)
    (add-hook 'org-after-todo-state-change-hook #'rk-org--set-next-todo-state)
    (add-hook 'org-after-todo-statistics-hook #'rk-org--children-done-parent-done)

    ;; Not sure I need this
    ;; (dolist (dir (f-directories "~/.org/lisp/"))
    ;;   (add-to-list 'load-path (f-slash dir)))

    (spacemacs-keys-set-leader-keys
      "ok" #'org-capture
      "ol" #'org-store-link
      "os" #'org-search-view))

  :config
  (progn

    (setq org-default-notes-file (f-join org-directory "notes.org"))

    (spacemacs-keys-set-leader-keys-for-major-mode
      'org-mode
      "r" #'org-refile
      "d" #'org-deadline
      "C" #'org-ctrl-c-ctrl-c
      "f" #'org-fill-paragraph
      "L" #'org-insert-link
      "s" #'org-schedule)

    (evil-define-key 'normal org-mode-map (kbd "RET") #'org-return)

    (add-to-list 'org-refile-targets '(nil :maxlevel . 3))
    (add-to-list 'org-refile-targets '(org-default-notes-file :maxlevel . 3))
    (add-to-list 'org-refile-targets `(,org-directory :maxlevel . 3))
    (add-to-list 'org-tags-exclude-from-inheritance "project")

    (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file-other-window)

    (setq org-M-RET-may-split-line nil)
    (setq org-catch-invisible-edits 'smart)
    (setq org-cycle-separator-lines 1)
    (setq org-enforce-todo-dependencies t)
    (setq org-footnote-auto-adjust t)
    (setq org-indirect-buffer-display 'current-window)
    (setq org-insert-heading-respect-content t)
    (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
    (setq org-log-done 'time)
    (setq org-use-sub-superscripts '{})
    (setq org-log-into-drawer t)
    (setq org-hide-emphasis-markers t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-pretty-entities t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))

    (setq org-refile-use-outline-path t)
    (setq org-return-follows-link t)
    (setq org-reverse-note-order nil)
    (setq org-confirm-elisp-link-function nil)
    (setq org-startup-indented t)
    (setq org-startup-with-inline-images t)
    (setq org-hierarchical-todo-statistics nil)
    (setq org-checkbox-hierarchical-statistics t)
    (setq org-log-repeat nil)
    (setq org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))

    (setq org-todo-keywords '((type "TODO(t)" "MAYBE(m)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                              (type "SOMEDAY(s)" "|")))

    (advice-add 'org-add-log-note :before #'rk-org--exit-minibuffer)
    (advice-add 'org-toggle-heading :after #'rk-org--toggle-heading-goto-eol)))

(use-package org-id
  :after org
  :config
  (setq org-id-locations-file (f-join rk-emacs-cache-directory "org-id-locations")))

(use-package org-table
  :after org
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Edit Formulas*" eos)
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))


(use-package org-attach
  :after org
  :config
  (setq org-attach-directory (f-join org-directory "data")))

(use-package org-agenda
  :load-path rk-org-load-path
  :after org
  :bind (:map org-agenda-mode-map ("J" . org-agenda-goto-date))

  :preface
  (defun rk-org--exclude-tasks-on-hold (tag)
    (and (equal tag "hold") (concat "-" tag)))

  :config
  (progn
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up)

    (spacemacs-keys-set-leader-keys
      "oA" #'org-agenda)

    (define-key org-agenda-mode-map (kbd "C-f" ) #'evil-scroll-page-down)
    (define-key org-agenda-mode-map (kbd "C-b") #'evil-scroll-page-up)

    ;; Match projects that do not have a scheduled action or NEXT action.
    (setq org-stuck-projects '("+project-ignore-maybe-done"
                               ("NEXT") nil
                               "SCHEDULED:"))

    ;; Enable leader key in agenda.
    (define-key org-agenda-mode-map (kbd "SPC") spacemacs-keys-default-map)

    (setq org-agenda-include-diary nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-auto-exclude-function #'rk-org--exclude-tasks-on-hold)
    (setq org-agenda-files (f-files org-directory (lambda (f) (f-ext? f "org"))))
    (setq org-agenda-diary-file (f-join org-directory "diary.org"))
    (setq org-agenda-hide-tags-regexp (rx (or "noexport" "someday" "project")))
    (setq org-agenda-insert-diary-extract-time t)
    (setq org-agenda-span 'week)
    (setq org-agenda-search-view-always-boolean t)
    (setq org-agenda-show-all-dates nil)
    (setq org-agenda-show-inherited-tags nil)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-sorting-strategy
          '((agenda time-up priority-down category-keep)
            (todo priority-down category-keep scheduled-up)
            (tags priority-down category-keep)
            (search category-keep)))
    (setq org-agenda-text-search-extra-files '(agenda-archives))
    (setq org-agenda-use-time-grid nil)
    (setq org-agenda-inhibit-startup t)
    (setq org-agenda-tags-column -100)

    (setq org-agenda-clockreport-parameter-plist
          (list
           :compact t
           :maxlevel 5
           :fileskip0 t
           :step 'week))

    (setq org-time-clocksum-format
          (list :hours "%d" :require-hours t
                :minutes ":%02d" :require-minutes t))

    (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

    (setq org-agenda-custom-commands
          '(("A" "Agenda and next actions"
             ((tags-todo "-someday/NEXT"
                         ((org-agenda-overriding-header "Next Actions")))
              (agenda "")
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting")))
              (stuck ""))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-files (list org-default-notes-file org-agenda-diary-file))
              (org-agenda-dim-blocked-tasks nil)
              (org-agenda-archives-mode nil)
              (org-agenda-ignore-drawer-properties '(effort appt))))

            ("n" "Next actions"
             ((tags-todo "-someday/NEXT"))
             ((org-agenda-overriding-header "Next Actions")))

            ("r" "Weekly Review"
             ((agenda ""
                      ((org-agenda-overriding-header "Review Previous Week")
                       (org-agenda-ndays 7)
                       (org-agenda-start-day "-7d")))
              (agenda ""
                      ((org-agenda-overriding-header "Review Upcoming Events")
                       (org-agenda-ndays 14)))
              (stuck ""
                     ((org-agenda-overriding-header "Review Stuck Projects")))
              (todo "WAITING"
                    ((org-agenda-overriding-header "Review Tasks on Hold")))

              (tags-todo "-someday/NEXT"
                         ((org-agenda-overriding-header "Next Actions")))
              (tags-todo "+goals+3_months+project/NEXT"
                         ((org-agenda-overriding-header "Review 3 Month Goals")))
              (tags-todo "+goals+1_year+project/NEXT"
                         ((org-agenda-overriding-header "Review 1 Year Goals")))
              (tags-todo "+goals+3_years+project/MAYBE|SOMEDAY|NEXT"
                         ((org-agenda-overriding-header "Review 3 Year Goals")))
              (tags-todo "someday-skill/MAYBE|NEXT"
                         ((org-agenda-overriding-header "Decide whether to promote any SOMEDAY items to NEXT actions")))
              (tags-todo "someday&skill"
                         ((org-agenda-overriding-header "Decide whether to promote any learning tasks to NEXT actions"))))
             ((org-agenda-tag-filter-preset
               '("-drill" "-gtd" "-ignore"))
              (org-agenda-include-inactive-timestamps t)
              (org-agenda-files (list org-default-notes-file rk-org-numero-file rk-org-work-file org-agenda-diary-file))
              (org-agenda-archives-mode nil)
              (org-agenda-dim-blocked-tasks nil)))

            ("s" "Standup "
             ((tags "+standup/!-DONE"
                    ((org-agenda-overriding-header
                      (concatenate 'string
                                   "Standup "
                                   (org-make-link-string "https://moviohq.atlassian.net/secure/RapidBoard.jspa?rapidView=134&selectedIssue=GREEN-60&quickFilter=523" "JIRA Board")
                                   " "
                                   (org-make-link-string "https://github.com/movio/mc-wysiwyg/issues" "WYSIWYG")
                                   " "
                                   (org-make-link-string "https://github.com/movio/green/issues" "Green")
                                   " "
                                   (org-make-link-string "https://gogs.movio.co/movio/kube-mc-green/issues" "Gogs")
                                   " "
                                   (org-make-link-string "https://github.com/movio/reporting-ui-components" "Reporting UI Components"))))))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-use-tag-inheritance nil)
              (org-agenda-files (list rk-org-work-file org-agenda-diary-file))
              (org-agenda-dim-blocked-tasks nil)
              (org-agenda-archives-mode nil)
              (org-agenda-ignore-drawer-properties '(effort appt))))

            ("w" "Work actions"
             ((tags-todo "-someday-media-study/NEXT"
                         ((org-agenda-overriding-header "Next Actions")))
              (agenda "")
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting")))
              (stuck ""))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-use-tag-inheritance nil)
              (org-agenda-files (list rk-org-work-file org-agenda-diary-file))
              (org-agenda-dim-blocked-tasks nil)
              (org-agenda-archives-mode nil)
              (org-agenda-ignore-drawer-properties '(effort appt))))))))

(use-package appt
  :defer t
  :config
  (progn
    (setq appt-message-warning-time 60)
    (setq appt-display-interval 5)))

(use-package org-archive
  :after org
  :load-path rk-org-load-path
  :functions (org-archive-subtree)
  :preface
  (progn
    (autoload 'org-map-entries "org")
    (autoload 'org-set-tags-to "org")
    (autoload 'org-get-tags-at "org")

    (defun rk-org--archive-done-tasks ()
      (interactive)
      (atomic-change-group
        (org-map-entries (lambda ()
                           ;; HACK: Ensure point does not move past the next
                           ;; item to archive.
                           (let ((org-map-continue-from (point)))
                             (org-archive-subtree)))
                         "/DONE|PAID|VOID|CANCELLED" 'tree)))

    (defun rk-org--apply-inherited-tags (&rest _)
      "Apply inherited tags when archiving."
      (org-set-tags-to (org-get-tags-at))))

  :config
  (progn
    (setq org-archive-default-command #'rk-org--archive-done-tasks)
    (advice-add 'org-archive-subtree :before #'rk-org--apply-inherited-tags)))

(use-package org-src
  :after org
  :load-path rk-org-load-path

  :preface
  (progn
    (defun rk-org--suppress-final-newline ()
      "Remove trailing newline in src blocks."
      (setq-local require-final-newline nil))

    (defun rk-org--org-src-delete-trailing-space (&rest _)
      "Delete trailing whitespace when exiting src blocks."
      (delete-trailing-whitespace)))

  :config
  (progn
    (setq org-src-fontify-natively t)
    (setq org-src-window-setup 'current-window)
    (add-hook 'org-src-mode-hook #'rk-org--suppress-final-newline)
    (advice-add 'org-edit-src-exit :before #'rk-org--org-src-delete-trailing-space)))

(use-package org-clock
  :after org
  :load-path rk-org-load-path

  :preface
  (progn
    (autoload 'org-remove-empty-drawer-at "org")

    (defun rk-org--remove-empty-clock-drawers ()
      "Remove empty clock drawers at point."
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point)))))

  :config
  (progn
    (setq org-clock-persist t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-history-length 20)
    (setq org-clock-in-resume t)
    (setq org-clock-report-include-clocking-task t)
    (setq org-clock-out-remove-zero-time-clocks t)
    (setq org-clock-persist-file (f-join org-directory ".org-clock-save"))

    (org-clock-persistence-insinuate)
    (add-hook 'org-clock-out-hook #'rk-org--remove-empty-clock-drawers t)))

(use-package org-drill
  :after org
  :commands (org-drill
             org-drill-strip-all-data
             org-drill-cram
             org-drill-tree
             org-drill-resume
             org-drill-merge-buffers
             org-drill-entry
             org-drill-directory
             org-drill-again)
  :preface
  (defconst rk-org-drill-files (f-files (concat org-directory "/drill")))

  :defines
  (org-drill-scope
   org-drill-learn-fraction
   org-drill-adjust-intervals-for-early-and-late-repetitions-p
   org-drill-add-random-noise-to-intervals-p
   org-drill-save-buffers-after-drill-sessions-p)

  :config
  (progn
    (defconst rk-org-drill-file (f-join org-directory "drill" "drill.org"))

    (setq org-drill-scope rk-org-drill-files)

    (add-to-list 'org-refile-targets '(rk-org-drill-files :maxlevel . 3))

    (setq org-drill-learn-fraction 0.25)
    (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-save-buffers-after-drill-sessions-p nil)))

(use-package org-capture
  :after org
  :load-path rk-org-load-path
  :preface
  (defun rk-org--capture-template-entry (key label form template &rest kws)
    (append
     (list key label 'entry form template
           :clock-keep t
           :empty-lines 1
           :prepend t)
     kws))
  :config
  (setq org-capture-templates
        (list
         (rk-org--capture-template-entry
          "d" "Diary"
          '(file+datetree org-agenda-diary-file) "*  %?\n%^T")

         (rk-org--capture-template-entry
          "D" "Diary (work)"
          `(file+datetree rk-org-work-file) "*  %?\n%^T")

         (rk-org--capture-template-entry
          "r" "Reporting"
          '(file+olp+datetree rk-org-work-file "Reporting") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "e" "Editors"
          '(file+olp+datetree rk-org-work-file "Editors") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "o" "Operations"
          '(file+olp+datetree rk-org-work-file "Operations") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "i" "Infra"
          '(file+olp+datetree rk-org-work-file "Infra") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "e" "ETL"
          '(file+olp+datetree rk-org-work-file "ETL") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "l" "Legacy"
          '(file+olp+datetree rk-org-work-file "Legacy") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "R" "Recruitment"
          '(file+olp+datetree rk-org-work-file "Recruitment") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "t" "Team Lead"
          '(file+olp+datetree rk-org-work-file "Team Lead") "*  %?\n%^T")

         (rk-org--capture-template-entry
          "S" "Standup"
          `(file+datetree rk-org-work-file) "* TODO %? :standup:\n%^t")

         (rk-org--capture-template-entry
          "s" "Someday"
          '(file+olp org-agenda-diary-file "Side projects")
          "* SOMEDAY  %?")))
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-minor-mode 'org-capture-mode
      "d" #'org-deadline
      "c" #'org-capture-finalize
      "k" #'org-capture-kill
      "r" #'org-capture-refile
      "A" #'org-align-all-tags)))

(use-package org-download
  :after org
  :load-path rk-org-load-path
  :config
  (setq org-download-method 'attach))

(use-package evil
  :defer t
  :preface
  (progn
    (autoload 'org-at-heading-p "org")

    (defun rk-org--evil-insert-state (&rest _)
      "Enter evil insert state when creating new headings."
      (when (called-interactively-p nil)
        (evil-insert-state)))

    (defun rk-org--add-blank-line-after-heading (&rest _)
      "Add a blank line of padding below new headings."
      (when (and (called-interactively-p nil)
                 (org-at-heading-p))
        (let ((next-line-blank?
               (save-excursion
                 (forward-line)
                 (s-blank? (buffer-substring (line-beginning-position) (line-end-position))))))
          (unless next-line-blank?
            (save-excursion
              (goto-char (line-end-position))
              (open-line 1)))))))

  :config
  (progn
    (advice-add 'org-insert-heading :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading :after #'rk-org--evil-insert-state)

    (advice-add 'org-insert-heading :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading :after #'rk-org--add-blank-line-after-heading)))

(use-package ox
  :after org
  :init
  (defvar org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence))
  :config
  (progn
    (require 'ox-gfm)
    (setq org-export-exclude-tags '("noexport" "crypt"))
    (setq org-export-coding-system 'utf-8)))

(use-package ox-confluence
  :after org
  :config
  (org-export-define-derived-backend 'custom-confluence 'confluence
    :menu-entry
    '(?c "Export as Confluence markup"
         ((?c "To temporary buffer" org-confluence-export-as-confluence)))))


(use-package ox-koma-letter
  :after org
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil)))

(use-package ox-html
  :after org
  :preface
  (defun rk-org-html-open-tags-setup
      (number _group-number _start-group-p _end-group-p topp bottomp)
    (cond (topp "<tr class=\"tr-top\">")
          (bottomp "<tr class=\"tr-bottom\">")
          (t (if (= (mod number 2) 1)
                 "<tr class=\"tr-odd\">"
               "<tr class=\"tr-even\">"))))
  :config
  (progn
    (setq org-html-html5-fancy t)
    (setq org-html-postamble nil)

    ;; Highlight alternating rows in HTML tables.

    (setq org-html-table-row-open-tag #'rk-org-html-open-tags-setup)
    (setq org-html-head-extra
          "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
")))

(use-package rk-org-clock-cascade
  :after org
  :functions (rk-org-clock-cascade-init)
  :init (add-hook 'org-mode-hook #'rk-org-clock-cascade-init))

(use-package rk-org-export-koma-letter
  :after org
  :functions (rk-org-export-koma-letter-init)
  :config
  (progn
    (add-to-list 'org-latex-classes `("koma-letter" ,rk-org-export-koma-letter-latex-class))
    (setq org-latex-hyperref-template "")
    (add-hook 'org-ctrl-c-ctrl-c-hook #'rk-org-export-koma-letter--handler t)))

(use-package rk-org-capture-url
  :after org)

(use-package rk-org-goto
  :commands (rk-org-goto-agenda
             rk-org-goto-diary
             rk-org-goto-notes
             rk-org-goto-work
             rk-org-goto-numero
             rk-org-goto-recruitment
             rk-org-goto-todo-list
             rk-org-goto-tags-list)
  :init
  (spacemacs-keys-set-leader-keys
    "oa" #'rk-org-goto-agenda
    "od" #'rk-org-goto-diary
    "on" #'rk-org-goto-notes
    "ow" #'rk-org-goto-work
    "oN" #'rk-org-goto-numero
    "or" #'rk-org-goto-recruitment
    "ot" #'rk-org-goto-todo-list
    "ov" #'rk-org-goto-tags-list))

(use-package rk-org-ctrl-c-ret
  :after org
  :config
  (progn
    (evil-define-key 'normal org-mode-map (kbd "C-c <return>") #'rk-org-ctrl-c-ret)
    (evil-define-key 'emacs org-mode-map (kbd "C-c <return>") #'rk-org-ctrl-c-ret)))

(use-package rk-org-ctrl-c-ctrl-k
  :after org
  :config
  (evil-define-key 'normal org-mode-map (kbd "C-c C-k") #'rk-org-ctrl-c-ctrl-k))

(use-package rk-diary-utils
  :after org)

(use-package evil-org
  :after org
  :config
  (progn
    ;; Remove weird keybindings.
    (evil-define-key 'normal org-mode-map (kbd "M-l") nil)
    (evil-define-key 'normal org-mode-map (kbd "M-h") nil)
    (evil-define-key 'insert org-mode-map (kbd "M-l") nil)
    (evil-define-key 'insert org-mode-map (kbd "M-h") nil)
    (evil-define-key 'normal evil-org-mode-map (kbd "J") nil)
    (evil-define-key 'normal evil-org-mode-map (kbd "O") nil)))

(use-package org-indent
  :after org
  :commands (org-indent-mode)
  :init
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package flyspell
  :after org
  :commands (flyspell-mode)
  :init
  (add-hook 'org-mode-hook #'flyspell-mode))

(provide 'rk-org)

;;; rk-org.el ends here
