;;; rk-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'paths)
(require 'general)
(require 'definers)
(require 'f)
(require 's)
(require 'dash)
(require 'subr-x)
(require 'transient)
(require 'pretty-hydra)
(require 'rk-misc-utils)

(autoload 'evil-define-key "evil")

(defvar org-directory paths--org-dir)
(defconst rk-org-gtd-dir (f-join paths--org-dir "gtd"))
(defconst rk-org-roam-dir (f-join paths--org-dir "roam"))
(defconst rk-org-roam-dailies-dir (f-join rk-org-roam-dir "daily"))
(defconst rk-org-roam-temporal-prefix "%<%Y%m%d%H%M%S>")

(use-package org
  :straight t
  :demand t
  :general
  (:keymaps 'org-mode-map
            "C-;" #'insert-char
            "C-c l" #'rkca-insert-TKISS-link
            "C-c C-." #'org-time-stamp-inactive
            "C-n" #'org-next-visible-heading
            "C-p" #'org-previous-visible-heading
            "M-p"     #'org-metaup
            "M-n"     #'org-metadown
            "C-c c"   #'org-columns)
  (:keymaps 'org-mode-map :states '(normal visual motion)
            "C-n" #'org-next-visible-heading
            "C-p" #'org-previous-visible-heading
            "gb" #'org-mark-ring-goto)
  (:keymaps 'org-mode-map :states '(visual)
            "-" #'rk-org--deemphasize
            "B" #'rk-org--embolden
            "_" #'rk-org--underline
            "/" #'rk-org--italicize
            "+" #'rk-org--strike-through
            "=" #'rk-org--quote)
  :defines (org-state
            org-log-states
            org-log-done
            org-tag-group-re
            org-refile-targets
            org-default-notes-file
            org-tags-exclude-from-inheritance
            org-link-frame-setup
            org-mode-map
            org-M-RET-may-split-line
            org-catch-invisible-edits
            org-cycle-separator-lines
            org-enforce-todo-dependencies
            org-footnote-auto-adjust
            org-indirect-buffer-display
            org-insert-heading-respect-content
            org-link-abbrev-alist
            org-use-sub-superscripts
            org-log-into-drawer
            org-hide-emphasis-markers
            org-outline-path-complete-in-steps
            org-pretty-entities
            org-refile-allow-creating-parent-nodes
            org-refile-target-verify-function
            org-done-keywords
            org-refile-use-outline-path
            org-return-follows-link
            org-reverse-note-order
            org-confirm-elisp-link-function
            org-startup-indented
            org-startup-with-inline-images
            org-hierarchical-todo-statistics
            org-checkbox-hierarchical-statistics
            org-log-repeat
            org-blank-before-new-entry
            org-todo-keywords)

  :preface
  (autoload 'outline-forward-same-level "outline")
  (defun rk-org--deemphasize ()
    (interactive)
    (when (use-region-p)
      (replace-regexp-in-region (rx (or "*" "_" "/" "+" "=" "~")) "" (region-beginning) (region-end))))
  (defun rk-org--embolden ()
    (interactive)
    (org-emphasize (string-to-char "*")))
  (defun rk-org--italicize ()
    (interactive)
    (org-emphasize (string-to-char "/")))
  (defun rk-org--underline ()
    (interactive)
    (org-emphasize (string-to-char "_")))
  (defun rk-org--quote ()
    (interactive)
    (org-emphasize (string-to-char "=")))
  (defun rk-org--strike-through ()
    (interactive)
    (org-emphasize (string-to-char "+")))
  (defun rk-org--tag-headline-or-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'org-change-tag-in-region)
      (counsel-org-tag)))

  (defun rk-org--exit-minibuffer (&rest _)
    "Exit minibuffer before adding notes."
    (when (minibufferp (window-buffer (selected-window)))
      (other-window 1)))

  (defun rk-org--toggle-heading-goto-eol (&rest _)
    "Prevent point from moving to BOL when toggling headings."
    (when (s-matches? (rx bol (+ "*") (* space) eol)
                      (buffer-substring (line-beginning-position) (line-end-position)))
      (goto-char (line-end-position))))

  (defun rk-org--mark-first-child-next ()
    "Mark first task in a project as NEXT when refiling."
    (when (org-first-sibling-p)
      (org-todo "NEXT")))

  (defun rk-org--remove-todo-tickler-or-reference ()
    (let ((last-refile (-first-item org-refile-history)))
      (when (or (s-contains-p "tickler.org" last-refile)
                (s-contains-p "reference.org" last-refile))
        (org-todo ""))))

  (defun rk-org--mark-next-parent-tasks-todo ()
    "Visit each parent task and change state to TODO."
    (let ((mystate (or (and (fboundp 'org-state)
                            state)
                       (nth 2 (org-heading-components)))))
      (when mystate
        (save-excursion
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) (list "NEXT"))
              (org-todo "TODO")))))))

  (defun rk-org--disable-flycheck ()
    "Disable Flycheck in org buffers."
    (with-eval-after-load 'flycheck
      (flycheck-mode -1)))

  (defun rk-org--disable-ligatures ()
    (when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode -1)))

  (defun rk-org--setup-org ()
    (rk-org--disable-flycheck)
    (rk-org--disable-ligatures))

  (defun rk-org--set-next-todo-state ()
    "When marking a todo to DONE, set the next TODO as NEXT.
Do not scheduled items or repeating todos."
    (save-excursion
      (when (and (string= org-state "DONE")
                 (org-goto-sibling)
                 (-contains? '("TODO") (org-get-todo-state))
                 (not (-contains-p (org-get-tags) "project")))
        (org-todo "NEXT"))))

  :init
  (load-file (expand-file-name "org-version.el" (concat paths-lisp-directory "/rk-org")))
  (rk-leader-def
    "ns" '(org-narrow-to-subtree :wk "narrow to subtree")
    "ok" '(org-capture :wk "capture")
    "ol" '(org-store-link :wk "store link")
    "os" '(org-search-view :wk "search"))

  :hook ((org-after-refile-insert . rk-org--remove-todo-tickler-or-reference)
         (org-after-refile-insert . rk-org--mark-first-child-next)
         (org-mode . rk-org--setup-org)
         (org-babel-after-execute . org-display-inline-images))
  :config
  (setq org-default-notes-file (f-join paths--org-dir "notes.org"))
  (add-hook 'org-mode-hook #'rk-org--disable-flycheck)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'rk-org--disable-ligatures)
  (add-hook 'org-after-todo-state-change-hook #'rk-org--set-next-todo-state)
  (add-hook 'org-clock-in-hook #'rk-org--mark-next-parent-tasks-todo 'append)
  (add-hook 'org-after-todo-state-change-hook #'rk-org--mark-next-parent-tasks-todo 'append)
  (add-hook 'org-after-refile-insert-hook #'rk-org--mark-first-child-next)

  (setq org-default-notes-file (f-join paths--org-dir "notes.org"))
  (rk-local-leader-def :keymaps 'org-mode-map
    "A" '(org-align-tags :wk "align tags")
    "r" '(org-refile :wk "refile")
    "d" '(org-deadline :wk "add deadline")
    "C" '(org-ctrl-c-ctrl-c :wk "magic C")
    "f" '(org-fill-paragraph :wk "wrap text")
    "L" '(org-insert-link :wk "insert link")
    "t" '(rk-org--tag-headline-or-region :wk "edit tags")
    "p" '(org-set-property :wk "set property")
    "s" '(org-schedule :wk "schedule"))

  (general-def :keymaps 'org-mode-map :states 'normal
    "RET" #'org-return)

  (setq org-refile-targets nil)
  (add-to-list 'org-refile-targets '(nil :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--someday-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--consume-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--work-projects-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--personal-projects-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--next-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--reference-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--tickler-file :maxlevel . 1))
  (add-to-list 'org-refile-targets '(rk-org--diary-file :maxlevel . 1))
  (add-to-list 'org-tags-exclude-from-inheritance "project")

  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

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
  (setq org-agenda-diary-file rk-org--inbox-file)

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

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "SOMEDAY(o)" "|")
                            (sequence "SCHEDULE(s)" "|")))

  (setq org-confirm-babel-evaluate nil)

  (advice-add 'org-add-log-note :before #'rk-org--exit-minibuffer)
  (advice-add 'org-toggle-heading :after #'rk-org--toggle-heading-goto-eol))

(use-package ob-python
  :after org
  :preface
  (defun rk-org-setup-python ()
    (when (executable-find "ipython")
      (setq-local org-babel-python-command "ipython")))
  :config
  (add-hook 'org-mode-hook #'rk-org-setup-python))

(use-package gnuplot
  :straight t
  :defer t)

(use-package ob-gnuplot
  :after org)

(use-package ob-restclient
  :straight t
  :after org)

(use-package ob-shell :after org)

(use-package ob-sql :after org)

(use-package org-id
  :after org
  :defines (org-id-locations-file)
  :config
  (setq org-id-locations-file (f-join paths-cache-directory "org-id-locations")))

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
  :custom
  (org-attach-id-dir (f-join paths--org-dir "data")))

(use-package org-agenda
  :after org
  :preface
  (defun rk-org--exclude-tasks-on-hold (tag)
    (and (equal tag "hold") (concat "-" tag)))

  :defines (org-duration-format
            org-stuck-projects
            org-agenda-mode-map
            org-agenda-include-diary
            org-agenda-start-on-weekday
            org-agenda-auto-exclude-function
            org-agenda-hide-tags-regexp
            org-agenda-insert-diary-extract-time
            org-agenda-span
            org-agenda-search-view-always-boolean
            org-agenda-show-all-dates
            org-agenda-show-inherited-tags
            org-agenda-skip-deadline-if-done
            org-agenda-skip-deadline-prewarning-if-scheduled
            org-agenda-skip-scheduled-if-done
            org-agenda-sorting-strategy
            org-agenda-text-search-extra-files
            org-agenda-use-time-grid
            org-agenda-inhibit-startup
            org-agenda-tags-column
            org-agenda-clockreport-parameter-plist
            org-agenda-custom-commands
            org-agenda-files
            org-agenda-diary-file)

  :config
  (rk-leader-def
    "oA" '(org-agenda :wk "agendas"))

  (rk-local-leader-def :keymaps 'org-agenda-mode-map
    "d" '(org-agenda-deadline :wk "deadline")
    "p" '(org-agenda-set-property :wk "set property")
    "r" '(org-agenda-refile :wk "refile"))

  ;; Match projects that do not have a scheduled action or NEXT action.
  (setq org-stuck-projects '("+project-ignore-maybe-done"
                             ("NEXT") nil
                             "SCHEDULED:"))

  (setq org-agenda-include-diary nil)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-auto-exclude-function #'rk-org--exclude-tasks-on-hold)
  (setq org-agenda-files (f-files paths--gtd-dir (lambda (f) (f-ext? f "org"))))
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

  (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

  (setq org-agenda-custom-commands
        '(("A" "All"
           ((tags-todo "inbox"
                       ((org-agenda-overriding-header "To Refile")))
            (tags-todo "-someday/NEXT"
                       ((org-agenda-overriding-header "Next Actions")))
            (stuck "")
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (agenda ""))
           ((org-agenda-tag-filter-preset '("-ignore"))
            (org-agenda-files (list rk-org--work-projects-file rk-org--inbox-file rk-org--next-file rk-org--consume-file rk-org--tickler-file))
            (org-agenda-dim-blocked-tasks nil)
            (org-agenda-archives-mode nil)
            (org-agenda-ignore-properties '(effort appt))))

          ("w" "Work"
           ((tags-todo "inbox"
                       ((org-agenda-overriding-header "To Refile")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Delegated / Follow Up")
                   (org-agenda-breadcrumbs-separator "")
                   (org-agenda-prefix-format '((todo . "  %i %-15b")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Notes to synthesise")
                   (org-agenda-breadcrumbs-separator "")
                   (org-agenda-files (list rk-org--tickler-file))))
            (agenda "")
            (tags-todo "-someday-media-study/NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-breadcrumbs-separator "")
                        (org-agenda-prefix-format '((tags . "%i %-24b")))))
            (stuck ""))
           ((org-agenda-tag-filter-preset '("-ignore"))
            (org-agenda-files (list rk-org--work-projects-file rk-org--inbox-file rk-org--next-file rk-org--tickler-file))
            (org-agenda-show-inherited-tags nil)
            (org-agenda-dim-blocked-tasks nil)
            (org-agenda-archives-mode nil)
            (org-agenda-ignore-drawer-properties '(effort appt))))

          ("p" "Personal"
           ((tags-todo "inbox"
                       ((org-agenda-overriding-header "To Refile")))
            (tags-todo "-someday-media-study/NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-breadcrumbs-separator "")
                        (org-agenda-prefix-format '((tags . "  %i %-15b")))))
            (stuck "")
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (agenda ""))
           ((org-agenda-tag-filter-preset '("-ignore"))
            (org-agenda-files (list rk-org--personal-projects-file rk-org--inbox-file rk-org--next-file rk-org--tickler-file))
            (org-agenda-show-inherited-tags nil)
            (org-agenda-dim-blocked-tasks nil)
            (org-agenda-archives-mode nil)
            (org-agenda-ignore-drawer-properties '(effort appt))))

          ("n" "Next"
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
            (tags-todo "+goals+3m+project"
                       ((org-agenda-overriding-header "Review 3 Month Goals")))
            (tags-todo "+goals+1y+project"
                       ((org-agenda-overriding-header "Review 1 Year Goals")))
            (tags-todo "+goals+3y+project"
                       ((org-agenda-overriding-header "Review 3 Year Goals")))
            (tags-todo "someday"
                       ((org-agenda-overriding-header "Someday"))))
           ((org-agenda-tag-filter-preset
             '("-ignore"))
            (org-agenda-show-log t)
            (org-agenda-files (list rk-org--consume-file rk-org--personal-projects-file rk-org--work-projects-file rk-org--inbox-file rk-org--someday-file rk-org--tickler-file))
            (org-agenda-archives-mode nil)
            (org-agenda-dim-blocked-tasks nil))))))

(use-package rk-org-agenda-transient-state
  :after (org org-agenda)
  :general
  (:keymaps 'org-agenda-mode-map
            "C-." #'rk-org-agenda-hydra-transient-state/body))

(use-package appt
  :defer t
  :config
  (progn
    (setq appt-message-warning-time 60)
    (setq appt-display-interval 5)))

(use-package org-archive
  :after org
  :functions (org-archive-subtree)
  :defines (org-archive-default-command)
  :preface
  (progn
    (autoload 'org-map-entries "org")
    (autoload 'org-set-tags "org")
    (autoload 'org-get-tags "org")

    (defun rk-org--archive-done-tasks ()
      (interactive)
      (atomic-change-group
        (org-map-entries (lambda ()
                           ;; HACK: Ensure point does not move past the next
                           ;; item to archive.
                           (let ((_ (point)))
                             (org-archive-subtree)))
                         "/DONE|PAID|VOID|CANCELLED" 'tree)))

    (defun rk-org--apply-inherited-tags (&rest _)
      "Apply inherited tags when archiving."
      (org-set-tags (org-get-tags))))

  :config
  (progn
    (rk-local-leader-def :keymaps 'org-mode-map
      "c" '(org-archive-subtree :wk "archive"))
    (setq org-archive-default-command #'rk-org--archive-done-tasks)
    (advice-add 'org-archive-subtree :before #'rk-org--apply-inherited-tags)))

(use-package org-src
  :after org
  :defines (org-src-fontify-natively)

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
    (setq org-src-lang-modes '(("ocaml" . tuareg)
                               ("json" . rk-web-json)
                               ("elisp" . emacs-lisp)
                               ("ditaa" . artist)
                               ("asymptote" . asy)
                               ("dot" . fundamental)
                               ("sqlite" . sql)
                               ("calc" . fundamental)
                               ("C" . c)
                               ("cpp" . c++)
                               ("C++" . c++)
                               ("screen" . shell-script)
                               ("shell" . sh)
                               ("javascript" . rk-web-js)
                               ("js" . rk-web-js)
                               ("bash" . sh)))
    (setq org-src-fontify-natively t)
    (setq org-src-window-setup 'current-window)
    (add-hook 'org-src-mode-hook #'rk-org--suppress-final-newline)
    (advice-add 'org-edit-src-exit :before #'rk-org--org-src-delete-trailing-space)))

(use-package org-clock
  :after org
  :defines (org-clock-persist
            org-clock-persist-query-resume
            org-clock-history-length
            org-clock-in-resume
            org-clock-report-include-clocking-task
            org-clock-out-remove-zero-time-clocks
            org-clock-persist-file)

  :preface
  (progn
    (autoload 'org-remove-empty-drawer-at "org")

    (transient-define-prefix org-clock-transient ()
      "Org clock transient"
      ["Actions"
       ("i" "In" org-clock-in)
       ("o" "Out" org-clock-out)
       ("g" "Goto" org-clock-goto)])

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
    (setq org-clock-persist-file (f-join rk-org-roam-dir ".org-clock-save"))
    (rk-leader-def
      "oC" '(org-clock-transient :wk "clock"))

    (org-clock-persistence-insinuate)
    (add-hook 'org-clock-out-hook #'rk-org--remove-empty-clock-drawers t)))

(use-package org-capture
  :after org
  :defines org-capture-list
  :defines (org-capture-templates)
  :preface
  (defun rk-org--capture-template-entry (key label form template &rest kws)
    (append
     (list key label 'entry form template
           :clock-keep t
           :empty-lines 1
           :prepend t)
     kws))
  :init
  (setq org-capture-templates
        (list
         ;; Reg org
         (rk-org--capture-template-entry
          "t" "Add [t]o inbox"
          '(file rk-org--inbox-file) "* TODO %i%?")

         (rk-org--capture-template-entry
          "m" "Add [m]eeting"
          '(file+headline rk-org--tickler-file "Meetings") "* TODO %i%? \n%^t")

         (rk-org--capture-template-entry
          "p" "Create work [p]roject"
          '(file rk-org--work-projects-file) "* TODO %i%? [%] :project:\n%^{CATEGORY}p")

         (rk-org--capture-template-entry
          "P" "Create [P]ersonal project"
          '(file rk-org--personal-projects-file) "* TODO %i%? [%] :project:\n%^{CATEGORY}p")

         (rk-org--capture-template-entry
          "s" "Add task to do [s]omeday"
          '(file+olp rk-org--someday-file "Side projects")
          "* SOMEDAY  %?"))))

(use-package org-download
  :straight t
  :after org
  :config
  (setq org-download-method 'attach))

(use-package evil
  :defer t
  :general
  (:keymaps 'evil-org-mode-map :states '(insert normal)
            "M-o" #'org-insert-subheading)
  :preface
  (progn
    (autoload 'org-at-heading-p "org")
    (autoload 'evil-insert-state "evil-states")

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
    (advice-add 'org-insert-subheading :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading :after #'rk-org--evil-insert-state)

    (advice-add 'org-insert-heading :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-subheading :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading :after #'rk-org--add-blank-line-after-heading)))

(use-package ox
  :after org
  :defines (org-export-exclude-tags
            org-export-coding-system)
  :init
  (defvar org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence))
  :config
  (progn
    (setq org-export-exclude-tags '("noexport" "crypt"))
    (setq org-export-coding-system 'utf-8)))

(use-package ox-gfm
  :straight t
  :after org)

(use-package ox-html
  :after org
  :defines (org-html-html5-fancy
            org-html-postamble
            org-html-table-row-open-tag
            org-html-head-extra)
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
  :commands rk-org-clock-cascade-init
  :config (add-hook 'org-mode-hook #'rk-org-clock-cascade-init))

(use-package typopunct
  :straight t
  :after org
  :preface
  (defun rk-typopunct-init ()
    (typopunct-change-language 'english)
    (typopunct-mode 1))
  :config
  (add-hook 'org-mode-hook #'rk-typopunct-init))

(use-package rk-org-goto
  :config
  (rk-leader-def
    "oa"  '(rk-org-goto-agenda :wk "agenda")
    "og"  '(:ignore t :wk "goto")
    "ogc" '(rk-org-goto-consume :wk "consume")
    "ogt" '(rk-org-goto-tickler :wk "tickler")
    "ogd" '(rk-org-goto-diary :wk "diary")
    "ogi" '(rk-org-goto-inbox :wk "inbox")
    "ogn" '(rk-org-goto-next :wk "next/one-off")
    "ogr" '(rk-org-goto-reference :wk "reference")
    "ogp" '(rk-org-goto-projects :wk "projects")
    "ot"  '(rk-org-goto-todo-list :wk "todos")
    "ov"  '(rk-org-goto-tags-list :wk "tags")))

(use-package evil-org
  :straight t
  :after org
  :demand t
  :preface
  (defun rk-org--return (arg)
    "Like `evil-org-return', but doesn't indent otherwise"
    (interactive "P")
    (cond ((and (not arg) (evil-org--empty-element-p))
           (delete-region (line-beginning-position) (line-end-position)))
          ((eolp)
           (if (bolp)
               (org-return nil)
             (call-interactively #'evil-org-open-below)))
          ('otherwise
           (org-return nil))))

  :general
  (:keymaps 'org-agenda-mode-map :states 'motion
            "j" #'org-agenda-next-line
            "k" #'org-agenda-previous-line
            "M-j" #'org-agenda-next-item
            "M-k" #'org-agenda-previous-item
            "M-h" #'org-agenda-earlier
            "M-l" #'org-agenda-later
            "gd" #'org-agenda-toggle-time-grid
            "gr" #'org-agenda-redo
            "C-f" #'evil-scroll-page-down
            "C-b" #'evil-scroll-page-up
            "M-RET" #'org-agenda-show-and-scroll-up
            "J" #'org-agenda-goto-date)
  (:keymaps 'org-mode-map :states '(insert normal)
            "M-o" #'evil-org-org-insert-subheading-below
            "RET" #'rk-org--return)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects navigation additional shift heading todo))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-indent
  :after org
  :hook (org-mode . org-indent-mode))

(use-package flyspell
  :after org
  :hook (org-mode . flyspell-mode))

(use-package org-bullets
  :straight t
  :disabled t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-superstar-headline-bullets-list '(?○))
  (org-superstar-leading-bullet ?\s)
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . ?☐)
                                     ("NEXT" . ?☐)
                                     ("WAITING" . ?◹)
                                     ("CANCELLED" . ?☒)
                                     ("DONE" . ?☑)))
  :config
  (setf (alist-get 45 org-superstar-item-bullet-alist) ?•))

;; Roam config

(use-package org-roam
  :straight t
  :after org
  :preface
  (defun rk-org-roam--open-with-buffer-maybe ()
    (and (not org-roam-capture--node)
         (not (eq 'visible (org-roam-buffer--visibility)))
         (org-roam-buffer-toggle)))
  (defun rk-org-roam--visit-node-other-window ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'org-roam-node-visit)))
  (defun rk-org-roam--visit-preview-other-window ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'org-roam-preview-visit)))
  (defun rk-org-roam--visit-grep-other-window ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'org-roam-grep-visit)))
  (pretty-hydra-define rk-org-roam--utils
    (:title "Roam Utilities" :quit-key "q")
    ("Refs"
     (("r" org-roam-ref-add "add")
      ("R" org-roam-ref-remove "remove"))
     "Tags"
     (("g" org-roam-tag-add "add")
      ("G" org-roam-tag-remove "remove"))
     "Aliases"
     (("a" org-roam-alias-add "add")
      ("A" org-roam-alias-remove "remove"))
     "Dailies"
     (("C-j" org-roam-dailies-goto-next-note "next")
      ("C-k" org-roam-dailies-goto-previous-note "previous"))))
  :general
  (:keymaps 'org-mode-map
            "C-c i" #'org-roam-node-insert)
  (:keymaps 'org-roam-mode-map :states '(normal)
            "<tab>" #'magit-section-toggle
            "n" #'magit-section-forward
            "p" #'magit-section-backward
            "<return>" #'org-roam-buffer-visit-thing
            "q" #'quit-window)
  (:keymaps 'org-roam-preview-map
            "SPC" nil
            "<return>" #'rk-org-roam--visit-preview-other-window)
  (:keymaps 'org-roam-node-map
            "SPC" nil
            "<return>" #'rk-org-roam--visit-node-other-window)
  (:keymaps 'org-roam-grep-map
            "SPC" nil
            "<return>" #'rk-org-roam--visit-grep-other-window)
  :custom
  (org-roam-directory rk-org-roam-dir)
  :init
  (rk-leader-def
    "ob"  '(org-roam-buffer-toggle :wk "toggle buffer")
    "oB"  '(org-roam-buffer-display-dedicated :wk "toggle dedicated buffer")
    "of"  '(org-roam-node-find :wk "find file node")

    "od"  '(:ignore t :wk "date")
    "odt" '(org-roam-dailies-goto-today :wk "today")
    "odd" '(org-roam-dailies-goto-date :wk "for date")
    "ody" '(org-roam-dailies-goto-yesterday :wk "yesterday")
    "odT" '(org-roam-dailies-goto-tomorrow :wk "tomorrow"))
  (rk-local-leader-def :keymaps 'org-mode-map
    "." '(rk-org-roam--utils/body :wk "roam utils"))
  :config
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (f-split dirs)))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-node-display-template (concat (propertize "${tags:20}" 'face 'org-tag) " ${title:80} " (propertize "${backlinkscount:6}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx "*" "org-roam" (zero-or-more anything) "*")
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (add-hook 'org-roam-find-file-hook #'rk-org-roam--open-with-buffer-maybe :append))

(use-package vulpea
  :straight (vulpea
             :type git
             :host github
             :repo "d12frosted/vulpea")
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Misc

(use-package verb
  :straight t
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; Disable yasnippet in org mode buffers
(use-package yasnippet
  :straight t
  :after org
  :preface
  (defun rk-yas--disable-yas ()
    (yas-minor-mode -1))
  :config
  (add-hook 'org-mode-hook #'rk-yas--disable-yas))

(use-package svg-tag-mode
  :straight t
  :after org
  :init
  (setq svg-tag-tags
        '(("\\(:[a-z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :beg 1 :end -1))))))
  :config
  (add-hook 'org-mode-hook #'svg-tag-mode))

(use-package citar
  :straight t
  :general
  (:keymaps 'org-mode-map :states '(insert)
            "C-c b" #'citar-insert-citation)
  (:keymaps 'minibuffer-local-map
            "C-b" #'citar-insert-preset)
  :preface
  (defvar rk-bib-refs-file (f-join paths--dropbox-dir "org/bib/references.bib"))
  (defvar rk-roam-refs-dir (f-join rk-org-roam-dir "references/"))
  (defun rk-citar--idle-refresh-cache ()
    "Generate bib item caches with idle timer."
    (run-with-idle-timer 0.5 nil #'citar-refresh))
  :custom
  (citar-notes-paths `(,rk-roam-refs-dir))
  (org-cite-global-bibliography `(,rk-bib-refs-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography `(,rk-bib-refs-file))
  :config
  (add-hook 'org-mode-hook #'rk-citar--idle-refresh-cache)
  (add-hook 'LaTeX-mode-hook #'rk-citar--idle-refresh-cache)
  (require 'citar-org))


(provide 'rk-org)

;;; rk-org.el ends here
