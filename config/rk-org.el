;;; rk-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'paths)
(require 'spacemacs-keys)
(require 'evilified-state)
(require 'f)
(require 's)
(require 'dash)
(require 'subr-x)

(autoload 'evil-define-key "evil")
(autoload 'org-todo "org")

(defvar org-directory "~/org")

(defconst rk-org-work-file (concat org-directory "/work_movio.org"))
(defconst rk-org-numero-file (concat org-directory "/numero.org"))
(defconst rk-org-recruitment-file (concat org-directory "/recruitment.org"))
(defconst rk-org-consume-file (concat org-directory "/consume.org"))

(with-eval-after-load 'which-key
  (with-no-warnings
    (push `(("," . ,(rx bos (? "evil-") "org-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package org
  :straight org-plus-contrib
  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ("C-c c" . org-columns))

  :defines (org-state
            org-log-states
            org-log-done
            org-tag-group-re)

  :preface
  (progn
    (autoload 'outline-forward-same-level "outline")
    (autoload 's-matches? "s")

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

  :init
  (progn
    (add-hook 'org-mode-hook #'rk-org--add-local-hooks)
    (add-hook 'org-after-todo-state-change-hook #'rk-org--set-next-todo-state)
    (add-hook 'org-after-todo-statistics-hook #'rk-org--children-done-parent-done)

    (spacemacs-keys-set-leader-keys
      "ok" #'org-capture
      "ol" #'org-store-link
      "os" #'org-search-view))

  :config
  (progn
    (load-file (expand-file-name "rk-org-version.el" (concat paths-lisp-directory "/rk-org")))
    (setq org-default-notes-file (f-join org-directory "notes.org"))

    (spacemacs-keys-set-leader-keys-for-major-mode
      'org-mode
      "A" #'org-align-tags
      "r" #'org-refile
      "d" #'org-deadline
      "C" #'org-ctrl-c-ctrl-c
      "f" #'org-fill-paragraph
      "L" #'org-insert-link
      "s" #'org-schedule)

    (evil-define-key 'normal org-mode-map (kbd "RET") #'org-return)

    (add-to-list 'org-refile-targets '(nil :maxlevel . 3))
    (add-to-list 'org-refile-targets '(org-default-notes-file :maxlevel . 3))
    (add-to-list 'org-refile-targets '(rk-org-consume-file :maxlevel . 3))
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

    (setq org-confirm-babel-evaluate nil)
    (setq org-babel-load-languages '((emacs-lisp . t)
                                     (restclient . t)
                                     (gnuplot . t)
                                     (python . t)
                                     (javascript . t)
                                     (scala . t)
                                     (shell . t)
                                     (shell . t)
                                     (sql . t)))

    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

    (advice-add 'org-add-log-note :before #'rk-org--exit-minibuffer)
    (advice-add 'org-toggle-heading :after #'rk-org--toggle-heading-goto-eol)))

(use-package ob-python
  :after org
  :preface
  (defun cb-org-setup-python ()
    (when (executable-find "ipython")
      (setq-local org-babel-python-command "ipython")))
  :config
  (add-hook 'org-mode-hook #'cb-org-setup-python))

(use-package gnuplot
  :straight t
  :defer t)

(use-package ob-gnuplot
  :after org)

(use-package ob-restclient
  :straight t
  :after org)

(use-package ob-javascript
  :straight (:host github :repo "zweifisch/ob-javascript"
                   :branch "master")
  :after org
  :defines (ob-javascript-path-to-lib)
  :init
  (setq ob-javascript-path-to-lib "/Users/raghuvirk/.emacs.d/straight/repos/ob-javascript/"))

(use-package ob-shell :after org)

(use-package ob-sql :after org)

(use-package org-id
  :after org
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
  :config
  (setq org-attach-directory (f-join org-directory "data")))

(use-package org-agenda
  :after org
  :bind (:map org-agenda-mode-map ("J" . org-agenda-goto-date))

  :preface
  (defun rk-org--exclude-tasks-on-hold (tag)
    (and (equal tag "hold") (concat "-" tag)))

  :defines (org-duration-format)

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

    (setq org-duration-format
          (list :hours "%d" :require-hours t
                :minutes ":%02d" :require-minutes t))

    (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

    (use-package rk-org-agenda-transient-state
      :after org
      :commands (rk-org-agenda-hydra-transient-state/body)
      :config
      (progn
        (define-key org-agenda-mode-map (kbd "C-.") #'rk-org-agenda-hydra-transient-state/body)))

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

            ("p" "Personal actions"
             ((tags-todo "-someday-media-study-@consume/NEXT"
                         ((org-agenda-overriding-header "Next Actions")))
              (tags-todo "+@consume+@read/NEXT"
                         ((org-agenda-overriding-header "Read next")))
              (tags-todo "+@consume+@watch/NEXT"
                         ((org-agenda-overriding-header "Watch next")))
              (tags-todo "+@consume+@eat/NEXT"
                         ((org-agenda-overriding-header "Eat next")))
              (tags-todo "+@consume+@listen/NEXT"
                         ((org-agenda-overriding-header "Listen next")))
              (tags-todo "+@consume+@experience/NEXT"
                         ((org-agenda-overriding-header "Experience next")))
              (agenda "")
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting")))
              (stuck ""))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-use-tag-inheritance nil)
              (org-agenda-files (list org-agenda-diary-file rk-org-consume-file))
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
              (org-agenda-files (list rk-org-work-file))
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
  :functions (org-archive-subtree)
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
                           (let ((org-map-continue-from (point)))
                             (org-archive-subtree)))
                         "/DONE|PAID|VOID|CANCELLED" 'tree)))

    (defun rk-org--apply-inherited-tags (&rest _)
      "Apply inherited tags when archiving."
      (org-set-tags (org-get-tags))))

  :config
  (progn
    (setq org-archive-default-command #'rk-org--archive-done-tasks)
    (advice-add 'org-archive-subtree :before #'rk-org--apply-inherited-tags)))

(use-package org-src
  :after org

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
                               ("bash" . sh)))
    (setq org-src-fontify-natively t)
    (setq org-src-window-setup 'current-window)
    (add-hook 'org-src-mode-hook #'rk-org--suppress-final-newline)
    (advice-add 'org-edit-src-exit :before #'rk-org--org-src-delete-trailing-space)))

(use-package org-clock
  :after org

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

(use-package org-capture
  :after org
  :preface
  (defun rk-org--capture-template-entry (key label form template &rest kws)
    (append
     (list key label 'entry form template
           :clock-keep t
           :empty-lines 1
           :prepend t)
     kws))
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-minor-mode 'org-capture-mode
      "d" #'org-deadline
      "c" #'org-capture-finalize
      "k" #'org-capture-kill
      "r" #'org-capture-refile
      "A" #'org-align-tags)
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
            '(file+olp rk-org-work-file "Reporting") "*  %?")

           (rk-org--capture-template-entry
            "e" "Editors"
            '(file+olp rk-org-work-file "Editors") "*  %?")

           (rk-org--capture-template-entry
            "o" "Operations"
            '(file+olp rk-org-work-file "Operations") "*  %?")

           (rk-org--capture-template-entry
            "y" "Design System"
            '(file+olp rk-org-work-file "Design System") "*  %?")

           (rk-org--capture-template-entry
            "i" "Infra"
            '(file+olp rk-org-work-file "Infra") "*  %?")

           (rk-org--capture-template-entry
            "E" "ETL"
            '(file+olp rk-org-work-file "ETL") "*  %?")

           (rk-org--capture-template-entry
            "l" "Legacy"
            '(file+olp rk-org-work-file "Legacy") "*  %?")

           (rk-org--capture-template-entry
            "R" "Recruitment"
            '(file+olp rk-org-work-file "Recruitment") "*  %?")

           (rk-org--capture-template-entry
            "t" "Team Lead"
            '(file+olp rk-org-work-file "Team Lead") "*  %?")

           (rk-org--capture-template-entry
            "S" "Standup"
            `(file+datetree rk-org-work-file) "* TODO %? :standup:\n%^t")

           (rk-org--capture-template-entry
            "s" "Someday"
            '(file+olp org-agenda-diary-file "Side projects")
            "* SOMEDAY  %?")))))

(use-package org-download
  :straight t
  :after org
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
    (advice-add 'org-insert-subheading :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading :after #'rk-org--evil-insert-state)

    (advice-add 'org-insert-heading :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-subheading :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading :after #'rk-org--add-blank-line-after-heading)

    (evil-define-key 'normal evil-org-mode-map (kbd "M-o") #'org-insert-subheading)
    (evil-define-key 'insert evil-org-mode-map (kbd "M-o") #'org-insert-subheading)))

(use-package ox
  :after org
  :init
  (defvar org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence))
  :config
  (progn
    (setq org-export-exclude-tags '("noexport" "crypt"))
    (setq org-export-coding-system 'utf-8)))

(use-package ox-gfm
  :straight t
  :after org)

(use-package ox-confluence
  :after org
  :config
  (org-export-define-derived-backend 'custom-confluence 'confluence
    :menu-entry
    '(?c "Export as Confluence markup"
         ((?c "To temporary buffer" org-confluence-export-as-confluence)))))

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

(use-package typopunct
  :straight t
  :after org
  :commands (typopunct-change-language typopunct-mode)
  :preface
  (defun rk-typopunct-init ()
    (typopunct-change-language 'english)
    (typopunct-mode 1))
  :config
  (add-hook 'org-mode-hook #'rk-typopunct-init))

(use-package rk-org-capture-url
  :after org)

(use-package rk-org-goto
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

(use-package rk-org-jira-url
  :after org
  :config
  (spacemacs-keys-set-leader-keys-for-major-mode 'org-mode
    "j" #'rk-org--create-jira-url))

(use-package rk-org-ctrl-c-ctrl-k
  :after org
  :config
  (evil-define-key 'normal org-mode-map (kbd "C-c C-k") #'rk-org-ctrl-c-ctrl-k))

(use-package rk-diary-utils
  :after org)

(use-package evil-org
  :straight t
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
  :init
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package flyspell
  :after org
  :init
  (add-hook 'org-mode-hook #'flyspell-mode))

(provide 'rk-org)

;;; rk-org.el ends here
