;;; rk-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'general)
(require 'definers)
(require 'f)
(require 's)
(require 'dash)
(require 'transient)

(defvar org-directory paths--org-dir)

(use-package org
  :straight t
  :demand t
  :general
  (:keymaps 'image-map
            "o" #'evil-open-below)
  (:keymaps 'org-mode-map
            "C-c l" #'org-insert-last-stored-link
            "C-n" #'org-next-visible-heading
            "C-p" #'org-previous-visible-heading
            "C-c t" #'org-table-create-or-convert-from-region
            "M-p"     #'org-metaup
            "M-n"     #'org-metadown)
  (:keymaps 'org-mode-map :states '(normal visual motion)
            "gb" #'org-mark-ring-goto
            "C-n" #'org-next-visible-heading
            "C-p" #'org-previous-visible-heading)
  (:keymaps 'org-mode-map :states '(visual)
            "-" #'rk-org--deemphasize
            "B" #'rk-org--embolden
            "_" #'rk-org--underline
            "/" #'rk-org--italicize
            "+" #'rk-org--strike-through
            "=" #'rk-org--quote)
  (:keymaps 'org-read-date-minibuffer-local-map
            "C-n" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
            "C-p" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
            "C-t" (lambda () (interactive) (org-eval-in-calendar '(calendar-goto-today)))
            "C-j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1)))
            "C-k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
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

  (defun rk-org--sort-tasks ()
    (when (eq 'org-mode major-mode)
      (with-current-buffer (current-buffer)
        (if-let ((marker (org-find-exact-headline-in-buffer "Tasks")))
            (save-excursion
              (goto-char (org-find-exact-headline-in-buffer "Tasks"))
              (org-sort-entries t ?t)
              (org-cycle)
              (org-cycle))))))

  (defun rk-org--exit-minibuffer (&rest _)
    "Exit minibuffer before adding notes."
    (when (minibufferp (window-buffer (selected-window)))
      (other-window 1)))

  (defun rk-org--toggle-heading-goto-eol (&rest _)
    "Prevent point from moving to BOL when toggling headings."
    (when (s-matches? (rx bol (+ "*") (* space) eol)
                      (buffer-substring (line-beginning-position) (line-end-position)))
      (goto-char (line-end-position))))



  (defun rk-org--disable-ligatures ()
    (when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode -1)))

  (defun rk-org--setup-org ()
    (flyspell-mode -1)
    (rk-org--disable-ligatures))

  (defun rk-org--set-next-todo-state ()
    "When marking a todo to DONE, set the next TODO as NEXT.
Do not scheduled items or repeating todos."
    (save-excursion
      (when (and (string= org-state "DONE")
                 (org-goto-sibling)
                 (-contains? '("TODO") (org-get-todo-state)))
        (org-todo "NEXT"))))

  (defun rk-org--set-subsequent-siblings-todo-state ()
    "When marking a todo to NEXT, mark all subsequent todos as TODO."
    (save-excursion
      (when (string= org-state "NEXT")
        (while (org-goto-sibling)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-todo "TODO"))))))

  (defun rk-org--sort-headings-mark-first-next ()
    "Sort headings under tasks and mark first as NEXT."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (search-forward "* Tasks")
      (move-beginning-of-line nil)
      (org-sort-entries nil ?d)
      (org-next-visible-heading 1)
      (org-todo "NEXT")))

  :custom
  (org-image-actual-width nil)
  (org-tags-exclude-from-inheritance '("project"))
  (org-M-RET-may-split-line nil)
  (org-catch-invisible-edits 'smart)
  (org-cycle-separator-lines 1)
  (org-enforce-todo-dependencies t)
  (org-footnote-auto-adjust t)
  (org-indirect-buffer-display 'current-window)
  (org-insert-heading-respect-content t)
  (org-link-abbrev-alist '(("att" . org-attach-expand-link)))
  (org-log-done 'time)
  (org-use-sub-superscripts '{})
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-return-follows-link t)
  (org-reverse-note-order nil)
  (org-confirm-elisp-link-function nil)
  (org-startup-indented nil)
  (org-startup-with-inline-images t)
  (org-hierarchical-todo-statistics nil)
  (org-checkbox-hierarchical-statistics t)
  (org-log-repeat nil)
  (org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))

  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                       (sequence "SOMEDAY(o)" "|")
                       (sequence "SCHEDULE(s)" "|")))

  (org-confirm-babel-evaluate nil)

  :init
  (rk-leader-def
    "ns" '(org-narrow-to-subtree :wk "narrow to subtree")
    "ol" '(org-store-link :wk "store link")
    "ous" '(org-save-all-org-buffers :wk "save all"))

  :hook ((org-mode . rk-org--setup-org))
  :config
  (add-hook 'org-after-todo-state-change-hook #'rk-org--set-subsequent-siblings-todo-state)
  (add-hook 'org-after-todo-state-change-hook #'rk-org--set-next-todo-state)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (rk-local-leader-def :keymaps 'org-mode-map
    "d" '(org-deadline :wk "deadline")
    "s" '(org-schedule :wk "schedule")

    "u"  '(:ignore t :wk "utils")

    "l"  '(org-insert-link :wk "insert link"))

  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

  (advice-add 'org-add-log-note :before #'rk-org--exit-minibuffer)
  (advice-add 'org-toggle-heading :after #'rk-org--toggle-heading-goto-eol))

(use-package org-capture
  :after org
  :preface
  :init
  (rk-leader-def
    "."  '(org-capture :wk "capture")))

(use-package ob-python
  :after org
  :preface
  (defun rk-org-setup-python ()
    (when (executable-find "ipython")
      (setq-local org-babel-python-command "ipython")))
  :config
  (add-hook 'org-mode-hook #'rk-org-setup-python))

(use-package gnuplot
  :straight t)

(use-package ob
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages '((emacs-lisp . t)
                              (sql . t)
                              (python . t)
                              (dot . t)
                              (shell . t)
                              (mermaid . t)))
  (org-babel-python-command (executable-find "python3")))

(use-package org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images t))

(use-package org
  :config
  (defun rk-org--suppress-final-newline ()
    (setq-local require-final-newline nil))

  (defun rk-org--delete-trailing-space-on-src-block-exit (&rest _)
    (delete-trailing-whitespace))

  (add-hook 'org-src-mode-hook 'rk-org--suppress-final-newline)
  (advice-add 'org-edit-src-exit :before 'rk-org--delete-trailing-space-on-src-block-exit))

(use-package ob-gnuplot
  :after org)

(use-package ob-restclient
  :straight t
  :after org)

(use-package ob-mermaid
  :straight t
  :after org)

(use-package ob-shell
  :after org)

(use-package ob-sql
  :after org)

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
  (org-attach-id-dir rk-org--data-dir))

(use-package org-agenda
  :after org
  :preface
  (defun rk-org--exclude-tasks-on-hold (tag)
    (and (equal tag "hold") (concat "-" tag)))
  (defun rk-org--general-agenda ()
    (interactive)
    (let ((agenda-key "g"))
      (org-agenda current-prefix-arg agenda-key)
      (delete-other-windows)))
  (defun org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
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
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))

  :custom
  (org-stuck-projects '("-ignore-maybe-done"
                        ("NEXT" "WAITING") nil
                        ""))

  (org-agenda-include-diary nil)
  (org-agenda-start-on-weekday nil)
  (org-agenda-auto-exclude-function #'rk-org--exclude-tasks-on-hold)
  (org-agenda-hide-tags-regexp (rx (or "noexport" "someday" "project")))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-span 'week)
  (org-agenda-search-view-always-boolean t)
  (org-agenda-show-all-dates nil)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-sorting-strategy
   '((agenda time-up priority-down category-keep)
     (todo priority-down category-keep scheduled-up)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-use-time-grid nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-tags-column -100)
  (org-agenda-clockreport-parameter-plist
   (list
    :compact t
    :maxlevel 5
    :fileskip0 t
    :step 'week))
  :init
  (rk-leader-def
    "o a"   '(rk-org--general-agenda :wk "agenda")
    "o A"   '(org-agenda :wk "all agendas"))
  :config
  (rk-local-leader-def :keymaps 'org-agenda-mode-map
    "d" '(org-agenda-deadline :wk "deadline")
    "p" '(org-agenda-set-property :wk "set property"))

  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)
  (add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt))

(use-package org
  :straight t
  :after (vulpea)
  :preface
  (defcustom rk-org--refile-headline "Tasks"
    "The headline to refile to in project files.")

  (defun rk-org--refile ()
    (interactive)
    (let* ((notes (vulpea-db-query-by-tags-some '("project")))
           (completions (seq-map (lambda (n) (cons (vulpea-select-describe n) n)) notes))
           (note (completing-read
                  "Refile to: "
                  completions nil t))
           (v-note (cdr (assoc note completions)))
           (file (vulpea-note-path v-note))
           (pos (save-excursion
                  (find-file file)
                  (org-find-exact-headline-in-buffer rk-org--refile-headline))))
      (org-refile nil nil (list rk-org--refile-headline file nil pos))
      (switch-to-buffer (current-buffer))))

  (defun rk-org-agenda--refile ()
    (interactive)
    (let* ((notes (vulpea-db-query-by-tags-some '("project")))
           (completions (seq-map (lambda (n) (cons (vulpea-select-describe n) n)) notes))
           (note (completing-read
                  "Refile to: "
                  completions nil t))
           (v-note (cdr (assoc note completions)))
           (file (vulpea-note-path v-note))
           (pos (save-excursion
                  (find-file file)
                  (org-find-exact-headline-in-buffer rk-org--refile-headline))))
      (org-agenda-refile nil (list rk-org--refile-headline file nil pos))
      (switch-to-buffer (current-buffer))))
  :config
  (rk-local-leader-def :keymaps 'org-mode-map
    "R" '(rk-org--refile :wk "refile"))
  (rk-local-leader-def :keymaps 'org-agenda-mode-map
    "r" '(rk-org-agenda--refile :wk "refile")))

(use-package org-archive
  :after org
  :functions (org-archive-subtree)
  :custom
  (org-archive-default-command #'rk-org--archive-done-tasks)
  :preface
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
    (org-set-tags (org-get-tags)))

  :config
  (rk-local-leader-def :keymaps 'org-mode-map
    "ua" '(org-archive-subtree :wk "archive"))
  (advice-add 'org-archive-subtree :before #'rk-org--apply-inherited-tags))

(use-package org-src
  :after org
  :defines (org-src-fontify-natively)

  :preface
  (defun rk-org--suppress-final-newline ()
    "Remove trailing newline in src blocks."
    (setq-local require-final-newline nil))

  (defun rk-org--org-src-delete-trailing-space (&rest _)
    "Delete trailing whitespace when exiting src blocks."
    (delete-trailing-whitespace))

  :config
  (setq org-src-lang-modes '(("ocaml" . tuareg)
                             ("json" . json-ts)
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
                             ("javascript" . js-ts)
                             ("js" . js-ts)
                             ("bash" . bash-ts)))
  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'current-window)
  (add-hook 'org-src-mode-hook #'rk-org--suppress-final-newline)
  (advice-add 'org-edit-src-exit :before #'rk-org--org-src-delete-trailing-space))

(use-package org-clock
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-history-length 20)
  (org-clock-in-resume t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist-file (f-join rk-org--roam-dir ".org-clock-save"))

  :preface
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
      (org-remove-empty-drawer-at (point))))

  :config
  (rk-local-leader-def :keymaps 'org-mode-map
    "uc" '(org-clock-transient :wk "clock"))
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-out-hook #'rk-org--remove-empty-clock-drawers t))

(use-package org-download
  :straight t
  :after org
  :general
  (:keymaps 'org-mode-map
            "C-c C-i" #'rk-org-download--insert-screenshot)
  :preface
  (defun rk-org-download--insert-screenshot ()
    (interactive)
    (let ((org-download-image-org-width (read-number "Desired width: " 600)))
      (org-download-screenshot)))
  :custom
  (org-download-method 'directory)
  (org-download-heading-lvl nil)
  (org-download-image-dir (f-join paths--org-dir "images"))
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-screenshot-method "screencapture -i %s"))

(use-package evil
  :straight t
  :after org
  :general
  (:keymaps 'evil-org-mode-map :states '(insert normal)
            "M-o" #'org-insert-subheading)
  :preface
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
            (open-line 1))))))

  :config
  (advice-add 'org-insert-heading :after #'rk-org--evil-insert-state)
  (advice-add 'org-insert-subheading :after #'rk-org--evil-insert-state)
  (advice-add 'org-insert-heading-respect-content :after #'rk-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading :after #'rk-org--evil-insert-state)

  (advice-add 'org-insert-heading :after #'rk-org--add-blank-line-after-heading)
  (advice-add 'org-insert-subheading :after #'rk-org--add-blank-line-after-heading)
  (advice-add 'org-insert-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
  (advice-add 'org-insert-todo-heading-respect-content :after #'rk-org--add-blank-line-after-heading)
  (advice-add 'org-insert-todo-heading :after #'rk-org--add-blank-line-after-heading))

(use-package ox
  :after org
  :custom
  (org-export-exclude-tags '("noexport" "crypt"))
  (org-export-coding-system 'utf-8)
  :init
  (defvar org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence)))

(use-package ox-gfm
  :straight t
  :after org)

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
  :custom
  (org-html-html5-fancy t)
  (org-html-postamble nil)

  ;; Highlight alternating rows in HTML tables.

  (org-html-table-row-open-tag #'rk-org-html-open-tags-setup)
  (org-html-head-extra
   "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
"))

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
    "o g"  '(:ignore t :wk "goto")
    "o g t"  '(rk-org-goto-todo-list :wk "todos")
    "o g v"  '(rk-org-goto-tags-list :wk "tags")
    "o g i"  '(rk-org-goto-inbox :wk "inbox")))

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
  (:keymaps 'org-mode-map :states '(insert normal motion)
            "M-o" #'evil-org-org-insert-subheading-below
            "TAB" #'org-cycle
            "RET" #'rk-org--return)
  :config
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects navigation additional shift heading todo))))
  (evil-org-agenda-set-keys))

(use-package flyspell
  :after org
  :hook (org-mode . flyspell-mode))

;; Roam config

(use-package org-roam
  :straight (:type git
                   :host github
                   :repo "org-roam/org-roam"
                   :branch "main")
  :after org
  :preface
  (defvar rk-org-roam--dailies-prev-buffer nil)
  (defun rk-org-roam--move-to-end ()
    (goto-char (point-max)))
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
  (defun rk-org-roam--set-created-timestamp ()
    (org-with-wide-buffer
     (unless (org-entry-get (point) "CREATED")
       (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))))
  (pretty-hydra-define rk-org-roam--daily-utils
    (:title "Roam Utilities" :quit-key "q"
            :foreign-keys run)
    ("Dailies"
     (("C-n"  org-roam-dailies-goto-today "today")
      ("C-y"  org-roam-dailies-goto-yesterday "yesterday")
      ("C-t"  org-roam-dailies-goto-tomorrow "tomorrow")
      ("C-j" org-roam-dailies-goto-next-note "next")
      ("C-k" org-roam-dailies-goto-previous-note "previous"))))
  :hook
  (org-roam-dailies-find-file . rk-org-roam--move-to-end)
  :general
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
  (org-roam-file-extensions '("org" "org_archive"))
  (org-roam-directory rk-org--roam-dir)
  (org-roam-dailies-capture-templates '(("d" "default" plain "" :target
                                         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n* Habits\n\n- exercise :: %(format \"%s\" (y-or-n-p \"Have you exercised today?\"))\n\n* Food\n\n* General\n\n* Work\n\n"))))
  :init
  (rk-leader-def
    "od"  '(:ignore t :wk "date")
    "odD" '(rk-org-roam--daily-utils/body :wk "dailies hydra")
    "odt" '(org-roam-dailies-goto-today :wk "today")
    "odd" '(org-roam-dailies-goto-date :wk "for date")
    "ody" '(org-roam-dailies-goto-yesterday :wk "yesterday")
    "odT" '(org-roam-dailies-goto-tomorrow :wk "tomorrow"))
  :config
  (rk-local-leader-def :keymaps 'org-mode-map
    "t" '(:ignore t :wk "tags")
    "ta" '(org-roam-tag-add :wk "add filetag(s)")
    "tx" '(org-roam-tag-remove :wk "remove filetag(s)"))
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
  (add-hook 'org-roam-capture-new-node-hook #'rk-org-roam--set-created-timestamp)
  (add-to-list 'display-buffer-alist
               `(,(rx "*" "org-roam" (zero-or-more anything) "*")
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package vulpea
  :straight (vulpea
             :type git
             :host github
             :repo "d12frosted/vulpea")
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :after (org-ql)
  :preface
  (defcustom rk-org--work-file-tag "canva"
    "The file tag to use for work-related org roam files.")
  (defun rk-org--filter-non-diary-notes (note)
    (and (not (f-child-of-p (vulpea-note-path note) rk-org--roam-dailies-dir))
         (not (f-child-of-p (vulpea-note-path note) rk-org--roam-refs-dir))))

  (defun vulpea-active-project-p ()
    "Return non-nil if current buffer is a Vulpea project and has
non-done status."
    (let ((p-status (vulpea-buffer-meta-get! (vulpea-buffer-meta) "status")))
      (or (s-equals? p-status "in progress")
          (s-equals? p-status "draft"))))

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (let ((p-stats (vulpea-buffer-meta-get! (vulpea-buffer-meta) "status")))
      (or (s-equals? (vulpea-buffer-prop-get "title") "Inbox")
          (vulpea-active-project-p)
          (seq-find
           (lambda (type)
             (eq type 'todo))
           (org-element-map
               (org-element-parse-buffer 'headline)
               'headline
             (lambda (h)
               (org-element-property :todo-type h)))))))

  (defun rk-vulpea--project-status ()
    (completing-read "Project status: "
                     '(("draft" 1) ("in progress" 2) ("abandoned" 3) ("complete" 4))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (eq 'org-mode major-mode)
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))

          (if (vulpea-project-p)
              (progn
                (setq tags (cons "project" tags)))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (defun rk-vulpea--org-roam-file-name (title)
    "Return the slug of NODE."
    (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                   (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                   (string-glyph-compose
                    (apply #'string (seq-remove #'nonspacing-mark-p
                                                (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                   (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ("__*" . "_") ("^_" . "") ("_$" . "")))
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs))
               (ts (format-time-string "%Y%m%d%H%M%S")))
          (expand-file-name (format "%s-%s.org" ts (downcase slug)) org-roam-directory)))))

  (defun rk-vulpea--person-to-tag (title)
    (concat "@" (s-replace " " "" title)))

  (defun rk-vulpea--create (title &optional insert-p jump-to)
    (let* ((type (completing-read "Note type: "
                                  '(("default" 1) ("person" 2) ("project" 3) ("article" 4) ("idea" 5) ("org struct" 6) ("technology" 7))
                                  nil t))
           (work-p (y-or-n-p "Is this note work related?"))
           (work-tag (if work-p rk-org--work-file-tag nil))
           (tags (remq nil (list work-tag)))
           (note (cond
                  ((string= type "person")
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :properties '(("CATEGORY" . "person"))
                    :tags (-concat tags `("person" ,(rk-vulpea--person-to-tag title)))
                    :body "* Metadata\n\n- type :: person\n\n* Notes\n\n* Events\n\n"
                    :immediate-finish t))
                  ((string= type "article")
                   (let* ((url (read-string "URL: "))
                          (domain (ignore-errors (url-domain (url-generic-parse-url url))))
                          (desc (if domain
                                    (org-link-make-string url domain)
                                  url)))
                     (vulpea-create
                      title
                      (rk-vulpea--org-roam-file-name title)
                      :tags (-concat tags '("article"))
                      :body (format "* Metadata\n\n- url :: %s\n\n* Notes\n\n" desc)
                      :immediate-finish t)))
                  ((string= type "project")
                   (let ((status (rk-vulpea--project-status)))
                     (vulpea-create
                      title
                      (rk-vulpea--org-roam-file-name title)
                      :tags (-concat tags '("project"))
                      :body (format "* Metadata\n\n- status :: %s\n\n* Description\n\n* Tasks\n\n** NEXT Fill out description\n\n" status)
                      :immediate-finish t)))
                  ((string= type "idea")
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :tags (-concat tags '("idea"))
                    :body (format "* Metadata\n\n- type :: idea\n\n* Abstract\n\n* Notes\n\n* Related reading")
                    :immediate-finish t))
                  ((string= type "org struct")
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :tags (-concat tags '("org"))
                    :body (format "* Metadata\n\n- type :: org structure\n\n* Description\n\n")
                    :immediate-finish t))
                  ((string= type "technology")
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :tags (-concat tags '("technology"))
                    :body (format "* Metadata\n\n- type :: technology\n\n* Description\n\n* Notes\n\n")
                    :immediate-finish t))
                  (t
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :immediate-finish t
                    :tags tags)))))
      (progn
        (vulpea-utils-with-note note
          (org-align-tags t)
          (save-buffer))
        (when insert-p
          (insert (org-link-make-string (concat "id:" (vulpea-note-id note)) title)))
        (when jump-to
          (find-file (vulpea-note-path note))))))

  (defun rk-vulpea--insert (&optional filter-fn)
    "Select a node and insert a link to it.

If the note doesn't exist, offer a set of options to create one.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'."
    (interactive)
    (unwind-protect
        (atomic-change-group
          (let* (region-text
                 beg end
                 (_ (when (region-active-p)
                      (setq
                       beg (set-marker (make-marker) (region-beginning))
                       end (set-marker (make-marker) (region-end))
                       region-text
                       (org-link-display-format (buffer-substring-no-properties beg end)))))
                 (note (vulpea-select
                        "Note"
                        :filter-fn (or filter-fn vulpea-insert-default-filter)
                        :initial-prompt region-text))
                 (title (or region-text
                            (vulpea-note-title note))))
            (if (vulpea-note-id note)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  (insert (org-link-make-string
                           (concat "id:" (vulpea-note-id note))
                           title))
                  (run-hook-with-args
                   'vulpea-insert-handle-functions
                   note))
              (rk-vulpea--create title t))))))

  (cl-defun rk-vulpea--find (&key filter-fn initial-prompt)
    (interactive)
    (let* ((region-text
            (when (region-active-p)
              (org-link-display-format
               (buffer-substring-no-properties
                (set-marker
                 (make-marker) (region-beginning))
                (set-marker
                 (make-marker) (region-end))))))
           (prompt (or initial-prompt region-text))
           (note (vulpea-select-from
                  "Note"
                  (funcall
                   vulpea-find-default-candidates-source
                   (or
                    filter-fn
                    vulpea-find-default-filter))
                  :initial-prompt prompt))
           (title (or prompt
                      region-text
                      (vulpea-note-title note))))
      (if (vulpea-note-id note)
          (org-roam-node-visit
           (org-roam-node-from-id (vulpea-note-id note)))
        (rk-vulpea--create title nil t))))

  (defun rk-org--non-diary-notes (&optional initial-prompt)
    (interactive)
    (rk-vulpea--find :filter-fn #'rk-org--filter-non-diary-notes :initial-prompt initial-prompt))

  (defun rk-org--project-notes ()
    (interactive)
    (funcall #'rk-org--non-diary-notes "#project"))

  (defun rk-vulpea--insert-handle (note)
    "Hook to be called on NOTE after `vulpea-insert'."
    (when-let* ((title (vulpea-note-title note))
                (tags (vulpea-note-tags note)))
      (when (seq-contains-p tags "person")
        (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (when (eq 'todo (org-element-property
                             :todo-type
                             (org-element-at-point)))
              (org-set-tags
               (seq-uniq
                (cons
                 (rk-vulpea--person-to-tag title)
                 (org-get-tags nil t))))))))))

  (defun rk-vulpea--update-project-status ()
    (interactive)
    (if-let* ((project-p (or (vulpea-project-p)
                             (vulpea-buffer-meta-get! (vulpea-buffer-meta) "status" 'string)))
              (status (rk-vulpea--project-status)))
        (vulpea-buffer-meta-set "status" status)
      (message "Can only set project status in PROJECTs")))

  :general
  (:keymaps 'org-mode-map
            "C-c i" #'rk-vulpea--insert)
  :init
  (rk-leader-def
    "of" '(rk-org--non-diary-notes :wk "find file node")
    "oP" '(rk-org--project-notes :wk "find projects"))
  :config
  (org-roam-db-sync)
  (rk-local-leader-def :keymaps 'org-mode-map
    "b" '(vulpea-find-backlink :wk "find backlinks")
    "m"   '(:ignore t :wk "meta")
    "m a" '(vulpea-meta-add :wk "add")
    "m A" '(vulpea-meta-add-list :wk "add list")
    "m x" '(vulpea-meta-remove :wk "remove")
    "m X" '(vulpea-meta-clean :wk "remove all")
    "m p" '(rk-vulpea--update-project-status :wk "project status"))
  (add-hook 'vulpea-insert-handle-functions #'rk-vulpea--insert-handle)

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update))

(use-package org-ql
  :straight t)

(use-package org-capture
  :after (vulpea org-ql)
  :preface
  (autoload 'org-element-at-point "org-element")
  (autoload 'org-duration-from-minutes "org-duration")

  (defun rk-org--capture-to-inbox ()
    "Capture TODO to inbox."
    (interactive)
    (let* ((org-capture-templates `(("t" "*file*" entry (file ,rk-org--roam-inbox)
                                     "* TODO %?\n" :empty-lines 1))))
      (org-capture nil "t")))
  :config
  (rk-leader-def
    "o ." '(rk-org--capture-to-inbox :wk "capture to inbox")))

(use-package vulpea
  :straight t
  :after org-agenda
  :preface
  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (vulpea-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  (if (string= category "???")
                      title
                    category))
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))
  (defun vulpea-agenda-person ()
    "Show main `org-agenda' view."
    (interactive)
    (let* ((person (vulpea-select
                    "Person"
                    :filter-fn
                    (lambda (note)
                      (seq-contains-p (vulpea-note-tags note)
                                      "person"))))
           (node (org-roam-node-from-id (vulpea-note-id person)))
           (names (cons (org-roam-node-title node)
                        (org-roam-node-aliases node)))
           (tags (seq-map #'rk-vulpea--person-to-tag names))
           (query (string-join tags "|")))
      (dlet ((org-agenda-overriding-arguments (list t query)))
        (org-agenda nil "M"))))
  :init
  (rk-leader-def
    "o p" '(vulpea-agenda-person :wk "for person"))
  :config
  (setq org-agenda-custom-commands
        '(("g" "General"
           ((org-ql-block '(todo)
                          ((org-ql-block-header "To Refile")
                           (org-agenda-files `(,(f-join org-roam-directory "20220128063937-inbox.org")))))
            (org-ql-block '(and (todo "NEXT")
                                (not (planning))
                                (heading "Fill out description"))
                          ((org-ql-block-header "Projects to kick-off")))
            (org-ql-block '(and (todo "NEXT" "TODO")
                                (not (planning))
                                (not (heading "Fill out description")))
                          ((org-ql-block-header "Unplanned TODO/NEXT actions")))
            (org-ql-block '(and (heading "Tasks")
                                (descendants (todo))
                                (not (descendants (todo "NEXT" "WAITING"))))
                          ((org-ql-block-header "Stuck projects")))
            (org-ql-block '(todo "WAITING")
                          ((org-ql-block-header "Waiting")))
            (agenda ""))
           ((org-agenda-files (vulpea-project-files))
            (org-agenda-prefix-format '((agenda . " %i %(vulpea-agenda-category 24)%?-24t% s")
                                        (todo . " %i %(vulpea-agenda-category 24)%?-24t% s")
                                        (tags . " %i %(vulpea-agenda-category 24)%?-24t% s")
                                        (search . " %i %(vulpea-agenda-category 24)%?-24t% s"))))))))

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

(use-package ebib
  :straight t
  :custom
  (ebib-preload-bib-files `(,rk-bib--refs-file)))

(use-package citar
  :straight t
  :general
  (:keymaps 'org-mode-map :states '(insert)
            "C-c b" #'org-cite-insert)
  (:keymaps 'minibuffer-local-map
            "C-b" #'citar-insert-preset)
  :preface
  (autoload #'org-roam-review-set-seedling "org-roam-review")
  (autoload #'vulpea-buffer-title-set "vulpea-buffer")

  (defun rk-citar--goto-bib ()
    "Open the bib file."
    (interactive)
    (find-file rk-bib--refs-file))

  (defun rk-citar--format-note (key entry)
    (let* ((template "${title}; ${author editor}")
           (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
           (note-meta
            (citar-format--entry template entry))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
        (erase-buffer)
        (citar-org-roam-make-preamble key)
        (vulpea-buffer-title-set note-meta)
        (call-interactively #'org-roam-review-set-seedling)
        (goto-char (point-max))
        (insert "\n* First read\n\n* Key insights\n\n* Further thinking required")
        (evil-insert 1))))
  :custom
  (citar-notes-paths `(,rk-org--roam-refs-dir))
  (citar-library-paths `(,rk-bib--lib-dir))
  (org-cite-global-bibliography `(,rk-bib--refs-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography `(,rk-bib--refs-file))
  (citar-link-fields '((doi . "https://doi.org/%s")
                       (pmid . "https://www.ncbi.nlm.nih.gov/pubmed/%s")
                       (pmcid . "https://www.ncbi.nlm.nih.gov/pmc/articles/%s")
                       (url . "%s")
                       (howpublished . "%s")))
  (citar-note-format-function #'rk-citar--format-note)
  :init
  (rk-leader-def
    "o c"   '(:ignore t :wk "cite")
    "o c o" '(citar-open :wk "open")

    "G b" '(rk-citar--goto-bib :wk "goto bib refs"))
  :config
  (require 'citar-org))

(use-package citar-embark
  :straight t
  :after citar embark
  :config (citar-embark-mode))

(use-package org-pdftools
  :straight t
  :after org
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-drill
  :straight t
  :after org)

(use-package org-roam-review
  :commands
  (org-roam-review
   org-roam-review-list-by-maturity
   org-roam-review-list-recently-added
   org-roam-review-set-seedling
   org-roam-review-set-excluded)
  :custom
  (org-roam-review-cache-file rk-org--roam-review-cache-file)
  (org-roam-review-ignored-tags '("daily"))
  :general
  ;; optional bindings for evil-mode compatability.
  (:states '(normal) :keymaps 'org-roam-review-mode-map
           "TAB" 'magit-section-cycle
           "g r" 'org-roam-review-refresh)
  :preface
  (autoload 'vulpea-buffer-tags-get "vulpea-buffer")
  (defun rk-orr--review-note-p ()
    (when-let* ((tags (vulpea-buffer-tags-get))
                (valid-p (not (-intersection tags org-roam-review-ignored-tags)))
                (review-p (y-or-n-p "Review this note?")))
      (org-roam-review-set-seedling)))
  :init
  (rk-leader-def
    "o r" '(:ignore t :wk "review")
    "o r s" '(org-roam-review :wk "status")
    "o r v" '(org-roam-review-list-due :wk "to review")
    "o r u" '(org-roam-review-list-by-maturity :wk "to categorize")
    "o r r" '(org-roam-review-list-recently-added :wk "recently added"))
  :config
  (add-hook 'org-roam-capture-new-node-hook #'rk-orr--review-note-p)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*org-roam-review*" eos)
                 (display-buffer-in-direction)
                 (direction       . right)
                 (window-width    . 0.5)
                 (window-height   . fit-window-to-buffer)))
  (rk-local-leader-def :keymaps 'org-mode-map
    "r"   '(:ignore t :wk "review")
    "r r" '(org-roam-review-accept :wk "accept")
    "r u" '(org-roam-review-bury :wk "bury")
    "r x" '(org-roam-review-set-excluded :wk "set excluded")
    "r b" '(org-roam-review-set-budding :wk "set budding")
    "r s" '(org-roam-review-set-seedling :wk "set seedling")
    "r e" '(org-roam-review-set-evergreen :wk "set evergreen")))

(straight-use-package
 '(nursery :type git :host github :repo "chrisbarrett/nursery"))

(use-package org-roam-gc
  :init
  (rk-leader-def
    "oug" '(org-roam-gc :wk "gc")))

(use-package org-roam-search
  :init
  (rk-leader-def
    "o/" '(org-roam-search :wk "search")))

(use-package org-format
  :after org
  :custom
  (org-format-blank-lines-before-content 0)
  :hook (org-mode . org-format-on-save-mode))

(use-package org-roam-links
  :init
  (rk-leader-def
    "ob" '(org-roam-links :wk "evergreen links")))

(use-package org-modern
  :straight (:type git :host github :repo "minad/org-modern" :branch "main")
  :after org
  :custom
  (org-modern-list '((?+ . "◦")
                     (?* . "–")
                     (?- . "•")))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (set-face-attribute 'org-modern-label nil :family "Iosevka")
  (global-org-modern-mode))

(provide 'rk-org)

;;; rk-org.el ends here
