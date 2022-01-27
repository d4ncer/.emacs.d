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
(defconst rk-org-gtd-dir (f-join paths--org-dir "gtd"))
(defconst rk-org-roam-dir (f-join paths--org-dir "roam"))
(defconst rk-org-roam-dailies-dir (f-join rk-org-roam-dir "daily"))
(defconst rk-org-roam-temporal-prefix "%<%Y%m%d%H%M%S>")
(defvar rk-bib-refs-file (f-join paths--dropbox-dir "org/bib/references.bib"))

(use-package org
  :straight t
  :demand t
  :general
  (:keymaps 'org-mode-map
            "C-c l" #'rkca-insert-TKISS-link
            "C-n" #'org-next-visible-heading
            "C-p" #'org-previous-visible-heading
            "M-p"     #'org-metaup
            "M-n"     #'org-metadown)
  (:keymaps 'org-mode-map :states '(normal visual motion)
            "gb" #'org-mark-ring-goto)
  (:keymaps 'org-mode-map :states '(visual)
            "-" #'rk-org--deemphasize
            "B" #'rk-org--embolden
            "_" #'rk-org--underline
            "/" #'rk-org--italicize
            "+" #'rk-org--strike-through
            "=" #'rk-org--quote)
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

  (defun rk-org--exit-minibuffer (&rest _)
    "Exit minibuffer before adding notes."
    (when (minibufferp (window-buffer (selected-window)))
      (other-window 1)))

  (defun rk-org--toggle-heading-goto-eol (&rest _)
    "Prevent point from moving to BOL when toggling headings."
    (when (s-matches? (rx bol (+ "*") (* space) eol)
                      (buffer-substring (line-beginning-position) (line-end-position)))
      (goto-char (line-end-position))))

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

  :custom
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
  (org-startup-indented t)
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
  (load-file (expand-file-name "org-version.el" (concat paths-lisp-directory "/rk-org")))
  (rk-leader-def
    "ns" '(org-narrow-to-subtree :wk "narrow to subtree")
    "ol" '(org-store-link :wk "store link")
    "ous" '(org-save-all-org-buffers :wk "save all"))

  :hook ((org-mode . rk-org--setup-org))
  :config
  (add-hook 'org-mode-hook #'rk-org--disable-flycheck)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'rk-org--disable-ligatures)
  (rk-local-leader-def :keymaps 'org-mode-map
    "t"  '(:ignore t :wk "time")
    "td" '(org-deadline :wk "deadline")
    "ts" '(org-schedule :wk "schedule")

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
                              (ledger . t)
                              (python . t)
                              (http . t)
                              (calc . t)
                              (dot . t)
                              (shell . t)))
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
  (org-attach-id-dir (f-join paths--org-dir "data")))

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

  :custom
  (org-stuck-projects '("+project-ignore-maybe-done"
                        ("NEXT") nil
                        "SCHEDULED:"))

  (org-agenda-include-diary nil)
  (org-agenda-start-on-weekday nil)
  (org-agenda-auto-exclude-function #'rk-org--exclude-tasks-on-hold)
  (org-agenda-files (f-files paths--gtd-dir (lambda (f) (f-ext? f "org"))))
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
    "p" '(org-agenda-set-property :wk "set property")
    "r" '(org-agenda-refile :wk "refile"))

  ;; Match projects that do not have a scheduled action or NEXT action.

  (add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt))

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

  (defun rk-org--disable-eldoc-check ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  :config
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
  (add-hook 'org-src-mode-hook #'rk-org--disable-eldoc-check)
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
  (org-clock-persist-file (f-join rk-org-roam-dir ".org-clock-save"))

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
  (rk-local-leader-def :keyamps 'org-mode-map
    "uc" '(org-clock-transient :wk "clock"))
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-out-hook #'rk-org--remove-empty-clock-drawers t))

(use-package org-download
  :straight t
  :after org
  :config
  (setq org-download-method 'attach))

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
    "o g v"  '(rk-org-goto-tags-list :wk "tags")))

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

(use-package org-indent
  :after org
  :hook (org-mode . org-indent-mode))

(use-package flyspell
  :after org
  :hook (org-mode . flyspell-mode))

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
  (defvar rk-org-roam--dailies-prev-buffer nil)
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
  (org-roam-directory rk-org-roam-dir)
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?" :target
                                         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n"))))
  :init
  (rk-leader-def
    "ob" '(org-roam-buffer-toggle :wk "toggle buffer")
    "oB" '(org-roam-buffer-display-dedicated :wk "toggle dedicated buffer")

    "od"  '(:ignore t :wk "date")
    "odt" '(org-roam-dailies-goto-today :wk "today")
    "odd" '(org-roam-dailies-goto-date :wk "for date")
    "ody" '(org-roam-dailies-goto-yesterday :wk "yesterday")
    "odT" '(org-roam-dailies-goto-tomorrow :wk "tomorrow"))
  (rk-local-leader-def :keymaps 'org-mode-map
    "d" '(rk-org-roam--daily-utils/body :wk "daily"))
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
  (org-roam-db-sync 'force)
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
  :preface
  (defun rk-org--filter-non-diary-notes (note)
    (and (not (f-child-of-p (vulpea-note-path note) rk-org-roam-dailies-dir))
         (not (f-child-of-p (vulpea-note-path note) rk-roam-refs-dir))))
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find
     (lambda (type)
       (eq type 'todo))
     (org-element-map
         (org-element-parse-buffer 'headline)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
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
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                        ("__*" . "_")                   ;; remove sequential underscores
                        ("^_" . "")                     ;; remove starting underscore
                        ("_$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs))
               (ts (format-time-string "%Y%m%d%H%M%S")))
          (expand-file-name (format "%s-%s.org" ts (downcase slug)) org-roam-directory)))))

  (defun rk-vulpea--person-to-tag (title)
    (concat "@" (s-replace " " "" title)))

  (defun rk-vulpea--create (title &optional insert-p jump-to)
    (let* ((type (completing-read "Note type: "
                                  '(("default" 1) ("person" 2))
                                  nil t))
           (is-ca (y-or-n-p "Is this note CA related?"))
           (note (cond
                  ((string= type "person")
                   (vulpea-create
                    title
                    (rk-vulpea--org-roam-file-name title)
                    :properties '(("CATEGORY" . "person"))
                    :tags (remq nil (list "person" is-ca (rk-vulpea--person-to-tag title)))
                    :immediate-finish t))
                  (t (vulpea-create
                      title
                      (rk-vulpea--org-roam-file-name title)
                      :immediate-finish t
                      :tags (remq nil (list is-ca)))))))
      (when insert-p
        (insert (org-link-make-string (concat "id:" (vulpea-note-id note)) title)))
      (when jump-to
        (find-file (vulpea-note-path note)))))

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

  (defun rk-vulpea--find (&key filter-fn)
    (interactive)
    (let* ((region-text
            (when (region-active-p)
              (org-link-display-format
               (buffer-substring-no-properties
                (set-marker
                 (make-marker) (region-beginning))
                (set-marker
                 (make-marker) (region-end))))))
           (note (vulpea-select-from
                  "Note"
                  (funcall
                   vulpea-find-default-candidates-source
                   (or
                    filter-fn
                    vulpea-find-default-filter))
                  :initial-prompt region-text))
           (title (or region-text
                      (vulpea-note-title note))))
      (if (vulpea-note-id note)
          (org-roam-node-visit
           (org-roam-node-from-id (vulpea-note-id note)))
        (rk-vulpea--create title nil t))))

  (defun rk-org--non-diary-notes ()
    (interactive)
    (rk-vulpea--find :filter-fn #'rk-org--filter-non-diary-notes))


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
  :general
  (:keymaps 'org-mode-map
            "C-c i" #'rk-vulpea--insert)
  :init
  (rk-leader-def
    "of" '(rk-org--non-diary-notes :wk "find file node"))
  :config
  (add-hook 'vulpea-insert-handle-functions #'rk-vulpea--insert-handle)
  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update))

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
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))
  :config
  (setq org-agenda-custom-commands
        '(("g" "General"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next action")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")))
            (agenda ""))
           ((org-agenda-files (vulpea-project-files))
            (org-agenda-prefix-format '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
                                        (todo . " %i %(vulpea-agenda-category 12) ")
                                        (tags . " %i %(vulpea-agenda-category 12) ")
                                        (search . " %i %(vulpea-agenda-category 12) "))))))))

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
  (ebib-preload-bib-files `(,rk-bib-refs-file)))

(use-package citar
  :straight t
  :general
  (:keymaps 'org-mode-map :states '(insert)
            "C-c b" #'citar-insert-citation)
  (:keymaps 'minibuffer-local-map
            "C-b" #'citar-insert-preset)
  :preface
  (autoload #'org-roam-review-set-budding "org-roam-review")
  (autoload #'vulpea-buffer-title-set "vulpea-buffer")

  (defvar rk-roam-refs-dir (f-join rk-org-roam-dir "references/"))
  (defvar rk-bib-lib-dir (f-join paths--dropbox-dir "bib_files"))
  (defun rk-citar--idle-refresh-cache ()
    "Generate bib item caches with idle timer."
    (run-with-idle-timer 0.5 nil #'citar-refresh))
  (defun rk-citar--goto-bib ()
    "Open the bib file."
    (interactive)
    (find-file rk-bib-refs-file))
  (defun rk-citar--format-note (key entry filepath)
    (let* ((template "${title}; ${author editor}")
           (note-meta
            (citar--format-entry-no-widths entry template))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
        (erase-buffer)
        (citar-org-roam-make-preamble key)
        (vulpea-buffer-title-set note-meta)
        (call-interactively #'org-roam-review-set-budding)
        (goto-char (point-max))
        (insert "\n")
        (evil-insert 1))))
  :custom
  (citar-notes-paths `(,rk-roam-refs-dir))
  (citar-library-paths `(,rk-bib-lib-dir))
  (org-cite-global-bibliography `(,rk-bib-refs-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography `(,rk-bib-refs-file))
  (citar-format-note-function #'rk-citar--format-note)
  :init
  (rk-leader-def
    "o c"   '(:ignore t :wk "cite")
    "o c o" '(citar-open :wk "open")

    "G b" '(rk-citar--goto-bib :wk "goto bib refs"))
  :config
  (add-hook 'org-mode-hook #'rk-citar--idle-refresh-cache)
  (add-hook 'LaTeX-mode-hook #'rk-citar--idle-refresh-cache)
  (citar-refresh)
  (require 'citar-org))

(use-package org-pdftools
  :straight t
  :after org
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-drill
  :straight t
  :after org)

(use-package org-roam-review
  :hook
  (org-mode . org-roam-review-cache-mode)
  (org-roam-capture-new-node . org-roam-review-set-seedling)
  :commands
  (org-roam-review
   org-roam-review-list-uncategorised
   org-roam-review-list-recently-added)
  :custom
  (org-roam-review-cache-file (f-join paths--org-dir ".org-roam-review"))
  (org-roam-review-ignored-tags '())
  :general
  ;; optional bindings for evil-mode compatability.
  (:states '(normal) :keymaps 'org-roam-review-mode-map
           "TAB" 'magit-section-cycle
           "g r" 'org-roam-review-refresh)
  :init
  (rk-leader-def
    "o r" '(:ignore t :wk "review")
    "o r s" '(org-roam-review :wk "status")
    "o r u" '(org-roam-review-list-uncategorised :wk "to categorize")
    "o r r" '(org-roam-review-list-recently-added :wk "recently added"))
  :config
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

(use-package org-roam-gc
  :init
  (rk-leader-def
    "oug" '(org-roam-gc :wk "gc")))

(use-package org-roam-search
  :init
  (rk-leader-def
    "o/" '(org-roam-search :wk "search")))

(provide 'rk-org)

;;; rk-org.el ends here
