;;; mod-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains org-mode and related packages configuration including:
;; - org (the main org-mode package)
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
                     "RET" #'org-open-at-point
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
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELLED")
                       (sequence "SOMEDAY" "|" "DONE" "CANCELLED")))
  (org-agenda-files (list (file-name-concat org-directory "roam")))
  (org-agenda-custom-commands
   '(("p" "Tasks"
      ((todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
       (todo "TODO" ((org-agenda-overriding-header "To Do")))))
     ("d" "Today"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-start-day nil)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))))))
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq org-capture-templates
        `(("i" "Inbox" entry (file ,org-default-notes-file)
           "* TODO %?\n%U\n" :prepend t))))

(use-package vulpea
  :ensure t
  :after org
  :commands (vulpea-find vulpea-insert vulpea-buffer-tags-get
                         vulpea-buffer-tags-add vulpea-buffer-tags-remove)
  :custom
  (vulpea-default-notes-directory (file-name-concat org-directory "roam"))
  :config
  (vulpea-db-autosync-mode 1))

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
  :after vulpea)

(with-eval-after-load 'vulpea
  (require '+life))

(use-package org-modern
  :ensure t
  :hook (org-mode-hook . org-modern-mode)
  :custom
  (org-modern-list '((?+ . "◦")
                     (?* . "–")
                     (?- . "•")))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(with-eval-after-load 'org
  (require '+org-format)
  (add-hook 'org-mode-hook #'+org-format-on-save-mode))

(provide 'mod-org)
;;; mod-org.el ends here
