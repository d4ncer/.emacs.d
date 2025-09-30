;;; mod-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains org-mode and related packages configuration including:
;; - org (the main org-mode package)
;; - org-node (knowledge management with org-id)
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
                     "TAB" #'org-cycle
                     "S-TAB" #'org-shifttab)
  :custom
  (org-directory org-directory)
  (org-default-notes-file org-default-notes-file)
  (org-startup-folded 'content)
  (org-startup-indented t)
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
  :config
  (add-to-list 'org-modules 'org-habit t))


(use-package org-node
  :ensure (:host github :repo "meedstrom/org-node")
  :after org
  :hook (org-mode-hook . org-node-cache-mode)
  :general (:keymaps 'org-mode-map :states '(normal visual motion)
                     "C-c C-l" #'org-node-insert-link
                     "C-c C-o" #'org-node-open)
  :custom
  (org-node-creation-fn #'org-node-new-file)
  (org-node-file-directory-ask +org-brain-dir)
  (org-node-slug-fn #'org-node-slugify-for-web)
  (org-node-datestamp-format "%Y%m%d%H%M%S")
  (org-node-extra-id-dirs (list +org-brain-dir))
  :config
  (org-node-cache-ensure))

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-list '((?+ . "◦")
                     (?* . "–")
                     (?- . "•")))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(provide 'mod-org)
;;; mod-org.el ends here
