;;; mod-project.el --- Project management (projectile) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains project management configuration including:
;; - Projectile for project-based navigation and commands
;; - Custom project switching action (magit or dired)

;;; Code:

;;; Projectile - Project management

(use-package projectile :ensure t
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'default)
  (projectile-switch-project-action #'+switch-project-action)
  (projectile-enable-caching t)
  (projectile-globally-ignored-files '("TAGS" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '("meta" "jsbundle" "gz" "zip" "tar" "elc"))
  (projectile-globally-ignored-directoriess '(".bzr"
                                              ".ensime_cache"
                                              ".eunit"
                                              ".fslckout"
                                              ".g8"
                                              ".git"
                                              ".hg"
                                              ".idea"
                                              ".stack-work"
                                              ".svn"
                                              "build"
                                              "dist"
                                              "node_modules"
                                              "vendor"
                                              "straight/repos"
                                              "target"))
  :config
  (autoload 'magit-status "magit")
  (defun +switch-project-action ()
    (let ((project-root (projectile-acquire-root)))
      (if (file-exists-p (expand-file-name ".git" project-root))
          (magit-status project-root)
        (dired project-root))))
  (projectile-mode +1))

;;; Beads - Issue tracking system interface

(use-package beads
  :ensure (:host codeberg :repo "ctietze/beads.el")
  :defer t
  :init
  (autoload 'beads-create-issue "beads-transient" nil t)
  :custom
  (beads-auto-refresh t)
  (beads-auto-refresh-interval 30)
  (beads-per-project-buffer t))

(provide 'mod-project)
;;; mod-project.el ends here
