;;; rk-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kastur

;; Author: Raghuvir Kastur <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'rk-emacs)
(require 'spacemacs-keys)

(use-package projectile
  :commands (projectile-compile-project
             projectile-invalidate-cache
             projectile-mode
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-project
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-test-project)

  :preface
  (autoload 'magit-status "magit")

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "p!" #'projectile-run-shell-command-in-root
      "p&" #'projectile-run-async-shell-command-in-root
      "pI" #'projectile-invalidate-cache
      "pc" #'projectile-compile-project
      "pr" #'projectile-replace
      "pt" #'projectile-test-project
      "pu" #'projectile-run-project))

  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action 'magit-status)
    (setq projectile-cache-file (concat rk-emacs-cache-directory "/projectile.cache"))
    (setq projectile-enable-caching t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))
    (setq projectile-globally-ignored-directories
          '(".bzr"
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
            "target"))

    (projectile-mode)))


(use-package counsel-projectile
  :defer t
  :commands (counsel-projectile-on
             counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-project
             counsel-projectile-switch-to-buffer
             counsel-projectile-ag)
  :init
  (spacemacs-keys-set-leader-keys
    "pf" #'counsel-projectile-find-file
    "pd" #'counsel-projectile-find-dir
    "pb" #'counsel-projectile-switch-to-buffer
    "pp" #'counsel-projectile-switch-project
    "/"  #'counsel-projectile-ag)

  :config
  (counsel-projectile-on))

(provide 'rk-projectile)

;;; rk-projectile.el ends here
