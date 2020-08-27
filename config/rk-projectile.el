;;; rk-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)
(require 'paths)

(use-package projectile
  :straight t
  :commands (projectile-compile-project
             projectile-invalidate-cache
             projectile-mode
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-project
             projectile-run-shell-command-in-root
             projectile-cleanup-known-projects
             projectile-switch-project
             projectile-test-project)

  :preface
  (autoload 'magit-status "magit")

  :init
  (progn
    (rk-leader-def
      "p!" '(projectile-run-shell-command-in-root :wk "shell cmd as root")
      "p&" '(projectile-run-async-shell-command-in-root :wk "async shell cmd as root")
      "pI" '(projectile-invalidate-cache :wk "invalidate cache")
      "pc" '(projectile-compile-project :wk "compile project")
      "pC" '(projectile-cleanup-known-projects :wk "cleanup known projects")
      "pr" '(projectile-replace :wk "replace (project)")
      "pt" '(projectile-test-project :wk "test (project)")
      "pu" '(projectile-run-project :wk "run (project)")))

  :config
  (progn
    (setq projectile-indexing-method 'alien)
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action 'magit-status)
    (setq projectile-cache-file (concat paths-cache-directory "/projectile.cache"))
    (setq projectile-enable-caching t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("meta" "jsbundle" "gz" "zip" "tar" "elc"))
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
            "flow-typed/npm"
            "vendor"
            "straight/repos"
            "target"))

    (projectile-mode)))


(use-package counsel-projectile
  :straight t
  :defer t
  :commands (counsel-projectile-mode
             counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-project
             counsel-projectile-switch-to-buffer
             counsel-projectile-rg)

  :custom
  (counsel-projectile-switch-project-action 'magit-status)

  :preface
  (defun rk-counsel-projectile--ad-read-args (fn &optional options)
    (funcall fn (if current-prefix-arg
                    (read-string "rg args: " options)
                  options)))
  :init
  (rk-leader-def
    "pf" '(counsel-projectile-find-file :wk "find file (project)")
    "pd" '(counsel-projectile-find-dir :wk "find dir (project)")
    "pb" '(counsel-projectile-switch-to-buffer :wk "switch buffer (project)")
    "/"  '(counsel-projectile-rg :wk "search (project)"))


  :config
  (progn
    (advice-add #'counsel-projectile-rg :around #'rk-counsel-projectile--ad-read-args)
    (counsel-projectile-mode +1)))

(provide 'rk-projectile)

;;; rk-projectile.el ends here
