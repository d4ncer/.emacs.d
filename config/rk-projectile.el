;;; rk-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'paths)

(use-package projectile
  :straight t
  :preface
  (autoload 'magit-status "magit")

  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'default)
  (projectile-switch-project-action 'magit-status)
  (projectile-enable-caching t)
  (projectile-cache-file (concat paths-cache-directory "/projectile.cache"))
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

  :init
  (rk-leader-def
    "p!" '(projectile-run-shell-command-in-root :wk "shell cmd as root")
    "p&" '(projectile-run-async-shell-command-in-root :wk "async shell cmd as root")
    "pI" '(projectile-invalidate-cache :wk "invalidate cache")
    "pc" '(projectile-compile-project :wk "compile project")
    "pC" '(projectile-cleanup-known-projects :wk "cleanup known projects")
    "pr" '(projectile-replace :wk "replace (project)")
    "pt" '(projectile-test-project :wk "test (project)")
    "pu" '(projectile-run-project :wk "run (project)")
    "pp" '(projectile-switch-project :wk "switch project")
    "pf" '(projectile-find-file :wk "find file (project)")
    "pF" '(projectile-recentf :wk "find recent file (project)")
    "pd" '(projectile-find-dir :wk "find dir (project)")
    "pb" '(projectile-switch-to-buffer :wk "switch buffer (project)"))

  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :disabled
  :custom
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-switch-project-action 'rk-counsel-projectile--switch-project-action)
  :preface
  (defun rk-counsel-projectile--switch-project-action (project)
    (if (f-exists-p (f-join project ".git"))
        (magit-status project)
      (dired project)))
  (defun rk-counsel-projectile--ad-read-args (fn &optional options)
    (funcall fn (if current-prefix-arg
                    (read-string "rg args: " options)
                  options)))
  :init
  (rk-leader-def
    "pp" '(counsel-projectile-switch-project :wk "switch project")
    "pf" '(counsel-projectile-find-file :wk "find file (project)")
    "pd" '(counsel-projectile-find-dir :wk "find dir (project)")
    "pb" '(counsel-projectile-switch-to-buffer :wk "switch buffer (project)")
    "/"  '(counsel-projectile-rg :wk "search (project)"))


  :config
  (advice-add #'counsel-projectile-rg :around #'rk-counsel-projectile--ad-read-args)
  (counsel-projectile-mode +1))

(provide 'rk-projectile)

;;; rk-projectile.el ends here
