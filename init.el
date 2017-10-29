;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

;;; Code:

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Initialize package.el
;;
;; Most packages are installed using git subtrees, but some packages (such as
;; flycheck) break unless installed via package.el.

(require 'package)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Bootstrap use-package.

(require 'seq)
(require 'subr-x)

(defun rk-init/init-load-path (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.
If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory))
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files lisp-dir t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files config-dir t "^[^.]"))))
    (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path)
      (add-to-list 'load-path (concat path "/emacs"))
      (add-to-list 'load-path (concat path "/elisp"))
      (add-to-list 'load-path (concat path "/lisp")))

    (add-to-list 'load-path (concat lisp-dir "/org-mode/contrib/lisp"))
    (add-to-list 'load-path (concat lisp-dir "/gocode/emacs-company"))
    (add-to-list 'load-path (concat lisp-dir "/hindent/elisp"))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let (added (seq-difference load-path before))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(rk-init/init-load-path)
(defconst use-package-verbose t)
(require 'use-package)


;; Load features.

(use-package rk-emacs)
(use-package rk-basic-settings)
(use-package rk-faces)
(use-package rk-modeline)
(use-package rk-auto-save)
(use-package rk-leader-keys)
(use-package rk-evil)
(use-package rk-ivy)
(use-package rk-darwin :if (equal system-type 'darwin))
(use-package rk-org)
(use-package rk-rg)
(use-package rk-help)
(use-package rk-projectile)
(use-package rk-dired)
(use-package rk-hl-todo)
(use-package rk-magit)
(use-package rk-smartparens)
(use-package rk-company)
(use-package rk-undo-tree)
(use-package rk-ws-butler)
(use-package rk-parentheses)
(use-package rk-aggressive-indent)
(use-package rk-flycheck)
(use-package rk-ibuffer)
(use-package rk-neotree)
(use-package rk-php)
(use-package rk-coffeescript)
(use-package rk-haskell)
(use-package rk-elisp)
(use-package rk-web-mode)
(use-package rk-go)
(use-package rk-scala)
(use-package rk-markdown)
(use-package rk-yaml)
(use-package rk-rnc)
(use-package rk-highlight-thing)
(use-package rk-spelling)
(use-package rk-string)
(use-package rk-docker)
(use-package rk-groovy)
(use-package rk-lobsters)
(use-package rk-expand-region)
(use-package rk-embrace)

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


(provide 'init)

;;; init.el ends here
