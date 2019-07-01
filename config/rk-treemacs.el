;;; rk-treemacs.el --- Configure Treemacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'paths)
(autoload 'evil-set-initial-state "evil-core")

(use-package treemacs
  :straight t
  :commands (treemacs
             treemacs-add-project-to-workspace
             treemacs-toggle
             treemacs-mode
             treemacs-next-line
             treemacs-previous-line)
  :init
  (rk-leader-def
    "f t" '(treemacs :wk "tree")
    "p t" '(treemacs-add-project-to-workspace :wk "add to tree"))

  :config
  (setq treemacs-persist-file (concat paths-cache-directory "/treemacs-persist"))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :straight t
  :config (treemacs-icons-dired-mode))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
