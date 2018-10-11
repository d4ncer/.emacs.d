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
  :general
  (:keymaps 'treemacs-mode-map :states 'emacs
            "j" #'treemacs-next-line
            "k" #'treemacs-previous-line)
  :config
  (progn
    (evil-set-initial-state 'treemacs-mode 'emacs))
  :init
  (progn
    (setq treemacs-persist-file (concat paths-cache-directory "/treemacs-persist"))
    (rk-leader-def
      "f t" '(treemacs :wk "tree")
      "p t" '(treemacs-add-project-to-workspace :wk "add to tree"))))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
