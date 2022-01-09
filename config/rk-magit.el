;;; rk-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'pretty-hydra)

(use-package with-editor
  :straight t
  :commands (with-editor-finish
             with-editor-cancel)
  :general
  (:keymaps 'with-editor-mode-map
            :states '(normal motion visual emacs)
            ", c" #'with-editor-finish
            ", k" #'with-editor-cancel))

(use-package transient
  :straight t
  :custom
  (transient-values-file (f-join paths-cache-directory "transient/values.el"))
  (transient-history-file (f-join paths-cache-directory "transient/history.el"))
  (transient-levels-file (f-join paths-cache-directory "transient/levels.el")))

(use-package magit
  :straight t
  :functions (magit-display-buffer-fullframe-status-v1)
  :custom
  (magit-log-section-commit-count 0)
  (magit-section-visibility-indicator nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :general
  (:keymaps 'magit-refs-mode-map :states '(normal)
            "." #'magit-branch-and-checkout)
  :preface
  (pretty-hydra-define rk-git--git-blame
    (:title "Git Blame" :pre (unless (bound-and-true-p magit-blame-mode) (call-interactively 'magit-blame-addition)) :post (when (bound-and-true-p magit-blame-mode) (magit-blame-quit)) :foreign-keys run)
    ("Blame"
     (("C-n" magit-blame-addition "More")
      ("C-p" magit-blame-removal "Less")
      ("q" magit-blame-quit "quit" :exit t))))
  :init
  (rk-leader-def
    "gs" '(magit-status :wk "git status")
    "gb" '(rk-git--git-blame/body :wk "git blame")))

(use-package git-commit-clubhouse-prefix
  :disabled t
  :config (git-commit-clubhouse-prefix-init))

(use-package git-commit-jira-prefix
  :commands git-commit-jira-prefix-init
  :config (git-commit-jira-prefix-init))

(use-package git-timemachine
  :straight (:host gitlab :repo "pidu/git-timemachine" :branch "master")
  :commands
  (git-timemachine-show-current-revision
   git-timemachine-show-nth-revision
   git-timemachine-show-previous-revision
   git-timemachine-show-next-revision
   git-timemachine-show-previous-revision
   git-timemachine-kill-revision
   git-timemachine-quit)
  :preface
  (pretty-hydra-define rk-git--timemachine
    (:title "Hot Git Time Machine" :pre (unless (bound-and-true-p git-timemachine-mode) (call-interactively 'git-timemachine)) :post (when (bound-and-true-p git-timemachine-mode) (git-timemachine-quit)) :foreign-keys run :quit-key "q")
    ("Goto"
     (("p" git-timemachine-show-previous-revision "previous commit")
      ("n" git-timemachine-show-next-revision "next commit")
      ("c" git-timemachine-show-current-revision "current commit")
      ("g" git-timemachine-show-nth-revision "nth commit"))

     "Misc"
     (("Y" git-timemachine-kill-revision "copy hash"))))
  :init
  (rk-leader-def
    "gt" '(rk-git--timemachine/body :wk "timemachine")))

(use-package diff-hl
  :straight t
  :preface
  (pretty-hydra-define rk-git--git-hunks
    (:title "Git Hunks" :foreign-keys run :quit-key "q")
    ("Navigation"
     (("n" diff-hl-next-hunk "Next")
      ("N" diff-hl-previous-hunk "Previous")
      ("p" diff-hl-previous-hunk "Previous"))
     "Actions"
     (("g" diff-hl-diff-goto-hunk "Goto")
      ("x" diff-hl-revert-hunk "Revert"))))
  :init
  (rk-leader-def
    "g."'(rk-git--git-hunks/body :wk "hunks"))
  :config
  (global-diff-hl-mode))

(use-package diff-hl
  :straight t
  :after iedit
  :preface
  (defun rk-magit--diff-hl-mode-on ()
    (diff-hl-mode -1))

  (defun rk-magit--diff-hl-mode-off ()
    (diff-hl-mode +1))
  :config
  (add-hook 'iedit-mode-hook #'rk-magit--diff-hl-mode-on)
  (add-hook 'iedit-mode-end-hook #'rk-magit--diff-hl-mode-off))

(use-package diff-hl
  :straight t
  :after magit
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package forge
  :straight (:host github :repo "magit/forge")
  :after magit
  :custom
  (forge-add-default-sections nil)
  (forge-add-default-bindings nil))

(use-package orgit
  :straight t
  :after (org magit))

(provide 'rk-magit)

;;; rk-magit.el ends here
