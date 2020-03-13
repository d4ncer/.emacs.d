;;; rk-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil-transient-state)
(require 'definers)
(require 'general)
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
  :init
  (setq transient-values-file (f-join paths-cache-directory "transient/values.el"))
  (setq transient-history-file (f-join paths-cache-directory "transient/history.el"))
  (setq transient-levels-file (f-join paths-cache-directory "transient/levels.el")))

(use-package magit
  :straight t
  :defer t
  :commands (magit-status magit-blame-addition magit-branch-and-checkout)
  :functions (magit-display-buffer-fullframe-status-v1)
  :general
  (:keymaps 'magit-refs-mode-map :states '(normal)
            "." #'magit-branch-and-checkout)
  :preface
  (pretty-hydra-define rk-git--git-blame
    (:title "Git Blame" :pre (unless (bound-and-true-p magit-blame-mode) (call-interactively 'magit-blame-addition)) :post (when (bound-and-true-p magit-blame-mode) (magit-blame-quit)) :foreign-keys run)
    ("Blame"
     (("j" magit-blame-addition "More")
      ("k" magit-blame-removal "Less")
      ("q" magit-blame-quit "quit" :exit t))))
  :init
  (rk-leader-def
    "gs" #'magit-status
    "gb" #'rk-git--git-blame/body)
  :config
  (general-setq magit-log-section-commit-count 0
                magit-section-visibility-indicator nil
                magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-commit-jira-prefix
  :hook (git-commit-setup . git-commit-jira-prefix-insert))

(use-package git-timemachine
  :straight (:host gitlab :repo "pidu/git-timemachine" :branch "master")
  :defer t
  :commands
  (git-timemachine
   git-timemachine-show-current-revision
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
    "gt" #'rk-git--timemachine/body))

(use-package diff-hl
  :straight t
  :after magit
  :preface
  (progn
    (defun rk-magit--diff-hl-mode-on ()
      (diff-hl-mode -1))

    (defun rk-magit--diff-hl-mode-off ()
      (diff-hl-mode +1)))
  :init
  (progn
    (evil-transient-state-define git-hunks
      :title "Git Hunk Transient State"
      :doc "
[_p_/_N_] previous [_n_] next [_g_] goto [_x_] revert [_q_] quit"
      :foreign-keys run
      :bindings
      ("n" diff-hl-next-hunk)
      ("N" diff-hl-previous-hunk)
      ("p" diff-hl-previous-hunk)
      ("g" diff-hl-diff-goto-hunk)
      ("x" diff-hl-revert-hunk)
      ("q" nil :exit t))

    (rk-leader-def
      "g." 'git-hunks-transient-state/body))
  :config
  (progn
    (add-hook 'iedit-mode-hook #'rk-magit--diff-hl-mode-on)
    (add-hook 'iedit-mode-end-hook #'rk-magit--diff-hl-mode-off)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode)))

(use-package forge
  :after magit
  :straight (:host github :repo "magit/forge"))

(use-package evil-magit
  :straight t
  :demand t
  :after magit
  :custom
  (forge-database-file (f-join paths-cache-directory "forge-database.sqlite"))
  :general
  (:keymaps 'magit-mode-map
            :states '(normal visual)
            "C-u" #'evil-scroll-page-up
            "C-d" #'evil-scroll-page-down))


(provide 'rk-magit)

;;; rk-magit.el ends here
