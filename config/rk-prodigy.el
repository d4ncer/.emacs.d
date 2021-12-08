;;; rk-prodigy.el --- Config for Prodigy  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package prodigy
  :straight t
  :general
  (:keymaps 'prodigy-view-mode-map :states '(normal motion)
            "C-l" #'prodigy-view-clear-buffer)
  (:keymaps 'prodigy-mode-map :states '(normal motion)
            "C-g" #'kill-buffer-and-window
            "$" #'prodigy-display-process
            "g r" #'prodigy-refresh
            "F" #'prodigy-clear-filters
            "M" #'prodigy-mark-all
            "S" #'prodigy-stop
            "U" #'prodigy-unmark-all
            "f" '(:ignore t :wk "filter")
            "f n" #'prodigy-add-name-filter
            "f t" #'prodigy-add-tag-filter
            "J" '(:ignore t :wk "jump")
            "J d" #'prodigy-jump-file-manager
            "J m" #'prodigy-jump-magit
            "s" #'prodigy-start
            "m" #'prodigy-mark)
  :custom
  (prodigy-view-truncate-by-default t)
  (prodigy-completion-system 'default)
  :init
  (rk-leader-def
    "x p" '(prodigy :wk "prodigy")))

(provide 'rk-prodigy)

;;; rk-prodigy.el ends here
