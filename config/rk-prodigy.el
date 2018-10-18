;;; rk-prodigy.el --- Config for Prodigy  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package prodigy
  :straight t
  :demand t
  :commands (prodigy)
  :general
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
  :init
  (rk-leader-def
    "x p" '(prodigy :wk "prodigy"))
  :config
  (progn
    (setq prodigy-view-truncate-by-default t)
    (setq prodigy-completion-system 'default)))

(provide 'rk-prodigy)

;;; rk-prodigy.el ends here
