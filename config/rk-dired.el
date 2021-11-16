;;; rk-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)

(use-package dired
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "$" #'end-of-line
           "i" nil
           "i i" 'dired-insert-subdir
           "i q" 'dired-kill-subdir
           "TAB" 'dired-hide-subdir)
  :preface
  (defun rk-dired--sort-directories-first (&rest _)
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :custom
  (dired-listing-switches "-alhv")
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-omit-files (rx bol (or (+ ".") (and "__pycache__" eol))))
  :config
  (advice-add 'dired-readin :after #'rk-dired--sort-directories-first)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (rk-leader-def
    "d" '(dired :wk "dired"))
  (rk-local-leader-def :keymaps 'dired-mode-map
    "?" '(dired-hide-details-mode :wk "toggle details")
    "." '(dired-omit-mode :wk "toggle hidden")
    "e" '(wdired-change-to-wdired-mode :wk "wdired")
    "s" '(dired-sort-toggle-or-edit :wk "toggle sort")

    "f" 'dired
    "F" '(dired-other-window :wk "dired (other window)")

    "m" '(:ignore t :wk "mark")
    "m a" '(dired-mark-unmarked-files :wk "unmarked")
    "m c" '(dired-change-marks :wk "change")
    "m r" '(dired-mark-files-regexp :wk "by regexp")
    "m l" '(dired-mark-symlinks :wk "symlinks")
    "m d" '(dired-mark-directories :wk "directories")
    "U" '(dired-unmark-all-marks :wk "unmark all")

    "!" '(dired-do-shell-command :wk "shell command...")

    "d" '(:ignore t :wk "execute (marked)")
    "d c" '(dired-do-copy :wk "copy")
    "d D" '(dired-do-delete :wk "delete")
    "d h" '(dired-do-hardlink :wk "hardlink")
    "d s" '(dired-do-relsymlink :wk "symlink (relative)")
    "d S" '(dired-do-symlink :wk "symlink (absolute)")
    "d /" '(dired-do-search :wk "search")))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-clean-up-buffers-too t)
  :general
  (:keymaps 'dired-mode-map :states '(normal)
            "h" #'dired-omit-mode))

(use-package dired-plus
  :straight t
  :custom
  (diredp-wrap-around-flag nil)
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "C-'" #'diredp-up-directory-reuse-dir-buffer
           "j" #'diredp-next-line
           "k" #'diredp-previous-line)
  :hook (dired-mode . dired-hide-details-mode))

(use-package wdired
  :config
  (rk-local-leader-def :keymaps 'dired-mode-map
    "w" '(wdired-change-to-wdired-mode :wk "edit as buffer"))
  (rk-local-leader-def :keymaps 'wdired-mode-map
    "c" '(wdired-finish-edit :wk "commit changes")
    "k" '(wdired-abort-changes :wk "abort edit")))

(provide 'rk-dired)

;;; rk-dired.el ends here
