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
  :commands (dired dired-hide-details-mode)
  :preface
  (progn
    (general-setq dired-omit-files (rx bol (or (+ ".")
                                               (and "__pycache__" eol))))
    (autoload 'diredp-next-line "dired+")
    (autoload 'diredp-previous-line "dired+")
    (autoload 'diredp-up-directory-reuse-dir-buffer "dired+")
    (autoload 'wdired-change-to-wdired-mode "wdired")
    (autoload 'diff-hl-dired-mode-unless-remote "diff-hl-dired")

    (defun rk-dired--sort-directories-first (&rest _)
      "Sort dired listings with directories first."
      (save-excursion
        (let (buffer-read-only)
          (forward-line 2) ;; beyond dir. header
          (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
        (set-buffer-modified-p nil))))

  :init
  (progn
    (rk-leader-def
      "d" '(dired :wk "dired"))
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))

  :config
  (progn
    (rk-local-leader-def :keymaps 'dired-mode-map
      "s" '(nil :ignore t :wk "subdir")
      "d" '(dired-hide-details-mode :wk "toggle details")
      "si" '(dired-insert-subdir :wk "make subdir")
      "sd" '(dired-kill-subdir :wk "kill subdir")
      "w" '(wdired-change-to-wdired-mode :wk "edit as buffer"))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq-default dired-listing-switches "-alhv")
    (setq dired-dwim-target t)
    (setq dired-auto-revert-buffer t)
    (advice-add 'dired-readin :after #'rk-dired--sort-directories-first)
    (general-def 'normal dired-mode-map
      "C-;" #'diredp-up-directory-reuse-dir-buffer
      "j" #'diredp-next-line
      "k" #'diredp-previous-line)))

(use-package dired-x
  :after dired
  :commands (dired-omit-mode)
  :init
  (progn
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (add-hook 'dired-mode-hook #'dired-omit-mode))
  :general
  (:keymaps 'dired-mode-map :states '(normal)
            "h" #'dired-omit-mode)
  :config
  (rk-local-leader-def :keymaps 'dired-mode-map
    "h" '(dired-omit-mode :wk "switch to omit mode")))

(use-package dired-plus
  :straight t
  :defer t
  :custom
  (diredp-wrap-around-flag nil)
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "j" #'diredp-next-line
           "k" #'diredp-previous-line)
  :hook (dired-mode . dired-hide-details-mode))

(use-package wdired
  :after dired
  :config
  (rk-local-leader-def :keymaps 'wdired-mode-map
    "c" '(wdired-finish-edit :wk "commit changes")
    "k" '(wdired-abort-changes :wk "abort edit")))

(provide 'rk-dired)

;;; rk-dired.el ends here
