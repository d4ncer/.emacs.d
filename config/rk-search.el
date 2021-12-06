;;; rk-search.el --- Configuration for search related utils.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'rk-misc-utils)

(use-package wgrep
  :straight t
  :init
  (add-hook 'grep-setup-hook #'wgrep-setup)
  :custom
  (wgrep-auto-save-buffer t)
  :general
  (:keymaps 'wgrep-mode-map :states '(normal motion visual emacs)
            ", c" #'rk-search-wgrep-finish-edit-kill-buffer
            ", k" #'rk-search-wgrep-abort-changes-kill-buffer))

(use-package deadgrep
  :straight t
  :general (:keymaps 'deadgrep-mode-map "C-c C-w" #'deadgrep-edit-mode)
  (:states 'normal :keymaps 'deadgrep-mode-map
           "c" #'rk-search--deadgrep-requery)
  :preface
  (defun rk-search--deadgrep-requery ()
    (interactive)
    (let ((button (save-excursion
                    (goto-char (point-min))
                    (forward-button 1))))
      (button-activate button)))
  (defun rk-search--on-enter-deadgrep-edit-mode (&rest _)
    (message "Entering edit mode. Changes will be made to underlying files as you edit."))
  (defun rk-search--on-exit-deadgrep-edit-mode (&rest _)
    (when (derived-mode-p 'deadgrep-edit-mode)
      (message "Exiting edit mode.")))
  :init
  (rk-leader-def
    "sd" '(deadgrep :wk "deadgrep"))
  :config
  (advice-add #'deadgrep-edit-mode :after #'rk-search--on-enter-deadgrep-edit-mode)
  (advice-add #'deadgrep-mode :before #'rk-search--on-exit-deadgrep-edit-mode))

(provide 'rk-search)

;;; rk-search.el ends here
