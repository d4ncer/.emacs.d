;;; rk-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)

(use-package ibuffer
  :commands (ibuffer-forward-line
             ibuffer-backward-line)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-never-show-predicates (list (rx (or "*Messages*"
                                               "*magit-"
                                               "*git-auto-push*"
                                               "*Backtrace*"
                                               "*new*"
                                               "*mu4e"
                                               "*Org"
                                               "*Flycheck error messages*"
                                               "*Help*"))))
  :general
  (:keymaps 'ibuffer-mode-map
            "j" #'ibuffer-forward-line
            "k" #'ibuffer-backward-line)
  :preface
  ;; HACK: Hide the cursor and use hl-line.
  (defun rk-ibuffer--hacky-show-line-only ()
    (run-with-timer 0.01 nil (lambda ()
                               (setq cursor-type nil)
                               (hl-line-mode +1))))
  :init
  (rk-local-leader-def :keymaps 'ibuffer-mode-map
    "o" '(ibuffer-other-window :wk "ib other window"))
  (rk-leader-def
    "b l" '(ibuffer :wk "ib"))
  :config
  (add-hook 'ibuffer-hook #'rk-ibuffer--hacky-show-line-only t))

(use-package ibuf-ext
  :commands (ibuffer-auto-mode)
  :custom
  (ibuffer-show-empty-filter-groups nil)
  :init
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode))

(use-package ibuffer-projectile
  :straight t
  :preface
  (defun rk-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (add-to-list 'ibuffer-filter-groups '("Dired" (mode . dired-mode)))
    (add-to-list 'ibuffer-filter-groups '("System" (predicate . (-contains? '("*Messages*" "*scratch*") (buffer-name)))))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :init
  (add-hook 'ibuffer-hook #'rk-ibuffer--setup-buffer))

(provide 'rk-ibuffer)

;;; rk-ibuffer.el ends here
