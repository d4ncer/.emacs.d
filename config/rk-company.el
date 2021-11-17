;;; rk-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package company
  :straight t
  :demand t
  :hook (after-init . global-company-mode)

  :general
  (:keymaps 'company-active-map
            "C-<return>" #'company-complete-selection)

  :commands (company-select-next-or-abort
             company-select-previous-or-abort
             company-show-doc-buffer)
  :custom
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.2)
  (company-require-match nil)

  :config
  (dolist (map (list company-active-map company-search-map))
    (general-def map "C-j" #'company-select-next-or-abort)
    (general-def map "C-k" #'company-select-previous-or-abort)
    (general-def map "C-h" #'company-show-doc-buffer)
    (general-def map "C-w" nil)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package corfu
  :straight t
  :disabled t
  :custom
  (corfu-auto t)
  :general
  (:keymaps 'corfu-map
            "C-<return>" #'corfu-insert
            "C-j" #'corfu-next
            "C-k" #'corfu-previous
            "C-h" #'corfu-show-documentation)
  :init
  (corfu-global-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(provide 'rk-company)

;;; rk-company.el ends here
