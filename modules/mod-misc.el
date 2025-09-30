;;; mod-misc.el --- Miscellaneous utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains miscellaneous visual enhancements and utilities:
;; - hideshow (code folding)
;; - page-break-lines (^L display)
;; - hide-mode-line (disable modeline in certain modes)
;; - paren (show-paren-mode)
;; - paren-face (dim parentheses)
;; - breadcrumb (navigation breadcrumbs)

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; Visual enhancements

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode-hook . hs-minor-mode))

(use-package page-break-lines :ensure t
  ;; Displays ^L page break characters as a horizontal rule. Useful for
  ;; demarcating sections of a file.
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1))

(use-package hide-mode-line :ensure (hide-mode-line
                                     :host github
                                     :repo "hlissner/emacs-hide-mode-line")
  ;; Disable the mode-line in situations where it's not useful.
  :hook ((completion-list-mode-hook
          Man-mode-hook
          ielm-mode-hook
          calendar-mode-hook
          eshell-mode-hook
          compilation-mode-hook
          help-mode-hook
          shell-command-mode-hook
          gptel-mode-hook
          org-node-mode-hook
          )
         . hide-mode-line-mode))


(use-package paren
  ;; Provides `show-paren-mode', which highlights the matched pair at point.
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay))

(use-package paren-face :ensure t
  ;; Adds a face for parentheses in lisps. I hijack it to dim semicolons and
  ;; other non-critical syntax elements in other langs.
  :hook (lisp-data-mode-hook c-ts-base-mode-hook)

  :config
  (setq-hook! 'c-ts-base-mode-hook
    paren-face-regexp (rx (any ";,"))))

(use-package breadcrumb :ensure t
  :custom
  (breadcrumb-idle-time 0.3))

(provide 'mod-misc)
;;; mod-misc.el ends here
