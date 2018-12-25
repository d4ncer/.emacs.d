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
  :commands (global-company-mode)

  :general
  (:keymaps 'company-mode-map
            "S-<return>" #'company-complete)

  :preface
  (defun rk-company--set-company-vars ()
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :commands (company-select-next
             company-select-previous
             company-show-doc-buffer)

  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (progn
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)

    (dolist (map (list company-active-map company-search-map company-filter-map))
      (general-def map "C-j" #'company-select-next)
      (general-def map "C-k" #'company-select-previous)
      (general-def map "C-h" #'company-show-doc-buffer)
      (general-def map "C-w" nil))

    (add-hook 'company-mode-hook #'rk-company--set-company-vars)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package company-lsp
  :straight t
  :after (company lsp-mode)
  :defines company-lsp
  :preface
  (progn
    (defun rk-company--lsp-mode-p ()
      (and (bound-and-true-p lsp-mode)
           (bound-and-true-p company-mode)))
    (defun rk-company--setup-lsp-backend ()
      (when (rk-company--lsp-mode-p)
        (set (make-local-variable 'company-backends) '(company-lsp)))))
  :config
  (add-hook 'company-mode-hook #'rk-company--setup-lsp-backend))

(provide 'rk-company)

;;; rk-company.el ends here
