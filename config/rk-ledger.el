;;; rk-ledger.el --- Config for ledger  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'general)
(require 'paths)
(require 'dash)
(require 's)

(use-package ledger-mode
  :straight t
  :demand t
  :mode ("\\.ledger$" . ledger-mode)
  :init
  (rk-local-leader-def :keymaps 'ledger-mode-map
    "." '(ledger-mode-clean-buffer :wk "format")
    "r" '(ledger-report :wk "report"))
  :custom
  (ledger-accounts-file rk-accounts--ledger-file)
  :config
  (general-setq ledger-reports
                `(("bal" "ledger -f %(ledger-file) bal")
                  ("reg" "ledger -f %(ledger-file) reg")
                  ("payee" "ledger -f %(ledger-file) reg @%(payee)")
                  ("account" "ledger -f %(ledger-file) reg %(account)"))))

(use-package org-capture
  :after ledger-mode
  :preface
  (defvar rk-org--ledger-base-template "%%(org-read-date) %%^{Payee%s}\n\tExpenses:%%^{Account%s}    %%^{Amount} NZD\n\t%s"
    "Base template")
  (defun rk-org--ledger-payees-list ()
    (let ((f ledger-accounts-file))
      (with-temp-buffer
        (insert-file-contents f)
        (ledger-payees-in-buffer))))
  (defun rk-org--ledger-expense-accounts-list ()
    (let* ((l (ledger-accounts-list))
           (fl (--filter (s-starts-with? "Expenses" it) l)))
      (--map (s-chop-prefix "Expenses:" it) fl)))

  (defun rk-org--align-ledger-clean-buffer ()
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'ledger-mode)
                   (-contains-p '("ls" "lj" "lc") (plist-get org-capture-plist :key)))
          (call-interactively #'ledger-mode-clean-buffer)))))
  (defun rk-org--ledger-template-entry (key label form template &rest kws)
    (append
     (list key label 'plain form template
           :empty-lines 1
           :immediate-finish t)
     kws))
  (defun rk-org--ledger-expense-template ()
    (format rk-org--ledger-base-template
            (s-join "|" (-concat '("") (rk-org--ledger-payees-list)))
            (s-join "|" (-concat '("") (rk-org--ledger-expense-accounts-list)))
            (plist-get org-capture-plist :account)))

  :init
  (add-hook 'org-capture-before-finalize-hook #'rk-org--align-ledger-clean-buffer)
  (let ((templates org-capture-templates)
        (ledger-templates (list
                           ;; Ledger templates
                           '("l" "Ledger")

                           (rk-org--ledger-template-entry
                            "lc" "Credit Card"
                            '(file rk-accounts--ledger-file)
                            '(function rk-org--ledger-expense-template)
                            :account "Liabilities:Visa")

                           (rk-org--ledger-template-entry
                            "ls" "Savings"
                            '(file rk-accounts--ledger-file)
                            '(function rk-org--ledger-expense-template)
                            :account "Assets:Savings")

                           (rk-org--ledger-template-entry
                            "lj" "Joint Checking"
                            '(file rk-accounts--ledger-file)
                            '(function rk-org--ledger-expense-template)
                            :account "Assets:Joint Checking"))))
    (setq org-capture-templates (-concat templates ledger-templates))))

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(provide 'rk-ledger)

;;; rk-ledger.el ends here
