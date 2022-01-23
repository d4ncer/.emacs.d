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
  :mode ("\\.ledger$" . ledger-mode)
  :init
  (rk-local-leader-def :keymaps 'ledger-mode-map
    "." '(ledger-mode-clean-buffer :wk "format")
    "r" '(ledger-report :wk "report"))
  :custom
  (ledger-accounts-file rk-accounts--ledger-file)
  (ledger-reports
   '(("account statement" "%(binary) reg --real [[ledger-mode-flags]] -f %(ledger-file) ^%(account)")
     ("balance sheet" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) bal ^assets ^liabilities ^equity")
     ("budget" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:budget")
     ("budget (long term)" "%(binary) --empty -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^assets:piggy")
     ("cleared" "%(binary) cleared [[ledger-mode-flags]] -f %(ledger-file)")
     ("equity" "%(binary) --real [[ledger-mode-flags]] -f %(ledger-file) equity")
     ("income statement" "%(binary) --invert --real -S -T [[ledger-mode-flags]] -f %(ledger-file) bal ^income ^expenses -p \"this month\""))))

(use-package org-capture
  :after (org ledger-mode)
  :preface
  (autoload 'ledger-payees-in-buffer "ledger-complete")
  (autoload 'ledger-accounts-list "ledger-complete")
  (autoload 'ledger-mode-clean-buffer "ledger-mode")
  (defvar ledger-accounts-file rk-accounts--ledger-file)
  (defvar rk-ledger--base-template "%%(org-read-date) %%^{Payee%s}\n\tExpenses:%%^{Account%s}    %%^{Amount} NZD\n\t%s")
  (defvar rk-ledger--inter-acc-template "%%(org-read-date) %%^{SelfPayee%s}\n\t%%^{ToAccount%s}    %%^{Amount} NZD\n\t%%^{FromAcc%s}")
  (defun rk-ledger--payees-list ()
    (let ((f ledger-accounts-file))
      (with-temp-buffer
        (insert-file-contents f)
        (ledger-payees-in-buffer))))

  (defun rk-ledger--expense-accounts-list ()
    (let* ((l (ledger-accounts-list))
           (fl (--filter (s-starts-with? "Expenses" it) l)))
      (--map (s-chop-prefix "Expenses:" it) fl)))

  (defun rk-ledger--self-payee-list ()
    (let* ((l (rk-ledger--payees-list))
           (fl (--filter (s-starts-with? "Self" it) l)))
      fl))

  (defun rk-ledger--non-expense-accounts-list ()
    (let* ((l (ledger-accounts-list))
           (fl (--filter (not (s-starts-with? "Expenses" it)) l)))
      fl))

  (defun rk-ledger--align-ledger-clean-buffer ()
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'ledger-mode)
                   (-contains-p '("s" "j" "c" "a") (plist-get org-capture-plist :key)))
          (call-interactively #'ledger-mode-clean-buffer)))))
  (defun rk-ledger--template-entry (key label form template &rest kws)
    (append
     (list key label 'plain form template
           :empty-lines 1
           :immediate-finish t)
     kws))
  (defun rk-ledger--expense-template ()
    (format rk-ledger--base-template
            (s-join "|" (-concat '("") (rk-ledger--payees-list)))
            (s-join "|" (-concat '("") (rk-ledger--expense-accounts-list)))
            (plist-get org-capture-plist :account)))

  (defun rk-ledger--inter-acc-template ()
    (format rk-ledger--inter-acc-template
            (s-join "|" (-concat '("") (rk-ledger--self-payee-list)))
            (s-join "|" (-concat '("") (rk-ledger--non-expense-accounts-list)))
            (s-join "|" (-concat '("") (rk-ledger--non-expense-accounts-list)))))

  :init
  (rk-leader-def
    "l"   '(:ignore t :wk "ledger")
    "l k" '(org-capture :wk "capture"))
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger))
  (add-hook 'org-capture-before-finalize-hook #'rk-ledger--align-ledger-clean-buffer)
  (let ((templates org-capture-templates)
        (ledger-templates (list
                           (rk-ledger--template-entry
                            "a" "Transfer between accounts"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--inter-acc-template))

                           (rk-ledger--template-entry
                            "c" "Credit Card"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Liabilities:Visa")

                           (rk-ledger--template-entry
                            "s" "Savings"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Assets:Savings")

                           (rk-ledger--template-entry
                            "j" "Joint Checking"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Assets:Joint Checking"))))
    (setq org-capture-templates (-concat templates ledger-templates))))

(use-package flycheck-ledger
  :straight t
  :after (flycheck ledger))

(provide 'rk-ledger)

;;; rk-ledger.el ends here
