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
  :demand t
  :general
  (:keymaps 'ledger-mode-map :states '(normal motion visual)
            "C-j" #'ledger-navigate-next-xact-or-directive
            "C-k" #'ledger-navigate-prev-xact-or-directive
            "C-n" #'ledger-navigate-next-uncleared
            "C-p" #'ledger-navigate-previous-uncleared)
  :preface
  ;; TODO: Improve this API to have more than one tag
  (defun rk-ledger--add-meta (tag)
    (save-excursion
      (ledger-navigate-beginning-of-xact)
      (end-of-line)
      (insert (format "\n\t; :%s:" tag))))

  (defun rk-ledger--add-housemove ()
    (interactive)
    (rk-ledger--add-meta "housemove22"))
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
     ("house move expenses" "%(binary) --real [[ledger-mode-flags]] bal '%housemove22'")
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
                   (-contains-p '("ls" "lj" "lc" "la") (plist-get org-capture-plist :key)))
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

  :config
  (add-hook 'org-capture-before-finalize-hook #'rk-ledger--align-ledger-clean-buffer)
  (let ((templates org-capture-templates)
        (ledger-templates (list
                           ;; Ledger templates
                           '("l" "Ledger")

                           (rk-ledger--template-entry
                            "la" "Transfer between accounts"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--inter-acc-template))

                           (rk-ledger--template-entry
                            "lc" "Credit Card"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Liabilities:Visa")

                           (rk-ledger--template-entry
                            "ls" "Savings"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Assets:Savings")

                           (rk-ledger--template-entry
                            "lj" "Joint Checking"
                            '(file rk-accounts--ledger-file)
                            '(function rk-ledger--expense-template)
                            :account "Assets:Joint Checking"))))
    (setq org-capture-templates (-concat templates ledger-templates))))

(use-package flycheck-ledger
  :straight t
  :after flycheck)

(provide 'rk-ledger)

;;; rk-ledger.el ends here
