;;; rk-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

;; Print a message on `eval-buffer'.

(use-package elisp-mode
  :preface
  (defun rk-elisp--message-on-eval-buffer (&rest _)
    (when (called-interactively-p nil)
      (message "Buffer evaluated.")))
  :init
  (rk-local-leader-def :keymaps 'emacs-lisp-mode-map
    "e" '(nil :ignore t :wk "eval")
    "eb" '(rk-elisp-eval-buffer :wk "eval buffer")
    "ee" '(eval-expression :wk "eval expression")
    "es" '(eval-last-sexp :wk "eval last sexp"))
  :config
  (advice-add #'eval-buffer :after #'rk-elisp--message-on-eval-buffer))


(use-package elisp-slime-nav
  :straight t
  :general
  (:keymaps 'emacs-lisp-mode-map :states '(normal)
            "gd" #'elisp-slime-nav-find-elisp-thing-at-point)
  :config
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :custom (eldoc-idle-delay 0.2))

(use-package checkdoc
  :custom
  (checkdoc-force-docstrings-flag nil)
  (checkdoc-arguments-in-order-flag nil))

(use-package lisp-extra-font-lock
  :straight t
  :config
  (lisp-extra-font-lock-global-mode 1))

(provide 'rk-elisp)

;;; rk-elisp.el ends here
