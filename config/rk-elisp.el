;;; rk-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'general)
(require 'definers)
(require 'rk-misc-utils)
(require 'evil)
(require 'subr-x)

;; Print a message on `eval-buffer'.

(use-package elisp-mode
  :preface
  (defun rk-elisp--message-on-eval-buffer (&rest _)
    (when (called-interactively-p nil)
      (message "Buffer evaluated.")))
  :general
  (:keymaps 'emacs-lisp-mode-map :states '(normal motion)
            "gd" #'xref-find-definitions)
  :config
  (progn
    (rk-local-leader-def :keymaps 'emacs-lisp-mode-map
      "e" '(nil :ignore t :wk "eval")
      "eb" '(rk-elisp-eval-buffer :wk "eval buffer")
      "ee" '(eval-expression :wk "eval expression")
      "es" '(eval-last-sexp :wk "eval last sexp"))
    (advice-add #'eval-buffer :after #'rk-elisp--message-on-eval-buffer)))


(use-package elisp-slime-nav
  :straight t
  :commands (turn-on-elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point
             elisp-slime-nav-describe-elisp-thing-at-point)
  :general
  (:keymaps 'emacs-lisp-mode-map
            "M-." #'elisp-slime-nav-find-elisp-thing-at-point)
  :init
  (progn
    (general-def 'normal emacs-lisp-mode-map
      "M-." #'elisp-slime-nav-find-elisp-thing-at-point
      "K" #'elisp-slime-nav-describe-elisp-thing-at-point)
    (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)))

(use-package eldoc
  :defer t
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

(use-package checkdoc
  :defer t
  :config
  (progn
    (setq checkdoc-force-docstrings-flag nil)
    (setq checkdoc-arguments-in-order-flag nil)))

(use-package lisp-extra-font-lock
  :straight t
  :config
  (lisp-extra-font-lock-global-mode 1))

(provide 'rk-elisp)

;;; rk-elisp.el ends here
