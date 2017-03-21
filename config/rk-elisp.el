;;; rk-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-define-key "evil-core")

;; Print a message on `eval-buffer'.

(use-package elisp-mode
  :preface
  (defun rk-elisp--message-on-eval-buffer (&rest _)
    (when (called-interactively-p nil)
      (message "Buffer evaluated.")))

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'emacs-lisp-mode "m e" "eval")

    (spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "eb" #'eval-buffer
      "ee" #'eval-expression))

  :config
  (advice-add #'eval-buffer :after #'rk-elisp--message-on-eval-buffer))


(use-package elisp-slime-nav
  :commands (elisp-slime-nav-find-elisp-thing-at-point
             elisp-slime-nav-describe-elisp-thing-at-point)
  :bind
  (:map emacs-lisp-mode-map ("M-." . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (progn
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point
      (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)

    (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

  :commands (turn-on-elisp-slime-nav-mode))

(use-package eldoc
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

(use-package evil-surround
  :defer t
  :preface
  (defun rk-elisp--init-evil-surround-pairs ()
    (make-local-variable 'evil-surround-pairs-alist)
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
  :config
  (add-hook 'emacs-lisp-mode-hook #'rk-elisp--init-evil-surround-pairs))

;; Checkdoc configuration

(use-package rk-flycheck-checkdoc
  :after flycheck
  :config
  (setq flycheck-emacs-lisp-checkdoc-form rk-flycheck-checkdoc-form))

(use-package checkdoc
  :defer t
  :config
  (progn
    (setq checkdoc-force-docstrings-flag nil)
    (setq checkdoc-arguments-in-order-flag nil)))

(use-package ert
  :commands (ert)
  :preface
  (defun cb/ert-run-all-tests ()
    (interactive)
    (ert t))
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
    "t" #'cb/ert-run-all-tests))


(provide 'rk-elisp)

;;; rk-elisp.el ends here
