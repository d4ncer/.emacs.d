;;; rk-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'straight)
(require 'spacemacs-keys)
(require 'evil)
(require 'subr-x)

(defun danc--elisp/eval-buffer ()
  "Evaluate the current buffer as Elisp code, within a straight transaction."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load buffer-file-name nil 'nomessage)))
  (message "Evaluating %s... done." (buffer-name)))

(spacemacs-keys-declare-prefix-for-mode 'emacs-lisp-mode "m e" "eval")
(spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
  "eb" #'danc--elisp/eval-buffer
  "ee" #'eval-expression)

;; Clean up which key labels

(use-package which-key
  :config
  (push `((", e" . ,(rx bos "eval-" (group (+ nonl)))) . (nil . "\\1"))
        which-key-replacement-alist))

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
      "ee" #'eval-expression
      "es" #'eval-last-sexp))

  :config
  (advice-add #'eval-buffer :after #'rk-elisp--message-on-eval-buffer))


(use-package elisp-slime-nav
  :straight t
  :commands (turn-on-elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point
             elisp-slime-nav-describe-elisp-thing-at-point)
  :bind
  (:map emacs-lisp-mode-map ("M-." . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (progn
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point
      (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)
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

(provide 'rk-elisp)

;;; rk-elisp.el ends here
