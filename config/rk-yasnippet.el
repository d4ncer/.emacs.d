;;; rk-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)
(require 'paths)
(require 'subr-x)
(require 'dash)
(require 's)

(use-package yasnippet
  :straight t
  :hook
  (prog-mode . (lambda () (require 'yasnippet)))
  (text-mode . (lambda () (require 'yasnippet)))

  :custom
  (yas-wrap-around-region t)
  (yas-alias-to-yas/prefix-p nil)
  (yas-prompt-functions '(yas-completing-prompt))
  (yas-verbosity 0)
  (yas-minor-mode-map (make-sparse-keymap))

  (yas-snippet-dirs (list (concat paths-etc-directory "/yasnippet/snippets")))


  :general
  (:keymaps 'yas-minor-mode-map :states 'insert
            "TAB"
            (general-predicate-dispatch 'indent-for-tab-command
              (yas-maybe-expand-abbrev-key-filter t) 'yas-expand))
  (:keymaps 'yas-keymap :states 'insert
            "SPC"
            (general-predicate-dispatch 'self-insert-command
              (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field))

  :init
  (rk-leader-def
    "yf" '(yas-visit-snippet-file :wk "visit snippet file")
    "ye" '(yas-expand :wk "expand snippet")
    "yn" '(yas-new-snippet :wk "new snippet"))

  :config
  (yas-global-mode +1))

(use-package yasnippet
  :straight t
  :after smartparens
  :general
  (:keymaps 'yas-keymap :states 'insert
            "<backspace>"
            (general-predicate-dispatch 'backward-delete-char
              (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field
              (bound-and-true-p smartparens-mode) 'sp-backward-delete-char)))

(use-package yasnippet
  :straight t
  :config
  (defun rk-yasnippet--end-of-field ()
    (when-let* ((field (yas-current-field)))
      (marker-position (yas--field-end field))))

  (defun rk-yasnippet--maybe-goto-field-end ()
    "Move to the end of the current field if it has been modified."
    (when-let* ((field (yas-current-field)))
      (when (and (yas--field-modified-p field)
                 (yas--field-contains-point-p field))
        (goto-char (rk-yasnippet--end-of-field)))))

  (defun yasnippet-goto-field-end (&rest _)
    (rk-yasnippet--maybe-goto-field-end)
    (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
      (evil-insert-state)))

  (advice-add 'yas-next-field :after #'yasnippet-goto-field-end)
  (advice-add 'yas-prev-field :after #'yasnippet-goto-field-end))

(use-package warnings
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package yas-funcs
  :after yasnippet)

(use-package consult-yasnippet
  :straight t
  :after (yasnippet consult)
  :general
  (:states '(insert)
           "C-y" #'consult-yasnippet)
  :init
  (rk-leader-def
    "y y" '(consult-yasnippet :wk "insert snippet")))

(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets"
                   :branch "master"
                   :files ("*"))
  :custom
  (doom-snippets-dir (f-join straight-base-dir "straight/build/doom-snippets"))
  :after yasnippet)

(provide 'rk-yasnippet)

;;; rk-yasnippet.el ends here
