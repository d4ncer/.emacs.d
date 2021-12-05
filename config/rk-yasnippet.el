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
  :preface
  (progn
    (autoload 'sp-backward-delete-char "smartparens")
    (autoload 'ivy-completing-read "ivy")

    (defun rk-yasnippet--ivy-yas-prompt (prompt choices &optional display-fn)
      (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

    (defun rk-yasnippet--current-field ()
      "Return the current active field."
      (and (boundp 'yas--active-field-overlay)
           yas--active-field-overlay
           (overlay-buffer yas--active-field-overlay)
           (overlay-get yas--active-field-overlay 'yas--field)))

    (defun rk-yasnippet-backspace ()
      "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
      (interactive "*")
      (let ((field (rk-yasnippet--current-field))
            (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              (sp-mode?
               (call-interactively #'sp-backward-delete-char))
              (t
               (call-interactively #'backward-delete-char))))))

  :init
  (progn
    (rk-leader-def
      "yf" '(yas-visit-snippet-file :wk "visit snippet file")
      "ye" '(yas-expand :wk "expand snippet")
      "yn" '(yas-new-snippet :wk "new snippet"))

    ;; Fix malformed face decl
    (defface yas-field-highlight-face
      '((t (:inherit region)))
      "The face used to highlight the currently active field of a snippet"))

  :config
  (progn
    (setq doom-snippets-dir (f-join straight-base-dir "straight/build/doom-snippets"))
    (setq yas-snippet-dirs (list (concat paths-etc-directory "/yasnippet/snippets")))
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(rk-yasnippet--ivy-yas-prompt))
    (setq yas-verbosity 0)

    (yas-global-mode +1)

    (add-to-list 'yas-dont-activate-functions (lambda () (derived-mode-p 'term-mode)))
    ;; Define key bindings for fancy snippet navigation.

    (general-def :keymaps 'yas-minor-mode-map :states '(insert normal motion)
      "C-," #'yas-expand)

    (general-def :keymaps 'yas-keymap :states 'insert
      "SPC" #'rk-yasnippet-space)
    (general-def :keymaps 'yas-keymap :states '(insert normal motion)
      "<backspace>" #'rk-yasnippet-backspace))

  :functions
  (yas--skip-and-clear
   yas--field-contains-point-p
   yas--field-text-for-display))

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
  :after yasnippet)

(provide 'rk-yasnippet)

;;; rk-yasnippet.el ends here
