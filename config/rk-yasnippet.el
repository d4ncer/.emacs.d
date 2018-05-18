;;; rk-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'paths)
(require 'subr-x)
(require 'dash)
(require 's)

(use-package yasnippet
  :straight t
  :defer 3

  :preface
  (progn
    (autoload 'sp-backward-delete-char "smartparens")
    (autoload 'evil-define-key "evil")
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
    (spacemacs-keys-declare-prefix "y" "yasnippet")
    (spacemacs-keys-set-leader-keys
      "yf" #'yas-visit-snippet-file
      "ye" #'yas-expand
      "yn" #'yas-new-snippet)

    ;; Fix malformed face decl

    (defface yas-field-highlight-face
      '((t (:inherit region)))
      "The face used to highlight the currently active field of a snippet"))

  :config
  (progn
    (setq yas-snippet-dirs (list (concat paths-etc-directory "/yasnippet/snippets")))
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(rk-yasnippet--ivy-yas-prompt))
    (setq yas-verbosity 0)

    (yas-global-mode +1)

    (add-to-list 'yas-dont-activate-functions (lambda () (derived-mode-p 'term-mode)))

    ;; Define key bindings for fancy snippet navigation.

    (bind-key (kbd "C-,") #'yas-expand yas-minor-mode-map)
    (evil-define-key 'insert yas-minor-mode-map (kbd "C-,") #'yas-expand)

    (evil-define-key 'insert yas-keymap (kbd "SPC") #'rk-yasnippet-space)
    (bind-key (kbd "<backspace>") #'rk-yasnippet-backspace yas-keymap))

  :functions
  (yas--skip-and-clear
   yas--field-contains-point-p
   yas--field-text-for-display))

(use-package warnings
  :defer t
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package rk-yas-utils
  :after yasnippet)

(use-package ivy-yasnippet
  :straight t
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "yy" #'ivy-yasnippet)))

(provide 'rk-yasnippet)

;;; rk-yasnippet.el ends here
