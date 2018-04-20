;;; rk-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)
(require 'dash)
(require 's)

(use-package yasnippet
  :defer 1

  :preface
  (progn
    (autoload 'sp-backward-delete-char "smartparens")
    (autoload 'evil-define-key "evil-core")

    (defun rk-yasnippet-preserve-indentation (f &rest args)
      (let ((col
             (save-excursion
               (back-to-indentation)
               (current-column))))
        (apply f args)
        (save-excursion
          (atomic-change-group
            (goto-char (line-beginning-position))
            (delete-horizontal-space)
            (indent-to col)))))

    (defun rk-yasnippet--maybe-goto-field-end ()
      "Move to the end of the current field if it has been modified."
      (-when-let* ((field (rk-yasnippet--current-field)))
        (when (and (yas--field-modified-p field)
                   (yas--field-contains-point-p field))
          (goto-char (rk-yasnippet--end-of-field)))))

    (defun rk-yasnippet-goto-field-end (&rest _)
      (rk-yasnippet--maybe-goto-field-end)
      (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
        (evil-insert-state)))

    (defun rk-yasnippet--current-field ()
      "Return the current active field."
      (and (boundp 'yas--active-field-overlay)
           yas--active-field-overlay
           (overlay-buffer yas--active-field-overlay)
           (overlay-get yas--active-field-overlay 'yas--field)))

    (defun rk-yasnippet--start-of-field ()
      (when-let* ((field (rk-yasnippet--current-field)))
        (marker-position (yas--field-start field))))

    (defun rk-yasnippet--end-of-field ()
      (when-let* ((field (rk-yasnippet--current-field)))
        (marker-position (yas--field-end field))))

    (defun rk-yasnippet--current-field-text ()
      "Return the text in the active snippet field."
      (when-let* ((field (rk-yasnippet--current-field)))
        (yas--field-text-for-display field)))

    (defun rk-yasnippet-clear-blank-field (&rest _)
      "Clear the current field if it is blank."
      (when-let* ((beg (rk-yasnippet--start-of-field))
                  (end (rk-yasnippet--end-of-field))
                  (str (rk-yasnippet--current-field-text)))
        (when (s-matches? (rx bos (+ space) eos) str)
          (delete-region beg end)
          t)))

    (defun rk-yasnippet-space ()
      "Clear and skip this field if it is unmodified.  Otherwise insert a space."
      (interactive "*")
      (let ((field (rk-yasnippet--current-field)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              (t
               (call-interactively #'self-insert-command)))))

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
      "yn" #'yas-new-snippet
      "yy" #'yas-insert-snippet)

    ;; Fix malformed face decl

    (defface yas-field-highlight-face
      '((t (:inherit region)))
      "The face used to highlight the currently active field of a snippet"))

  :config
  (progn
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-verbosity 0)
    (setq yas-minor-mode-map (make-sparse-keymap))
    (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))

    (yas-global-mode +1)

    (add-to-list 'yas-dont-activate-functions (lambda () (derived-mode-p 'term-mode)))

    ;; Define key bindings for fancy snippet navigation.

    (bind-key (kbd "C-,") #'yas-expand yas-minor-mode-map)
    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)

    (evil-define-key 'insert yas-keymap (kbd "SPC") #'rk-yasnippet-space)
    (bind-key (kbd "<backspace>") #'rk-yasnippet-backspace yas-keymap)

    ;; Advise editing commands.
    ;;
    ;; Pressing SPC in an unmodified field will clear it and switch to the next.
    ;;
    ;; Pressing S-TAB to go to last field will place point at the end of the field.

    (advice-add #'yas-next-field :before #'rk-yasnippet-clear-blank-field)
    (advice-add #'yas-prev-field :before #'rk-yasnippet-clear-blank-field)
    (advice-add #'yas-next-field :after #'rk-yasnippet-goto-field-end)
    (advice-add #'yas-prev-field :after #'rk-yasnippet-goto-field-end)

    ;; Ensure yasnippet expansion preserves current indentation. This can be a
    ;; problem in modes with significant whitespace, where the indentation
    ;; command unconditionally indents one step.

    (advice-add 'yas--expand-or-prompt-for-template :around #'rk-yasnippet-preserve-indentation))

  :commands
  (yas-expand
   yas-global-mode
   yas-insert-snippet
   yas-new-snippet
   yas-next-field
   yas-prev-field
   yas-visit-snippet-file)

  :functions
  (yas--skip-and-clear
   yas--field-contains-point-p
   yas--field-text-for-display))

(use-package warnings
  :defer t
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

;; (use-package cb-yas-elisp
;;   :after yasnippet)

;; (use-package cb-yas-js
;;   :after yasnippet)

;; (use-package cb-yas-haskell
;;   :after yasnippet)

(provide 'rk-yasnippet)

;;; rk-yasnippet.el ends here
