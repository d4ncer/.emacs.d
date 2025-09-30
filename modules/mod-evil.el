;;; mod-evil.el --- Evil mode and vim emulation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; Evil mode configuration including:
;; - evil core
;; - evil-escape
;; - evil-collection
;; - evil-surround
;; - evil-multiedit
;; - evil-iedit-state
;; - evil-org (evil integration for org-mode)
;; - Related packages: vundo, iedit, expand-region

;;; Code:

(use-package evil :ensure t
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t

  :init
  (evil-mode +1)

  :config
  (defun +shift-left (&optional beg end)
    "Shift left, keeping the region active.
    BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-left beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun +shift-right (&optional beg end)
    "Shift right, keeping the region active.
BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun +find-refs-at-point ()
    (interactive)
    (if-let* ((sym (thing-at-point 'symbol)))
        (xref-find-references sym)
      (call-interactively #'xref-find-references)))

  (defun +select-non-empty-line ()
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line)
    (backward-char 1))

  :general-config
  (:states 'emacs "ESC ESC" #'evil-normal-state)
  (:states 'visual
           "<" #'+shift-left
           ">" #'+shift-right)
  (:states '(normal motion)
           "gd" #'xref-find-definitions
           "gD" #'xref-find-definitions-other-window
           "gb" #'xref-go-back
           "R" #'+find-refs-at-point
           "C-l" #'+select-non-empty-line
           "C-j" #'evil-scroll-page-down
           "C-k" #'evil-scroll-page-up)
  (:states '(insert normal emacs)
           "M-." #'xref-find-definitions
           "C-x RET" #'insert-char)
  ;; `comment-indent-new-line' is a nicer default--it inserts comment delimiters
  ;; for you when you do a newline in a comment. However, it breaks
  ;; electric-pair's special newline padding functionality, so only call it if
  ;; we're actually on a comment.
  (:states 'insert "RET"
           (general-predicate-dispatch #'newline-and-indent
             (nth 4 (syntax-ppss)) ; at a comment?
             #'comment-indent-new-line))
  :custom
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  (evil-want-C-g-bindings)
  (evil-want-C-u-delete nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  ;; Cursor customisation
  :config
  (setq evil-motion-state-cursor '("#9b59b6" box)
        evil-visual-state-cursor '("#eee8d5" (hbar . 2))
        evil-normal-state-cursor '("#f1c40f" box)
        evil-insert-state-cursor '("#2ecc71" (bar . 2))
        evil-emacs-state-cursor  '("#3498db" hbar))

  :config
  ;; Keep shift-width in sync if mode changes.
  (setq-hook! 'after-change-major-mode
    evil-shift-width tab-width)

  :config
  (add-hook '+escape-hook
            (defun +evil-disable-ex-highlights-h ()
              (when (evil-ex-hl-active-p 'evil-ex-search)
                (evil-ex-nohighlight)
                t)))

  ;; Use more natural Emacs/readline keybindings in ex.
  :general-config
  (:keymaps '(evil-ex-completion-map evil-ex-search-keymap)
            "C-a" #'evil-beginning-of-line
            "C-b" #'evil-backward-char
            "C-f" #'evil-forward-char)

  :config
  (defun +delete-backward-word-no-kill (arg)
    "Like `backward-kill-word', but doesn't affect the kill-ring."
    (interactive "p")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (ignore-errors (backward-kill-word arg))))

  :general-config
  (:keymaps +default-minibuffer-maps
            "C-a"    #'move-beginning-of-line
            "C-r"    #'evil-paste-from-register
            "C-u"    #'evil-delete-back-to-indentation
            "C-v"    #'yank
            "C-h"    #'+delete-backward-word-no-kill))

(use-package evil-escape :ensure t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :init
  (evil-escape-mode))

(use-package vundo :ensure (vundo :host github :repo "casouri/vundo")
  ;; Visualise the Emacs undo history.
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-collection :ensure t
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :custom
  ;; Ensure we do not overwrite the global leader key binding.
  (evil-collection-key-blacklist '("SPC" "S-SPC"))

  ;; Org-mode derives from outline-mode; disable the outline bindings to prevent
  ;; conflicts.
  (evil-collection-outline-enable-in-minor-mode-p nil)

  ;; Be a bit smarter about the evil-collection load sequence; in particular,
  ;; set up bindings in hooks first time we activate a major-mode. This makes
  ;; key binding setup more performant and more predictable.
  :init
  (with-eval-after-load 'evil
    (require '+evil-collection)
    (+evil-collection-defer-install-to-mode-activation))
  :config
  (+evil-collection-init 'comint)

  ;; Fix leader keybindings that get clobbered by evil-collection.

  (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
    (general-define-key :keymaps (append evil-collection-magit-maps
                                         evil-collection-magit-section-maps)
                        :states '(normal)
                        "SPC" #'+leader-key)))

(use-package evil-surround :ensure t
  ;; Evil-surround makes the S key work as an operator to surround an
  ;; object with, e.g., matched parentheses.
  :hook ((text-mode-hook prog-mode-hook) . evil-surround-mode)
  ;; Use lowercase 's' for surround instead of 'S'.
  :general (:states '(visual) :keymaps 'evil-surround-mode-map "s" #'evil-surround-region)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\) . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\] . ("[" . "]"))
                               (?\{ . ("{" . "}"))
                               (?\} . ("{" . "}"))
                               (?# . ("#{" . "}"))
                               (?> . ("<" . ">"))
                               (?f . evil-surround-function)
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)))

  :config
  (add-hook! 'emacs-lisp-mode-hook
    (make-local-variable 'evil-surround-pairs-alist)
    (alist-set! evil-surround-pairs-alist ?` '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?' '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?f #'evil-surround-prefix-function)))

(use-package evil-multiedit :ensure t
  ;; Evil-compatible multiple cursors.
  :after evil
  :disabled t
  :config
  (evil-multiedit-default-keybinds)

  :init
  (defun +multiedit ()
    (interactive)
    (evil-normal-state)
    (unless (eolp)
      (forward-char -1))
    (evil-multiedit-match-all))

  :general
  (:states 'visual
           "E" (general-predicate-dispatch #'evil-multiedit-match-all
                 (equal last-command 'evil-visual-char) #'+multiedit))

  :general-config
  (:keymaps 'evil-multiedit-mode-map
   :states 'normal
   "Y" (defun +evil-multiedit-copy ()
         (interactive)
         (when-let* ((str (iedit-current-occurrence-string)))
           (kill-new str)
           (message "Copied to kill ring")))
   "<tab>" #'iedit-toggle-selection
   "n" #'evil-multiedit-next
   "N" #'evil-multiedit-prev
   "S" #'evil-multiedit--change-line))

(use-package iedit :ensure t)

(use-package evil-iedit-state :ensure t
  :after evil
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (require 'iedit))

(use-package expand-region :ensure t
  :custom
  (expand-region-contract-fast-key "V")
  (expand-region-reset-fast-key "r"))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode-hook . evil-org-mode)
  :custom
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-special-o/O '(table-row))
  (evil-org-use-additional-insert-keys nil)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'mod-evil)
;;; mod-evil.el ends here
