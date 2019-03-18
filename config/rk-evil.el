;;; rk-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)

(use-package evil
  :straight t
  :demand t
  :preface
  (defun rk-evil--sp-delete-and-join-compat (fn &rest args)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      (apply fn args)))
  (defun rk-evil--select-non-empty-line ()
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line)
    (backward-char 1))
  :general
  (:states '(normal motion)
           "C-l" #'rk-evil--select-non-empty-line
           "C-u" #'evil-scroll-page-up
           "C-d" #'evil-scroll-page-down)
  (:states 'motion
           "gb" #'xref-pop-marker-stack)
  (:keymaps 'help-mode-map
            :states 'motion
            "<escape>" #'quit-window
            "<tab>" #'forward-button
            "S-<tab>" #'backward-button
            "]" #'help-go-forward
            "[" #'help-go-back
            "gf" #'help-go-forward
            "gb" #'help-go-back
            "gh" #'help-follow-symbol)
  :init
  (progn
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil))
  :config
  (progn
    (evil-mode +1)
    (setq-default evil-shift-width 2)
    (setq-default evil-symbol-word-search t)

    (general-setq evil-want-visual-char-semi-exclusive t
                  evil-want-Y-yank-to-eol t)

    ;; Configure cursors.

    (general-setq evil-motion-state-cursor `(,rk-theme-cursor-purple box)
                  evil-visual-state-cursor `(,rk-theme-base-solarized-b2 (hbar . 2))
                  evil-normal-state-cursor `(,rk-theme-cursor-yellow box)
                  evil-insert-state-cursor `(,rk-theme-cursor-green (bar . 2))
                  evil-emacs-state-cursor  `(,rk-theme-cursor-blue hbar))

    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'rk-evil--sp-delete-and-join-compat))

  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol))

(use-package evil-terminal-cursor-changer
  :straight t
  :if (not (display-graphic-p))
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :straight t
  :after evil
  :commands (evil-surround-region
             evil-substitute)
  :preface
  (progn
    (defun rk/elisp/init-evil-surround-pairs ()
      (make-local-variable 'evil-surround-pairs-alist)
      (push '(?\` . ("`" . "'")) evil-surround-pairs-alist)))

  :general
  (:keymaps 'evil-surround-mode-map
   :states '(visual)
   "s" #'evil-surround-region
   "S" #'evil-substitute)
  :hook
  (emacs-lisp-mode-hook . rk/elisp/init-evil-surround-pairs)
  :init
  (progn
    (setq-default evil-surround-pairs-alist
                  '((?\( . ("(" . ")"))
                    (?\[ . ("[" . "]"))
                    (?\{ . ("{" . "}"))

                    (?\) . ("(" . ")"))
                    (?\] . ("[" . "]"))
                    (?\} . ("{" . "}"))

                    (?# . ("#{" . "}"))
                    (?b . ("(" . ")"))
                    (?B . ("{" . "}"))
                    (?> . ("<" . ">"))
                    (?$ . ("${" . "}"))
                    (?t . evil-surround-read-tag)
                    (?< . evil-surround-read-tag)
                    (?f . evil-surround-function)))
    (global-evil-surround-mode +1)))

(use-package evil-multiedit
  :after evil
  :straight t
  :commands (evil-multiedit-match-all)
  :general
  (:keymaps 'evil-multiedit-state-map
            "RET" #'evil-multiedit-toggle-or-restrict-region
            "C" nil
            "S" #'evil-multiedit--substitute
            "C-j" #'evil-multiedit-match-symbol-and-next
            "C-k" #'evil-multiedit-match-symbol-and-prev)
  (:states 'motion
           "RET" #'evil-multiedit-toggle-or-restrict-region)
  (:keymaps 'evil-multiedit-insert-state-map
            "C-j" #'evil-multiedit-match-symbol-and-next
            "C-k" #'evil-multiedit-match-symbol-and-prev)
  :init
  (rk-leader-def
    "se" '(evil-multiedit-match-all :wk "iedit - all matches")
    "sp" '(evil-multiedit-match-symbol-and-next :wk "iedit - match current / next")))

(use-package evil-ex
  :defer t
  :commands (evil-ex-define-cmd)
  :preface
  (progn
    (defun rk-evil-flyspell-on ()
      "Enable flyspell."
      (interactive)
      (turn-on-flyspell))

    (defun rk-evil-flyspell-off ()
      "Disable flyspell."
      (interactive)
      (turn-off-flyspell)))

  :config
  (progn
    (evil-ex-define-cmd "nospell" #'rk-evil-flyspell-off)
    (evil-ex-define-cmd "spell" #'rk-evil-flyspell-on)))

(use-package evil-ediff
  :straight t
  :after ediff)

(use-package evil-args
  :straight t
  :after evil
  :general
  (:keymaps 'evil-inner-text-objects-map "a" #'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (dolist (maj-mode (list 'magit-process-mode 'magit-log-mode 'magit-cherry-mode 'magit-mode 'magit-refs-mode 'magit-status-mode 'magit-diff-mode))
      (add-to-list 'evil-escape-excluded-major-modes maj-mode))
    (evil-escape-mode)))

(use-package evil-indent-plus
  :straight t
  :after evil
  :commands (evil-indent-plus-default-bindings)
  :config (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator)
  :preface
  (require 'rk-evil-nerd-commenter)
  :init
  (progn
    (rk-leader-def
      ";"  '(evilnc-comment-operator :wk "comment")
      "cl" '(rk-evil-nerd-commenter/comment-or-uncomment-lines :wk "comment/uncomment lines")
      "cL" '(rk-evil-nerd-commenter/comment-or-uncomment-lines-inverse :wk "comment/uncomment lines (inverse)")
      "cp" '(rk-evil-nerd-commenter/comment-or-uncomment-paragraphs :wk "comment/uncomment paragraphs")
      "cP" '(rk-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse :wk "comment/uncomment paragraphs (inverse)")
      "ct" '(rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line :wk "comment/uncomment to the line")
      "cT" '(rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse :wk "comment/uncomment to the line (inverse)")
      "cy" '(rk-evil-nerd-commenter/copy-and-comment-lines :wk "copy & comment lines")
      "cY" '(rk-evil-nerd-commenter/copy-and-comment-lines-inverse :wk "copy & comment lines (inverse)"))))

(use-package evil-matchit
  :straight t
  :after evil)

(use-package evil-numbers
  :straight t
  :general
  (:states '(normal)
    "+" #'evil-numbers/inc-at-pt
    "-" #'evil-numbers/dec-at-pt)
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt))

(use-package evil-search-highlight-persist
  :straight t
  :after evil
  :commands (global-evil-search-highlight-persist
             evil-search-highlight-persist-remove-all)

  :preface
  (autoload 'evil-ex-define-cmd "evil-ex")

  :config
  (progn
    (global-evil-search-highlight-persist)
    (evil-ex-define-cmd "noh" #'evil-search-highlight-persist-remove-all)))

(use-package vi-tilde-fringe
  :straight t
  :after evil
  :commands (vi-tilde-fringe-mode global-vi-tilde-fringe-mode)

  :preface
  (progn
    (defun rk-evil--vi-tilde-fringe-off ()
      (vi-tilde-fringe-mode -1))

    (defun rk-evil--vi-tilde-fringe-off-if-readonly ()
      (when buffer-read-only
        (vi-tilde-fringe-mode -1))))

  :config
  (progn
    (add-hook 'which-key-init-buffer-hook #'rk-evil--vi-tilde-fringe-off)
    (add-hook 'after-change-major-mode-hook #'rk-evil--vi-tilde-fringe-off-if-readonly)
    (global-vi-tilde-fringe-mode)))

(use-package rk-evil-shift
  :after evil
  :general
  (:states '(visual)
           "<" #'rk-evil-shift-left
           ">" #'rk-evil-shift-right))

(use-package evil-commands
  :after evil
  :commands (evil-window-next
             evil-window-split
             evil-window-vsplit
             evil-window-rotate-downwards)
  :config
  (progn
    (rk-leader-def
      "w w" '(evil-window-next :wk "next window")
      "w r" '(evil-window-rotate-downwards :wk "rotate windows")
      "w -" '(evil-window-split :wk "split windows -")
      "w /" '(evil-window-vsplit :wk "split windows |"))))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-mode-list '(ivy dired company racer git-timemachine ibuffer help info))
  (evil-collection-init))

(provide 'rk-evil)

;;; rk-evil.el ends here
