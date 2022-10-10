;;; rk-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'rk-colors)

(use-package evil
  :straight t
  :demand t
  :preface
  (defun rk-evil--select-non-empty-line ()
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line)
    (backward-char 1))
  (defun rk-evil--find-refs-at-point ()
    (interactive)
    (if-let ((sym (thing-at-point 'symbol)))
        (xref-find-references sym)
      (call-interactively #'xref-find-references)))
  :general
  (:states '(normal motion)
           "R" #'rk-evil--find-refs-at-point
           "C-l" #'rk-evil--select-non-empty-line
           "C-u" #'evil-scroll-page-up
           "C-d" #'evil-scroll-page-down
           "C-." nil)
  (:states 'motion
           "gd" #'xref-find-definitions
           "gD" #'xref-find-definitions-other-window
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
  :custom
  (evil-want-visual-char-semi-exclusive nil)
  (evil-want-Y-yank-to-eol t)
  (evil-shift-width 2)
  (evil-symbol-word-search t)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; Configure cursors.
  (setq evil-motion-state-cursor `(,rk-colors-cursor-purple box)
        evil-visual-state-cursor `(,rk-colors-solarized-b2 (hbar . 2))
        evil-normal-state-cursor `(,rk-colors-cursor-yellow box)
        evil-insert-state-cursor `(,rk-colors-cursor-green (bar . 2))
        evil-emacs-state-cursor  `(,rk-colors-cursor-blue hbar))
  (evil-mode +1)
  :functions (evil-mode)
  :defines (evil-want-Y-yank-to-eol))

(use-package avy
  :straight t
  :after evil
  :general
  (:states '(normal motion)
           "g c" #'avy-goto-char-2))

(use-package evil-surround
  :straight t
  :after evil
  :commands (evil-surround-region
             evil-substitute)
  :preface
  (defun rk-elisp--init-evil-surround-pairs ()
    (make-local-variable 'evil-surround-pairs-alist)
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))

  :hook
  (emacs-lisp-mode-hook . rk-elisp--init-evil-surround-pairs)
  :init
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
  (global-evil-surround-mode +1))

(use-package evil-iedit-state
  :straight (:host github :repo "d4ncer/evil-iedit-state" :branch "master")
  :after evil
  :init
  (rk-leader-def
    "se" '(evil-iedit-state/iedit-mode :wk "iedit - all matches")))

(use-package evil-args
  :straight t
  :after evil
  :general
  (:keymaps 'evil-inner-text-objects-map "a" #'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-escape
  :straight t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :config
  (dolist (mode (list 'magit-process-mode 'magit-log-mode 'magit-cherry-mode 'magit-mode 'magit-refs-mode 'magit-status-mode 'magit-diff-mode))
    (add-to-list 'evil-escape-excluded-major-modes mode))
  (evil-escape-mode))

(use-package evil-indent-plus
  :straight t
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :straight t
  :after evil
  :preface
  (require 'rk-evil-nerd-commenter)
  :init
  (rk-leader-def
    ";"  '(evilnc-comment-operator :wk "comment")
    "cl" '(rk-evil-nerd-commenter/comment-or-uncomment-lines :wk "comment/uncomment lines")
    "cL" '(rk-evil-nerd-commenter/comment-or-uncomment-lines-inverse :wk "comment/uncomment lines (inverse)")
    "cp" '(rk-evil-nerd-commenter/comment-or-uncomment-paragraphs :wk "comment/uncomment paragraphs")
    "cP" '(rk-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse :wk "comment/uncomment paragraphs (inverse)")
    "ct" '(rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line :wk "comment/uncomment to the line")
    "cT" '(rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse :wk "comment/uncomment to the line (inverse)")
    "cy" '(rk-evil-nerd-commenter/copy-and-comment-lines :wk "copy & comment lines")
    "cY" '(rk-evil-nerd-commenter/copy-and-comment-lines-inverse :wk "copy & comment lines (inverse)")))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode +1))

(use-package evil-numbers
  :straight t
  :general
  (:states '(normal)
           "+" #'evil-numbers/inc-at-pt
           "-" #'evil-numbers/dec-at-pt))

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
  ;; Unbind some odd evil-insert commands
  (general-unbind :states '(insert)
    "C-w"
    "C-r"
    "C-k")
  (rk-leader-def
    "w w" '(evil-window-next :wk "next window")
    "w r" '(evil-window-rotate-downwards :wk "rotate windows")
    "w -" '(evil-window-split :wk "split windows -")
    "w /" '(evil-window-vsplit :wk "split windows |")))

(use-package evil-collection
  :straight t
  :after evil
  :custom
  (evil-collection-mode-list `(ebib cider dired racer company git-timemachine ibuffer help info docker magit forge (pdf pdf-view) ediff deadgrep custom))
  :config
  (evil-collection-init))

(provide 'rk-evil)

;;; rk-evil.el ends here
