;;; rk-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package evil
  :preface
  (defun rk-evil--sp-delete-and-join-compat (fn &rest args)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      (apply fn args)))

  :config
  (progn
    (evil-mode +1)
    (setq-default evil-shift-width 2)
    (setq-default evil-symbol-word-search t)

    (setq evil-want-visual-char-semi-exclusive t)
    (setq evil-want-Y-yank-to-eol t)

    ;;; Rebind C-u to scroll up

    (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-page-up)
    (define-key evil-motion-state-map (kbd "C-u") #'evil-scroll-page-up)

    (define-key evil-normal-state-map (kbd "C-d") #'evil-scroll-page-down)
    (define-key evil-motion-state-map (kbd "C-d") #'evil-scroll-page-down)

    ;; Bind g b to pop def stack

    (define-key evil-motion-state-map "gb" 'xref-pop-marker-stack)

    ;; Configure cursors.

    (setq evil-motion-state-cursor '("plum3" box))
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

    ;; Motion keys for help buffers.

    (evil-define-key 'motion help-mode-map (kbd "<escape>") #'quit-window)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") #'backward-button)
    (evil-define-key 'motion help-mode-map (kbd "]") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "gf") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "^") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "[") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gb") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gh") #'help-follow-symbol)

    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'rk-evil--sp-delete-and-join-compat))

  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol))

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-region)

  :preface
  (autoload 'evil-substitute "evil-commands")

  :config
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

    (global-evil-surround-mode)
    (evil-define-key 'visual evil-surround-mode-map "s" #'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" #'evil-substitute)))

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (spacemacs-keys-set-leader-keys "se" #'evil-iedit-state/iedit-mode)

  :config
  (progn
    (setq iedit-toggle-key-default nil)

    ;; Enable leader key in iedit and iedit-insert states
    (define-key evil-iedit-state-map (kbd "SPC") spacemacs-keys-default-map)))

(use-package evil-ex
  :defer t
  :functions (evil-ex-define-cmd)
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
  :after ediff)

(use-package evil-args
  :after evil
  :config
  (progn
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-escape
  :after evil
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (evil-escape-mode)))

(use-package evil-indent-plus
  :after evil
  :commands (evil-indent-plus-default-bindings)
  :config (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator)
  :preface
  (require 'rk-evil-nerd-commenter)
  :init
  (progn
    ;; Double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

    (spacemacs-keys-set-leader-keys
      ";"  #'evilnc-comment-operator
      "cl" #'rk-evil-nerd-commenter/comment-or-uncomment-lines
      "cL" #'rk-evil-nerd-commenter/comment-or-uncomment-lines-inverse
      "cp" #'rk-evil-nerd-commenter/comment-or-uncomment-paragraphs
      "cP" #'rk-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse
      "ct" #'rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line
      "cT" #'rk-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse
      "cy" #'rk-evil-nerd-commenter/copy-and-comment-lines
      "cY" #'rk-evil-nerd-commenter/copy-and-comment-lines-inverse)))

(use-package evil-matchit
  :after evil)

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)

  :init
  (progn
    (evil-global-set-key 'normal (kbd "+") #'evil-numbers/inc-at-pt)
    (evil-global-set-key 'normal (kbd "-") #'evil-numbers/dec-at-pt)))

(use-package evil-search-highlight-persist
  :after evil
  :commands (global-evil-search-highlight-persist
             evil-search-highlight-persist-remove-all)

  :preface
  (autoload 'evil-ex-define-cmd "evil-ex")

  :config
  (progn
    (global-evil-search-highlight-persist)
    (evil-ex-define-cmd "noh" #'evil-search-highlight-persist-remove-all)))

(use-package evil-visual-mark-mode
  :disabled t
  :after evil
  :commands (evil-visual-mark-mode)
  :config (evil-visual-mark-mode))

(use-package vi-tilde-fringe
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
  :preface
  (autoload 'evil-visual-state-map "evil-states")
  :bind (:map evil-visual-state-map
              ("<" . rk-evil-shift-left)
              (">" . rk-evil-shift-right)))

(provide 'rk-evil)

;;; rk-evil.el ends here
