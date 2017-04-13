;;; rk-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'evil-window-rotate-downwards "evil-commands")
(autoload 'rk/alternate-buffer "rk-alternate-buffer")
(autoload 'rk/copy-buffer-path "rk-copy-buffer-path")
(autoload 'rk/rename-file-and-buffer "rk-rename-file-and-buffer")
(autoload 'rk/sudo-edit "rk-sudo-edit")
(autoload 'rk/toggle-window-split "rk-toggle-window-split")
(autoload 'rk-goto-init-file "rk-goto")
(autoload 'rk-goto-messages "rk-goto")
(autoload 'rk-goto-personal-config "rk-goto")
(autoload 'org-narrow-to-subtree "org")

(use-package rk-delete-current-buffer-and-file
  :commands (rk/delete-current-buffer-and-file)
  :preface
  (progn
    (autoload 'projectile-invalidate-cache "projectile")
    (autoload 'projectile-project-p "projectile")

    (defun rk-leader-keys--invalidate-cache (_path)
      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))))

  :config
  (add-hook 'rk-delete-current-buffer-and-file-functions #'rk-leader-keys--invalidate-cache))

(use-package which-key
  :preface
  (progn
    (autoload 'which-key-mode "which-key")
    (autoload 'which-key-add-key-based-replacements "which-key"))

  :config
  (progn
    (setq which-key-special-keys nil)
    (setq which-key-use-C-h-commands t)
    (setq which-key-echo-keystrokes 0.02)
    (setq which-key-max-description-length 32)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-idle-delay 0.4)
    (setq which-key-allow-evil-operators t)

    ;; Rename functions shown by which-key for legibility.

    (add-to-list 'which-key-description-replacement-alist
                 (cons (rx bos "cb" (* (not (any "/"))) "/" (group (+ nonl)) eos) "\\1"))

    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC a"   "applications"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC h d" "describe"
      "SPC h f" "find"
      "SPC k"   "kill"
      "SPC n"   "narrow"
      "SPC o"   "org"
      "SPC p"   "project"
      "SPC q"   "quit"
      "SPC w"   "window"
      "SPC s"   "search/edit"
      "SPC t"   "toggles"
      "SPC SPC" "M-x"
      "SPC m"   '("major-mode-cmd" . "Major mode commands"))

    (which-key-mode +1)))

(use-package spacemacs-keys
  :preface
  (progn
    (autoload 'evil-window-next "evil-commands")
    (autoload 'evil-window-split "evil-commands")
    (autoload 'evil-window-vsplit "evil-commands")

    (defun rk-copy-whole-buffer-to-clipboard ()
      "Copy entire buffer to clipboard"
      (interactive)
      (clipboard-kill-ring-save (point-min) (point-max)))

    (defun rk-copy-clipboard-to-whole-buffer ()
      "Copy clipboard and replace buffer"
      (interactive)
      (delete-region (point-min) (point-max))
      (clipboard-yank)
      (deactivate-mark))

    (defun rk-reload-file ()
      "Revisit the current file."
      (interactive)
      (when-let (path (buffer-file-name))
        (find-alternate-file path))))

  :config
  (progn
    (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

    (spacemacs-keys-set-leader-keys
      "u"   #'universal-argument
      "SPC" #'execute-extended-command
      "TAB" #'rk/alternate-buffer
      "|"   #'rk/toggle-window-split

      "!"   #'shell-command

      "b d" #'kill-this-buffer
      "b b" #'bury-buffer
      "b Y" #'rk-copy-whole-buffer-to-clipboard
      "b P" #'rk-copy-clipboard-to-whole-buffer

      "C" #'compile

      "c r" #'comment-or-uncomment-region

      "f D" #'rk/delete-current-buffer-and-file
      "f F" #'find-file-other-window
      "f R" #'rk/rename-file-and-buffer
      "f e" #'rk/sudo-edit
      "f f" #'find-file
      "f s" #'save-buffer
      "f S" #'save-some-buffers
      "f W" #'write-file
      "f v" #'rk-reload-file
      "f y" #'rk/copy-buffer-path

      "g i" #'rk-goto-init-file
      "g m" #'rk-goto-messages
      "g p" #'rk-goto-personal-config

      "h d c" #'describe-face
      "h d k" #'describe-key
      "h d m" #'describe-mode
      "h d v" #'describe-variable
      "h f c" #'find-face-definition
      "h f f" #'find-function
      "h f l" #'find-library
      "h f v" #'find-variable
      "h i"   #'info

      "k b" #'kill-this-buffer
      "k w" #'delete-window

      "n d" #'narrow-to-defun
      "n f" #'narrow-to-defun
      "n r" #'narrow-to-region
      "n s" #'org-narrow-to-subtree
      "n w" #'widen

      "q w" #'delete-window
      "q q" #'kill-emacs

      "t F" #'toggle-frame-fullscreen

      "w =" #'balance-windows
      "w w" #'evil-window-next
      "w o" #'delete-other-windows
      "w q" #'delete-window
      "w r" #'evil-window-rotate-downwards
      "w -" #'evil-window-split
      "w /" #'evil-window-vsplit)))

(use-package rk-scale-font-transient-state
  :commands (rk-scale-font-transient-state/body)
  :init
  (spacemacs-keys-set-leader-keys
    "zx" #'rk-scale-font-transient-state/body))

(use-package rk-buffer-transient-state
  :commands (rk-buffer-transient-state/body
             rk-buffer-transient-state/next-buffer
             rk-buffer-transient-state/previous-buffer)
  :init
  (spacemacs-keys-set-leader-keys
    "bn" #'rk-buffer-transient-state/next-buffer
    "bN" #'rk-buffer-transient-state/previous-buffer
    "bp" #'rk-buffer-transient-state/previous-buffer))


(provide 'rk-leader-keys)

;;; rk-leader-keys.el ends here
