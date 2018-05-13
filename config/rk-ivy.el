;;; rk-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(use-package swiper
  :straight t
  :commands swiper
  :config
  (progn
    (evil-global-set-key 'motion "/" #'swiper)
    (evil-global-set-key 'normal "/" #'swiper)))

(use-package ivy
  :commands (ivy-dispatching-done
             ivy-help
             ivy-immediate-done
             ivy-mode
             ivy-previous-line
             ivy-next-line
             ivy-partial-or-done
             ivy-resume
             ivy-switch-buffer
             ivy-wgrep-change-to-wgrep-mode)

  :preface
  (progn

    (autoload 'wgrep-finish-edit "wgrep")
    (autoload 'wgrep-abort-changes "wgrep")

    ;; KLUDGE: Declare dynamic var.
    (defvar org-startup-folded)

    (defun rk-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun rk-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "r" #'ivy-resume
      "b s" #'ivy-switch-buffer)

    (bind-key "C-c C-r" #'ivy-resume)
    (bind-key "C-x b" #'ivy-switch-buffer))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'rk-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-occur-mode-map (kbd "C-x C-w") #'ivy-wgrep-change-to-wgrep-mode)

    (spacemacs-keys-set-leader-keys-for-major-mode 'ivy-occur-grep-mode
      "w" #'ivy-wgrep-change-to-wgrep-mode
      "c" #'wgrep-finish-edit
      "k" #'wgrep-abort-changes)

    (define-key ivy-minibuffer-map (kbd "<f1>") #'rk-ivy-help)
    (define-key ivy-minibuffer-map (kbd "C-z") #'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
    (setq ivy-flx-limit 2000)

    (ivy-mode))

  :defines (ivy-use-virtual-buffers ivy-count-format))

(use-package flx
  :straight t
  :after ivy)

(use-package ivy-hydra
  :straight t
  :after ivy)

(use-package counsel
  :after swiper
  :commands (counsel-M-x
             counsel-descbinds
             counsel-describe-face
             counsel-describe-function
             counsel-describe-variable
             counsel-minibuffer-history
             counsel-find-file
             counsel-imenu
             counsel-faces
             counsel-colors-emacs
             counsel-list-processes
             counsel-colors-web
             counsel-command-history
             counsel-file-jump
             counsel-recentf
             counsel-yank-pop
             counsel-load-theme
             counsel-up-directory)
  :defines (counsel-rg-base-command)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel"))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "SPC" #'counsel-M-x
      "?"   #'counsel-descbinds
      "f f" #'counsel-find-file
      "f j" #'counsel-file-jump
      "f r" #'counsel-recentf
      "k r" #'counsel-yank-pop
      "i i" #'counsel-imenu
      "i f" #'counsel-faces
      "i e" #'counsel-colors-emacs
      "i w" #'counsel-colors-web
      "i c" #'counsel-command-history
      "i m" #'counsel-minibuffer-history
      "i p" #'counsel-list-processes
      "i t" #'counsel-load-theme
      "h d f" #'counsel-describe-function
      "h d v" #'counsel-describe-variable
      "h d c" #'counsel-describe-face)

    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-h f" #'counsel-describe-function))

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)
    (define-key ivy-minibuffer-map (kbd "C-h") #'counsel-up-directory)

    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command "rg -i -g '!.git/*' --no-heading --line-number --hidden --max-columns 120 --color never %s .")
    (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))

    (counsel-mode +1)))

(use-package rk-ivy-commands
  :after swiper
  :commands (rk-swiper-region-or-symbol
             rk-counsel-project-region-or-symbol
             rk-counsel-region-or-symbol)
  :init
  (spacemacs-keys-set-leader-keys
    "sS" #'rk-swiper-region-or-symbol
    "sP" #'rk-counsel-project-region-or-symbol
    "sF" #'rk-counsel-region-or-symbol))

(provide 'rk-ivy)

;;; rk-ivy ends here
