;;; rk-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(use-package ivy
  :commands (ivy-dispatching-done
             ivy-help
             ivy-immediate-done
             ivy-mode
             ivy-partial-or-done
             ivy-resume
             ivy-switch-buffer
             ivy-wgrep-change-to-wgrep-mode)

  :preface
  (progn
    (autoload 'rk-ivy-occur-then-wgrep "rk-ivy-occur-then-wgrep")

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
    (require 'flx)

    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'rk-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-minibuffer-map (kbd "<f1>") #'rk-ivy-help)
    (define-key ivy-occur-mode-map (kbd "C-x C-w") #'ivy-wgrep-change-to-wgrep-mode)
    (define-key ivy-minibuffer-map (kbd "C-z") #'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") #'rk-ivy-occur-then-wgrep)
    (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)

    (ivy-mode))

  :defines (ivy-use-virtual-buffers ivy-count-format))

(use-package counsel
  :commands (counsel-M-x
             counsel-descbinds
             counsel-describe-function
             counsel-describe-variable
             counsel-expression-history
             counsel-find-file
             counsel-imenu
             counsel-recentf
             counsel-yank-pop)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel")

    (defun rk-ivy--ag-populate-with-symbol-at-point (f &rest args)
      (if-let ((sym (symbol-at-point)))
          (apply f (symbol-name sym) (cdr args))
        (apply f args))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "SPC" #'counsel-M-x
      "?"   #'counsel-descbinds
      "f f" #'counsel-find-file
      "f r" #'counsel-recentf
      "k r" #'counsel-yank-pop
      "i"   #'counsel-imenu
      "h d f" #'counsel-describe-function
      "h d v" #'counsel-describe-variable)

    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-h f" #'counsel-describe-function))

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)

    ;; Prefill counsel-ag with the symbol at point.
    (advice-add 'counsel-ag :around #'rk-ivy--ag-populate-with-symbol-at-point)

    (counsel-mode +1)))

(use-package swiper
  :bind (("C-s" . swiper))
  :init
  (evil-global-set-key 'normal "/" #'swiper))

(provide 'rk-ivy)
