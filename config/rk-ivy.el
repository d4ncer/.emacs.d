;;; rk-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'subr-x)
(require 'definers)
(require 'rk-misc-utils)

(use-package swiper
  :straight t
  :general
  (:states '(motion normal)
           "/" #'swiper)
  :config
  (general-def "s-f" #'swiper))

(use-package ivy
  :straight t
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
  :general
  (:keymaps 'ivy-occur-grep-mode-map
            :states '(normal motion visual emacs)
            ", c" #'rk-search-wgrep-finish-edit-kill-buffer
            ", k" #'rk-search-wgrep-abort-changes-kill-buffer
            ", w" #'ivy-wgrep-change-to-wgrep-mode)

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
  (rk-leader-def
    "r" '(ivy-resume :wk "ivy resume")
    "b s" '(ivy-switch-buffer :wk "switch buffer"))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

    ;; Do not show extra directories when finding files.

    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'rk-ivy-with-empty-ivy-extra-directories)

    (general-def ivy-minibuffer-map "C-z" #'ivy-dispatching-done)
    (general-def ivy-minibuffer-map "<escape>" 'minibuffer-keyboard-quit)
    (general-def ivy-minibuffer-map "C-l" #'ivy-partial-or-done)
    (general-def ivy-minibuffer-map "C-<return>" #'ivy-immediate-done)
    (general-def ivy-minibuffer-map "C-j" #'ivy-next-line)
    (general-def ivy-minibuffer-map "C-k" #'ivy-previous-line)

    (general-def ivy-switch-buffer-map "C-k" #'ivy-previous-line)
    (general-def ivy-switch-buffer-map "M-k" #'ivy-switch-buffer-kill)
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
  :straight t
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
  :custom
  (counsel-yank-pop-preselect-last t)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel"))

  :init
  (progn
    (rk-leader-def
      "SPC"   '(counsel-M-x :wk "M-x")
      "?"     '(counsel-descbinds :wk "describe bindings")
      "f f"   '(counsel-find-file :wk "find file")
      "f j"   '(counsel-file-jump :wk "find file in subdirs")
      "f r"   '(counsel-recentf :wk "recent files")
      "k r"   '(counsel-yank-pop :wk "kill ring")
      "i i"   '(counsel-imenu :wk "imenu")
      "i f"   '(counsel-faces :wk "list faces")
      "i e"   '(counsel-colors-emacs :wk "colors (emacs)")
      "i w"   '(counsel-colors-web :wk "colors (web)")
      "i c"   '(counsel-command-history :wk "history (commands)")
      "i m"   '(counsel-minibuffer-history :wk "history (minibuffer)")
      "i p"   '(counsel-list-processes :wk "list processes")
      "i t"   '(counsel-load-theme :wk "load theme")
      "h d f" '(counsel-describe-function :wk "describe function")
      "h d v" '(counsel-describe-variable :wk "describe variable")
      "h d c" '(counsel-describe-face :wk "describe face"))

    (general-def "M-x" #'counsel-M-x)
    (general-def "C-h v" #'counsel-describe-variable)
    (general-def "C-h f" #'counsel-describe-function))

  :config
  (progn
    (general-def counsel-find-file-map "C-h" 'counsel-up-directory)
    (general-def counsel-find-file-map "C-M-j" #'ivy-immediate-done)
    (general-def counsel-find-file-map "C-h" #'counsel-up-directory)
    (general-def ivy-minibuffer-map "C-h" #'counsel-up-directory)

    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command "rg -i -g '!.git/*' --no-heading --line-number --hidden --max-columns 120 --color never %s .")
    (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))

    (counsel-mode +1)))

(use-package rk-ivy-commands
  :after (ivy projectile)
  :commands (rk-swiper-region-or-symbol
             rk-counsel--switch-project
             rk-counsel-project-region-or-symbol
             rk-counsel-region-or-symbol
             rk-counsel-deadgrep-from-ivy)
  :config
  (general-def counsel-ag-map "C-c C-e" '(rk-counsel-deadgrep-from-ivy :wk "deadgrep"))
  :init
  (rk-leader-def
    "pp" '(rk-counsel--switch-project :wk "switch project")
    "sS" '(rk-swiper-region-or-symbol :wk "search in buffer")
    "sP" '(rk-counsel-project-region-or-symbol :wk "search in project")
    "sI" '(rk-counsel-i18n-project-region-or-symbol :wk "search i18n in project")
    "sF" '(rk-counsel-region-or-symbol :wk "search in dir")))

(use-package ivy-posframe
  :straight t
  :after ivy
  :hook (after-init . ivy-posframe-mode)
  :custom
  (ivy-posframe-style 'frame-center)
  (ivy-posframe-border-width 20)
  (ivy-posframe-hide-minibuffer t)
  (ivy-posframe-parameters '((alpha 100 100))))

(provide 'rk-ivy)

;;; rk-ivy ends here
