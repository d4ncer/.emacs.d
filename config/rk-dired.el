;;; rk-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'rk-emacs)
(require 'spacemacs-keys)

(autoload 'evil-define-key "evil-core")

(use-package dired
  :defer t
  :commands (dired dired-hide-details-mode)
  :bind (:map spacemacs-keys-default-map ("d" . dired))
  :preface
  (progn
    (autoload 'diredp-next-line "dired+")
    (autoload 'diredp-previous-line "dired+")
    (autoload 'diredp-up-directory-reuse-dir-buffer "dired+")
    (autoload 'wdired-change-to-wdired-mode "wdired")

    (defun rk-dired--sort-directories-first (&rest _)
      "Sort dired listings with directories first."
      (save-excursion
        (let (buffer-read-only)
          (forward-line 2) ;; beyond dir. header
          (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
        (set-buffer-modified-p nil))))

  :init
  (progn
    (autoload 'diff-hl-dired-mode-unless-remote "diff-hl-dired")
    (spacemacs-keys-declare-prefix-for-mode 'dired-mode "s" "subdir")
    (spacemacs-keys-set-leader-keys "d" #'dired)
    (spacemacs-keys-set-leader-keys-for-major-mode 'dired-mode
      "d"  #'dired-hide-details-mode
      "si" #'dired-insert-subdir
      "sd" #'dired-kill-subdir
      "w"  #'wdired-change-to-wdired-mode)

    (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))

  :config
  (progn

    (put 'dired-find-alternate-file 'disabled nil)

    (setq-default dired-listing-switches "-alhv")
    (setq dired-dwim-target t)
    (setq dired-auto-revert-buffer t)
    (advice-add 'dired-readin :after #'rk-dired--sort-directories-first)

    (evil-define-key 'normal dired-mode-map (kbd "C-;") #'diredp-up-directory-reuse-dir-buffer)
    (evil-define-key 'normal dired-mode-map (kbd "j") #'diredp-next-line)
    (evil-define-key 'normal dired-mode-map (kbd "k") #'diredp-previous-line)))

(use-package dired-x
  :commands (dired-omit-mode)
  :init
  (progn
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (spacemacs-keys-set-leader-keys-for-major-mode
      'dired-mode
      "h" #'dired-omit-mode)

    (add-hook 'dired-mode-hook #'dired-omit-mode))
  :config
  (progn
    (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-omit-mode)
    (setq dired-omit-verbose nil)
    (setq dired-clean-up-buffers-too t)
    (setq dired-omit-files (rx bol (or (+ ".")
                                       (and "__pycache__" eol))))))

(use-package dired+
  :straight t
  :after dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package wdired
  :after dired)

(provide 'rk-dired)

;;; rk-dired.el ends here
