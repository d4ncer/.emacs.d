;;; rk-treemacs.el --- Configure Treemacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-set-initial-state "evil-core")

(use-package treemacs
  :commands (treemacs
             treemacs-toggle
             treemacs-mode
             treemacs-next-line
             treemacs-previous-line)
  :config
  (progn
    (evil-set-initial-state 'treemacs-mode 'emacs)
    (define-key treemacs-mode-map (kbd "j") #'treemacs-next-line)
    (define-key treemacs-mode-map (kbd "k") #'treemacs-previous-line)
    (define-key treemacs-mode-map (kbd "SPC") spacemacs-keys-default-map))
  :init
  (spacemacs-keys-set-leader-keys
    "f t" #'treemacs-toggle))

(use-package treemacs-projectile
  :after treemacs
  :commands (treemacs-projectile)
  :init
  (spacemacs-keys-set-leader-keys
    "p t" #'treemacs-projectile))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
