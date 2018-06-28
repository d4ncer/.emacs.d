;;; rk-treemacs.el --- Configure Treemacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'paths)
(autoload 'evil-set-initial-state "evil-core")

(use-package treemacs
  :straight t
  :commands (treemacs
             treemacs-add-project
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
  (progn
    (setq treemacs-persist-file (concat paths-cache-directory "/treemacs-persist"))
    (spacemacs-keys-set-leader-keys
      "f t" #'treemacs
      "p t" #'treemacs-add-project)))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
