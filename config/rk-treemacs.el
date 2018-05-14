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
  :preface
  (defun rk-treemacs--setup-header-line ()
    (-when-let- [b (treemacs-buffer-exists?)]
      (when (buffer-live-p b)
        (with-current-buffer b
          (setq-local header-line-format "Tree you later")
          (force-mode-line-update)))))
  :config
  (progn
    (setq treemacs--persist-file (concat paths-cache-directory "/treemacs-persist"))
    (evil-set-initial-state 'treemacs-mode 'emacs)
    (define-key treemacs-mode-map (kbd "j") #'treemacs-next-line)
    (define-key treemacs-mode-map (kbd "k") #'treemacs-previous-line)
    (define-key treemacs-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (add-hook 'buffer-list-update-hook #'rk-treemacs--setup-header-line))
  :init
  (spacemacs-keys-set-leader-keys
    "f t" #'treemacs
    "p t" #'treemacs-add-project))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
