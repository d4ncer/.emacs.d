;;; rk-treemacs.el --- Configure Treemacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package treemacs
  :commands (treemacs
             treemacs-toggle)
  :config
  (spacemacs-keys-set-leader-keys
    "f t" #'treemacs))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :commands (treemacs-projectile)
  :config
  (spacemacs-keys-set-leader-keys
    "p t" #'treemacs-projectile))

(provide 'rk-treemacs)

;;; rk-treemacs.el ends here
