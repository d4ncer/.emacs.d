;;; rk-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;;; TODO THIS NEEDS WORK WORK WORK WORK WORK (sung in a Rihanna voice)
;; (use-package spaceline
;;   :straight t)

;; (use-package spaceline-config
;;   :after spaceline
;;   :config
;;   (progn
;;     (spaceline-toggle-minor-modes-off)
;;     (spaceline-emacs-theme)))

;; (use-package rk-mode-line-format
;;   :defines rk-mode-line-format
;;   :config
;;   (setq-default mode-line-format rk-mode-line-format))

(use-package doom-modeline
  :config
  (+doom-modeline|init))

;; (use-package hidden-mode-line
;;   :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
;;   :init
;;   (setq-default mode-line-format " "))

;; (use-package rk-header-line-mode
;;   :commands (rk-header-line-global-mode rk-header-line-mode rk-header-line-mode-on)
;;   :init
;;   (progn
;;     (spacemacs-keys-set-leader-keys
;;       "TM" #'rk-header-line-mode
;;       "Tm" #'rk-header-line-global-mode)
;;     (add-hook 'after-init-hook #'rk-header-line-global-mode))
;;   :config
;;   (setq rk-header-line-function (lambda () rk-header-line-format)))

(provide 'rk-modeline)

;;; rk-modeline.el ends here
