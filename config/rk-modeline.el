;;; rk-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package rk-header-line-format
  :defines rk-header-line-format
  :config
  (setq-default header-line-format rk-header-line-format))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format " "))

(use-package rk-header-line-mode
  :commands (rk-header-line-global-mode rk-header-line-mode rk-header-line-mode-on)
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "tM" #'rk-header-line-mode
      "tm" #'rk-header-line-global-mode)
    (add-hook 'after-init-hook #'rk-header-line-global-mode))
  :config
  (setq rk-header-line-function (lambda () rk-header-line-format)))

(provide 'rk-modeline)

;;; rk-modeline.el ends here
