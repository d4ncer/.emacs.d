;;; rk-expand-region.el --- Configuration for expand-region.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package expand-region
  :commands
  (er/expand-region)

  :init
  (spacemacs-keys-set-leader-keys "v" #'er/expand-region)

  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(provide 'rk-expand-region)

;;; rk-expand-region.el ends here
