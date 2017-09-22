;;; rk-embrace.el --- Configuration for embrace.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package embrace
  :commands
  (embrace-commander
   embrace-org-mode-hook)

  :init
  (progn

    ;; Add custom pairs to embrace
    (embrace-add-pair ?| "|" "|")
    (embrace-add-pair ?# "#" "#")

    (global-set-key (kbd "C-,") #'embrace-commander))

  :config
  (add-hook 'org-mode-hook #'embrace-org-mode-hook))

(provide 'rk-embrace)

;;; rk-embrace.el ends here
