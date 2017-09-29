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
   embrace-add-pair
   embrace-org-mode-hook)

  :preface
  (progn
    (defun rk-embrace-all-mode-hook ()
      "Add some default pairs to all modes."
      (dolist (pair '((?| . ("|" . "|"))
                      (?# . ("#" . "#"))))
        (embrace-add-pair (car pair) (cadr pair) (cddr pair)))))

  :config
  (progn

    (global-set-key (kbd "C-,") #'embrace-commander)

    (add-hook 'prog-mode-hook #'rk-embrace-all-mode-hook)
    (add-hook 'org-mode-hook #'embrace-org-mode-hook)))

(provide 'rk-embrace)

;;; rk-embrace.el ends here
