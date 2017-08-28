;;; rk-lobsters.el --- Configuration for lobste.rs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package ivy-lobsters
  :commands
  (ivy-lobsters)

  :init
  (spacemacs-keys-set-leader-keys
    "al" #'ivy-lobsters))

(provide 'rk-lobsters)

;;; rk-lobsters.el ends here
