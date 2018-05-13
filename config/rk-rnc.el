;;; rk-rnc.el --- Configuration for RelaxNG Compact mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package rnc-mode
  :straight t
  :mode ("\\.rnc\\'" . rnc-mode)
  :config
  (setq rnc-indent-level 2))

(provide 'rk-rnc)

;;; rk-rnc.el ends here
