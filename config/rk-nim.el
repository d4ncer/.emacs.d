;;; rk-nim.el --- Config for Nim  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nim-mode
  :straight t
  :mode ("\\.nim\\'" . nim-mode)
  :config
  (add-hook 'nim-mode-hook #'nimsuggest-mode))

(provide 'rk-nim)

;;; rk-nim.el ends here
