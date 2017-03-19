;;; rk-ws-butler.el --- Configure ws-butler.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :defer 1
  :config
  (ws-butler-global-mode))

(provide 'rk-ws-butler)

;;; rk-ws-butler.el ends here
