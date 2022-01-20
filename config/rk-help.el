;;; rk-help.el --- Configuration for Help+.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package help-fns+
  :straight t
  :init
  (rk-leader-def
    "h d K" '(describe-keymap :wk "describe keymap")
    "h d F" '(describe-file :wk "describe file")
    "h d B" '(describe-buffer :wk "describe buffer")))

(use-package helpful
  :straight t
  :general
  (:keymaps 'emacs-lisp-mode-map :states '(normal)
            "K" #'helpful-at-point)
  :init
  (rk-leader-def
    "h d f" '(helpful-callable :wk "describe function")
    "h d v" '(helpful-variable :wk "describe variable")
    "h d k" '(helpful-key :wk "describe key")))

(provide 'rk-help)

;;; rk-help.el ends here
