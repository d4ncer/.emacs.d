;;; rk-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ispell
  :straight t
  :custom
  (ispell-silently-savep t)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")))

(use-package flyspell-correct
  :straight t
  :general
  (:keymaps 'flyspell-mode-map :states 'normal
            "z g" #'flyspell-correct-wrapper))

(provide 'rk-spelling)

;;; rk-spelling.el ends here
