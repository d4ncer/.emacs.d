;;; rk-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'flyspell)

(use-package ispell
  :straight t
  :config
  (progn
    (when (executable-find "aspell")
      (setq ispell-program-name "aspell"))
    (setq ispell-silently-savep t)))

(use-package flyspell-correct-ivy
  :straight t
  :general
  (:keymaps 'flyspell-mode-map :states 'normal
            "z g" #'flyspell-correct-wrapper)
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'rk-spelling)

;;; rk-spelling.el ends here
