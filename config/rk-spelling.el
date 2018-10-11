;;; rk-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package ispell
  :straight t
  :config
  (progn
    (when (executable-find "aspell")
      (setq ispell-program-name "aspell"))
    (setq ispell-silently-savep t)))

(use-package rk-evil-ispell
  :commands (rk-evil-ispell-previous-spelling-error
             rk-evil-ispell-next-spelling-error
             rk-evil-ispell-mark-word-as-good
             rk-evil-ispell-mark-word-as-locally-good
             rk-evil-ispell-correct-word)
  :preface
  (autoload 'flyspell-auto-correct-word "flyspell")
  :init
  (with-eval-after-load 'evil
    (general-def :states 'normal
      "z u" #'flyspell-auto-correct-word
      "z p" #'rk-evil-ispell-previous-spelling-error
      "z n" #'rk-evil-ispell-next-spelling-error
      "z g" #'rk-evil-ispell-mark-word-as-good
      "z G" #'rk-evil-ispell-mark-word-as-locally-good
      "z =" #'rk-evil-ispell-correct-word)))

(provide 'rk-spelling)

;;; rk-spelling.el ends here
