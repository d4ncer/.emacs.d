;;; rk-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

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
  (progn
    (autoload 'evil-global-set-key "evil-core")
    (autoload 'flyspell-auto-correct-word "flyspell"))
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "z u") #'flyspell-auto-correct-word)
    (evil-global-set-key 'normal (kbd "z p") #'rk-evil-ispell-previous-spelling-error)
    (evil-global-set-key 'normal (kbd "z n") #'rk-evil-ispell-next-spelling-error)
    (evil-global-set-key 'normal (kbd "z g") #'rk-evil-ispell-mark-word-as-good)
    (evil-global-set-key 'normal (kbd "z G") #'rk-evil-ispell-mark-word-as-locally-good)
    (evil-global-set-key 'normal (kbd "z =") #'rk-evil-ispell-correct-word)))

(provide 'rk-spelling)

;;; rk-spelling.el ends here
