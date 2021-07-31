;;; rk-hl-todo.el --- Configuration for todo highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)

(use-package hl-todo
  :straight t
  :defer t
  :custom
  (hl-todo-keyword-faces
   (--map (cons it 'hl-todo)
          '("TODO"
            "NEXT"
            "HACK"
            "FIXME"
            "KLUDGE"
            "NOTE")))
  :preface
  (defun rk-hl-todo--enable-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))

  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'text-mode-hook #'rk-hl-todo--enable-unless-org-buffer))

(provide 'rk-hl-todo)

;;; rk-hl-todo.el ends here
