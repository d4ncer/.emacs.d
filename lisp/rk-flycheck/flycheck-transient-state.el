;;; flycheck-transient-state.el --- Flycheck transient state  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Flycheck transient state

;;; Code:

(require 'pretty-hydra)
(require 'flycheck)

(pretty-hydra-define rk-flycheck
  (:title "Flycheck"
          :quit-key "q"
          :foreign-keys run)
  ("Navigation"
   (("j" flycheck-next-error "next")
    ("k" flycheck-previous-error "previous")
    ("gg" flycheck-first-error "first")
    ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "last"))
   "Action"
   (("f" flycheck-error-list-set-filter "filter"))))

(provide 'flycheck-transient-state)

;;; flycheck-transient-state.el ends here
