;;; flycheck-transient-state.el --- Flycheck transient state  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;; Flycheck transient state

;;; Code:

(require 'evil-transient-state)

(evil-transient-state-define rk-flycheck-ts
  :on-enter (progn (setq hydra-lv t) (flycheck-list-errors))
  :on-exit (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
  :title "Flycheck errors"
  :hint nil
  :foreign-keys run
  :doc "
[_j_] Next [_k_] Previous [_gg_] First [_G_] Last [_f_] Filter [_q_] Quit"
  :bindings
  ("f" flycheck-error-list-set-filter)
  ("j" flycheck-next-error)
  ("k" flycheck-previous-error)
  ("gg" flycheck-first-error)
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)))
  ("q" nil))

(provide 'flycheck-transient-state)

;;; flycheck-transient-state.el ends here
