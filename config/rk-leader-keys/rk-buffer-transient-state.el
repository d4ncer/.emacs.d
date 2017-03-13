;;; rk-buffer-transient-state.el --- Buffer navigation transient state.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'evil-transient-state)

(evil-transient-state-define rk-buffer
  :title "Buffer Selection Transient State"
  :bindings
  ("n" next-buffer "next")
  ("N" previous-buffer "previous")
  ("p" previous-buffer "previous")
  ("k" kill-this-buffer "kill")
  ("q" nil "quit" :exit t))

(provide 'rk-buffer-transient-state)

;;; rk-buffer-transient-state.el ends here
