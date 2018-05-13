;;; rk-undo-tree.el --- Configure undo-tree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package undo-tree
  :straight t
  :demand t
  :commands (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (global-undo-tree-mode)))

(provide 'rk-undo-tree)

;;; rk-undo-tree.el ends here
