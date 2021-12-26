;;; rk-tree-sitter.el --- Tree sitter config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tree-sitter
  :custom
  ;; Favour compilation rather than pulling binaries
  ;; from github until tree-sitter puts up m1 binaries.
  (tsc-dyn-get-from '(:compilation))
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query)
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode))

(provide 'rk-tree-sitter)

;;; rk-tree-sitter.el ends here
