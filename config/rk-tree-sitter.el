;;; rk-tree-sitter.el --- Tree sitter config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tree-sitter
  :custom
  (tsc-dyn-get-from '(:compilation))
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query)

  (dolist (entry '((rk-web-tsx-mode . tsx) (rk-web-css-mode . css) (rk-web-html-mode . html) (rk-web-js-mode . javascript)))
    (add-to-list 'tree-sitter-major-mode-language-alist entry))
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode))

(provide 'rk-tree-sitter)

;;; rk-tree-sitter.el ends here
