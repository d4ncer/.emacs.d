;;; rk-elixir.el --- Elixir setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'treesit-expand-region)

(use-package elixir-ts-mode
  :straight t)

(use-package elixir-ts-mode
  :straight t
  :after lsp-mode
  :hook
  ((elixir-ts-mode . lsp)
   (elixir-ts-mode . rk-er/add-treesit-expander)))

(provide 'rk-elixir)

;;; rk-elixir.el ends here
