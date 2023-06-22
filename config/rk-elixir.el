;;; rk-elixir.el --- Elixir setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))


(use-package elixir-ts-mode
  :straight t)

(use-package elixir-ts-mode
  :straight t
  :after lsp-mode
  :hook
  ((elixir-ts-mode . lsp)

(provide 'rk-elixir)

;;; rk-elixir.el ends here
