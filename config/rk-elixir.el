;;; rk-elixir.el --- Elixir setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package elixir-mode
  :straight t)

(use-package elixir-mode
  :straight t
  :after lsp-mode
  :preface
  (defun rk-elixir--setup ()
    (lsp-deferred))
  :hook
  (elixir-mode . rk-elixir--setup))

(provide 'rk-elixir)

;;; rk-elixir.el ends here
