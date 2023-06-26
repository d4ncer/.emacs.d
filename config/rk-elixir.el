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

(use-package smartparens
  :straight t
  :after elixir-ts-mode
  :preface
  (defun rk-sp/in-quoted-content-p (id action context)
    (let ((n (treesit-node-at (point))))
      (string= (treesit-node-type n) "quoted_content")))
  :config
  ;; KLUDGE: I'm overwriting the local pair defs shipped with
  ;; smartparens-elixir because `sp-in-comment-p' does not work
  ;; with elixir-ts-mode for some reason. I've re-written the
  ;; :unless clause to use a treesit-based guard.
  (sp-with-modes '(elixir-ts-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :skip-match 'sp-elixir-skip-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "def" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-bodyless-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "defp" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-bodyless-defp-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "defmodule" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "defimpl" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '("| "))
    (sp-local-pair "if" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "for" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-for-in-defimpl-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "cond" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "with" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "unless" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "case" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))
    (sp-local-pair "try" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :post-handlers '(sp-elixir-do-block-post-handler)
                   :skip-match 'sp-elixir-skip-keyword-list-def-p
                   :unless '(rk-sp/in-quoted-content-p))))

(provide 'rk-elixir)

;;; rk-elixir.el ends here
