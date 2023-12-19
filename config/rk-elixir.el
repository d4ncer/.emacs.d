;;; rk-elixir.el --- Elixir setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'treesit-expand-region)
(require 'eglot)

(use-package elixir-ts-mode
  :straight t
  :preface
  (defun rk-elixir/single-blank-line-p ()
    (save-excursion
      (let (thisblank)
        (beginning-of-line)
        (setq thisblank (looking-at "[ \t]*$"))
        (and thisblank
             (not (looking-at "[ \t]*\n[ \t]*$"))
             (or (bobp)
                 (progn (forward-line -1)
                        (not (looking-at "[ \t]*$"))))))))
  (defun rk-elixir/return-and-indent-block ()
    (interactive)
    (save-excursion
      (newline-and-indent 2))
    (forward-line)
    (indent-according-to-mode)
    (unless (rk-elixir/single-blank-line-p)
      (delete-blank-lines)))
  :hook
  (elixir-ts-mode . rk-er/add-treesit-expander)
  :general
  (:keymaps 'elixir-ts-mode-map
            "C-<return>" #'rk-elixir/return-and-indent-block))

(use-package elixir-ts-mode
  :straight t
  :after yasnippet
  :preface
  (defun rk-elixir/setup-yas ()
    (yas-activate-extra-mode 'elixir-mode))
  :hook
  ((elixir-ts-mode . rk-elixir/setup-yas)))

;; [unused] lsp-mode config
;; (use-package elixir-ts-mode
;;   :straight t
;;   :preface
;;   (defvar
;;     rk-elixir/elixir-ls-bin
;;     (f-join paths-cache-directory "lsp-servers" "elixir-ls" "language_server.sh"))
;;   (defvar
;;     rk-elixir/lexical-bin
;;     (f-join gnus-home-directory "code" "lexical" "_build/dev/package/lexical/bin" "start_lexical.sh"))
;;   :custom
;;   (lsp-elixir-server-command `(,rk-elixir/lexical-bin))
;;   :hook
;;   ((elixir-ts-mode . lsp)
;;    (heex-ts-mode . lsp)))

;; Eglot config
(use-package elixir-ts-mode
  :straight t
  :preface
  (defvar
    rk-elixir/elixir-ls-bin
    (f-join paths-cache-directory "lsp-servers" "elixir-ls" "language_server.sh"))
  (defvar
    rk-elixir/lexical-bin
    (f-join gnus-home-directory "code" "lexical" "_build/dev/package/lexical/bin" "start_lexical.sh"))
  :config
  (add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) ,rk-elixir/elixir-ls-bin))
  :hook
  ((elixir-ts-mode . eglot-ensure)
   (heex-ts-mode . eglot-ensure)))

;; TODO: This will only work when eglot supports multiple servers
;; (use-package lsp-tailwindcss
;;   :straight t
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '((heex-ts-mode :language-id "html") "tailwindcss-language-server")))

;; (use-package elixir-ts-mode
;;   :straight t
;;   :after lsp-mode
;;   :hook
;;   ((elixir-ts-mode . lsp)
;;    (elixir-ts-mode . rk-er/add-treesit-expander)))

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

(use-package dumb-jump
  :straight t
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "elixir" :ext "heex" :agtype "elixir" :rgtype "elixir")))

(provide 'rk-elixir)

;;; rk-elixir.el ends here
