;;; rk-elixir.el --- Elixir setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'treesit-expand-region)
(require 'eglot)

(use-package elixir-ts-mode
  :init
  (when (treesit-ready-p 'elixir t)
    (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
    (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode)))

  :preface
  (defun rk-elixir/single-blank-line-p ()
    (save-excursion
      (let ((thisblank (looking-at "[ \t]*$")))
        (and thisblank
             (not (looking-at "[ \t]*\n[ \t]*$"))
             (or (bobp)
                 (progn (forward-line -1)
                        (not (looking-at "[ \t]*$"))))))))

  (defun rk-elixir/return-and-indent-block ()
    (interactive)
    (newline-and-indent 2)
    (forward-line)
    (indent-according-to-mode)
    (unless (rk-elixir/single-blank-line-p)
      (delete-blank-lines)))

  :hook
  (elixir-ts-mode . rk-er/add-treesit-expander)
  (elixir-ts-mode . rk-elixir/setup-yas)

  :general
  (:keymaps 'elixir-ts-mode-map
            "C-<return>" #'rk-elixir/return-and-indent-block))

(use-package yasnippet
  :after elixir-ts-mode
  :preface
  (defun rk-elixir/setup-yas ()
    (yas-activate-extra-mode 'elixir-mode)))

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

    (sp-local-pair "def" nil :actions nil)
    (sp-local-pair "defp" nil :actions nil)))

(use-package dumb-jump
  :straight t
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "elixir" :ext "heex" :agtype "elixir" :rgtype "elixir")))

(provide 'rk-elixir)

;;; rk-elixir.el ends here
