;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'treesit-expand-region)

(use-package typescript-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  :hook
  ((typescript-ts-mode tsx-ts-mode) . rk-er/add-treesit-expander))

(use-package typescript-ts-mode
  :after lsp-mode
  ;; :custom
  ;; (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=verbose" "--tsserver-log-file=/tmp/ts-logs.txt" "--log-level=4"))
  :hook
  ((typescript-ts-mode tsx-ts-mode) . lsp))

(use-package prettier
  :straight t
  :preface
  ;; KLUDGE For some reason prettier gets loaded before direnv loads. This causes issues with
  ;; incorrect binaries.
  (defun rk/prettier-deferred ()
    (run-with-idle-timer 0 nil (lambda () (prettier-mode))))
  :hook
  ((tsx-ts-mode typescript-ts-mode) . rk/prettier-deferred)
  :init
  (rk-local-leader-def :keymaps '(typescript-ts-mode-map tsx-ts-mode-map)
    "." '(prettier-prettify :wk "format")))

(use-package emmet-mode
  :straight t
  :general
  (:keymaps 'emmet-mode-keymap :states '(normal insert)
            "C-'" #'emmet-expand-line)
  :custom
  (emmet-move-cursor-between-quotes t)
  :hook
  (tsx-ts-mode . emmet-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

(use-package jest
  :straight t
  :init
  (rk-local-leader-def :keymaps 'typescript-ts-mode-map
    "t" '(jest :wk "test")))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
