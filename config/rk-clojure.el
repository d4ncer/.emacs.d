;;; rk-clojure.el --- Clojure stuff  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package clojure-ts-mode
  :straight t)

(use-package cider
  :straight t
  :after clojure-ts-mode
  :general
  (:keymaps 'cider-repl-mode-map
            "C-r" #'cider-repl-history)
  :config
  (rk-local-leader-def :keymaps 'clojure-ts-mode-map
    "c"   '(cider-jack-in :wk "jack in")
    "e"   '(:ignore t :wk "eval")
    "e b" '(cider-eval-buffer :wk "eval buffer")
    "e s" '(cider-eval-last-sexp :wk "eval last sexp"))
  (rk-local-leader-def :keymaps 'cider-repl-mode-map
    "." '(cider-repl-handle-shortcut :wk "shortcuts")))

(use-package clj-refactor
  :straight t
  :after clojure-ts-mode
  :preface
  (defun rk-cider--setup-clj-refactor ()
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook
  (clojure-ts-mode . rk-cider--setup-clj-refactor))

(use-package inf-clojure
  :straight t)

(provide 'rk-clojure)

;;; rk-clojure.el ends here
