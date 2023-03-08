;;; rk-clojure.el --- Clojure stuff  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package clojure-mode
  :straight t
  :config
  (define-clojure-indent
   (GET 2)
   (POST 2)
   (PUT 2)
   (PATCH 2)
   (DELETE 2)
   (match 1)
   (friend/authorize 1)))

(use-package cider
  :straight t
  :after clojure-mode
  :general
  (:keymaps 'cider-repl-mode-map
            "<return>" #'cider-repl-closing-return)
  :config
  (rk-local-leader-def :keymaps 'clojure-mode-map
    "c"   '(cider-jack-in :wk "jack in")
    "e"   '(:ignore t :wk "eval")
    "e b" '(cider-eval-buffer :wk "eval buffer")
    "e s" '(cider-eval-last-sexp :wk "eval last sexp")))

(use-package inf-clojure
  :straight t)

(provide 'rk-clojure)

;;; rk-clojure.el ends here
