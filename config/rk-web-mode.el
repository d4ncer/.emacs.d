;;; rk-web-mode.el --- Configuration for HTML, CSS, JSON.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package json-ts-mode)

(use-package json-ts-mode
  :after lsp-mode
  :hook (json-ts-mode . lsp))

(use-package js)

(use-package js
  :after lsp-mode
  :hook (js-ts-mode . lsp))

(use-package css-mode)

(use-package css-mode
  :after lsp-mode
  :hook (css-ts-mode . lsp))

(use-package html-ts-mode)

(use-package html-ts-mode
  :after lsp-mode
  :hook (html-ts-mode . lsp))

(use-package prettier
  :straight t
  :preface
  ;; KLUDGE For some reason prettier gets loaded before direnv loads. This causes issues with
  ;; incorrect binaries.
  (defun rk/prettier-deferred ()
    (run-with-idle-timer 0 nil (lambda () (prettier-mode))))
  :after (js json-ts-mode css-mode html-ts-mode)
  :hook ((js-ts-mode json-ts-mode css-ts-mode html-ts-mode) . rk/prettier-deferred))

(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el" :branch "main")
  :after js
  :init
  (rk-local-leader-def :keymaps 'js-ts-mode-map
    "d" '(jsdoc :wk "jsdoc")))

(use-package jest
  :straight t
  :after js
  :init
  (rk-local-leader-def :keymaps 'js-ts-mode-map
    "t" '(jest :wk "test")))

(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
