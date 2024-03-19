;;; rk-web-mode.el --- Configuration for HTML, CSS, JSON.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'treesit-expand-region)

(use-package json-ts-mode
  :hook
  (json-ts-mode . rk-er/add-treesit-expander))

(use-package json-ts-mode
  :after lsp-mode
  :hook (json-ts-mode . lsp))

(use-package js
  :hook
  (js-ts-mode . rk-er/add-treesit-expander))

(use-package js
  :after lsp-mode
  :hook (js-ts-mode . lsp))

(use-package css-mode
  :hook
  (css-ts-mode . rk-er/add-treesit-expander))

(use-package css-mode
  :after lsp-mode
  :hook (css-ts-mode . lsp))

(use-package html-ts-mode
  :hook
  (html-ts-mode . rk-er/add-treesit-expander))

(use-package html-ts-mode
  :after lsp-mode
  :hook (html-ts-mode . lsp))

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
