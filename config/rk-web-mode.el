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
  :after eglot
  :hook (json-ts-mode . eglot-ensure))

(use-package js
  :mode ("\\.cjs\\'" . js-ts-mode)
  :hook
  (js-ts-mode . rk-er/add-treesit-expander))

(use-package js
  :after eglot
  :hook (js-ts-mode . eglot-ensure))

(use-package css-mode
  :hook
  (css-ts-mode . rk-er/add-treesit-expander))

(use-package css-mode
  :after eglot
  :hook (css-ts-mode . eglot-ensure))

(use-package html-ts-mode
  :hook
  (html-ts-mode . rk-er/add-treesit-expander))

(use-package html-ts-mode
  :after eglot
  :hook (html-ts-mode . eglot-ensure))

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
