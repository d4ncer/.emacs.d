;;; rk-graphql.el --- GraphQL config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package graphql-mode
  :straight t)

;; (use-package graphql-mode
;;   :straight t
;;   :after lsp-mode
;;   :config
;;   (require 'lsp-graphql)
;;   (lsp-dependency 'graphql-language-service-cli
;;                   `(:system ,(executable-find "graphql-lsp")))
;;   (add-hook 'graphql-mode-hook #'lsp-deferred))

(use-package graphql
  :straight t)

(use-package prettier
  :straight t
  :after graphql
  :init
  (add-hook 'graphql-mode-hook #'prettier-mode))

(provide 'rk-graphql)

;;; rk-graphql.el ends here
