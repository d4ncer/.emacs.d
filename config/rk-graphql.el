;;; rk-graphql.el --- GraphQL config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package graphql-mode
  :straight t)

(use-package graphql
  :straight t)

(use-package prettier
  :straight t
  :after graphql
  :init
  (add-hook 'graphql-mode-hook #'prettier-mode))

(provide 'rk-graphql)

;;; rk-graphql.el ends here
