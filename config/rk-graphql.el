;;; rk-graphql.el --- GraphQL config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package graphql-mode
  :straight t)

(use-package graphql
  :straight t)

(provide 'rk-graphql)

;;; rk-graphql.el ends here
