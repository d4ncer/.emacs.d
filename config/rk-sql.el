;;; rk-sql.el --- SQL mode config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package sql
  :straight t
  :after lsp-mode
  :custom
  (lsp-sqls-workspace-config-path nil)
  :config
  (add-hook 'sql-mode-hook 'lsp-deferred))

(provide 'rk-sql)

;;; rk-sql.el ends here
