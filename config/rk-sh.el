;;; rk-sh.el --- Config for shell script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'lsp)

(use-package sh-script
  :hook
  (sh-mode . lsp))

(provide 'rk-sh)

;;; rk-sh.el ends here
