;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package typescript-mode
  :straight t
  :defines (typescript-indent-level)
  :config
  (setq typescript-indent-level 2))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
