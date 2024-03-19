;;; rk-fmt.el --- All things formatting  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(provide 'rk-fmt)

;;; rk-fmt.el ends here
