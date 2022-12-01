;;; rk-nim.el --- Config for Nim  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'lsp)

(use-package nim-mode
  :straight t
  :mode ("\\.nim\\'" . nim-mode)
  :preface
  ;; TODO: Unused at the moment, will pick it back up later
  (defun rk-nim--init-lsp ()
    (add-to-list 'lsp-language-id-configuration '(nim-mode . "nim"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
                      :major-modes '(nim-mode)
                      :server-id 'nimlsp))
    (lsp))

  (defun rk-nim--setup ()
    (general-def :keymaps 'nim-mode-map :states '(normal visual)
      "K" #'nimsuggest-show-doc
      ">" #'nim-indent-shift-right
      "<" #'nim-indent-shift-left)

    (when (string-match "/\.nimble/" buffer-file-name) (read-only-mode 1))

    (nimsuggest-mode 1)
    (auto-fill-mode 0)
    (electric-indent-local-mode 0))

  :config
  (add-hook 'nim-mode-hook #'rk-nim--setup))

(provide 'rk-nim)

;;; rk-nim.el ends here
