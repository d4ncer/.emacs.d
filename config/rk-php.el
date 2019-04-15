;;; rk-php.el --- Config for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar rk-php--lsp-ip-cmd `("node" ,(executable-find "intelephense") "--stdio"))

(use-package php-mode
  :straight t
  :mode ("\\.php\\'" . php-mode)
  :general
  (:keymaps 'php-mode-map "(" nil)
  :custom
  (php-template-compatibility nil)
  :preface
  (defun rk-php--setup-intelephense ()
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection rk-php--lsp-ip-cmd)
                      :major-modes '(php-mode)
                      :priority 0
                      :server-id 'php-ip
                      :ignore-messages '("indexing\\(Started\\|Ended\\)"))))
  (defun rk-php--setup ()
    (rk-php--setup-intelephense)
    (lsp))
  :hook
  (php-mode . rk-php--setup))

(provide 'rk-php)

;;; rk-php.el ends here
