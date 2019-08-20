;;; rk-php.el --- Config for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'f)

(use-package php-mode
  :straight t
  :mode ("\\.php\\'" . php-mode)
  :general
  (:keymaps 'php-mode-map
            "(" nil
            "{" nil)
  :custom
  (php-template-compatibility nil)
  :preface
  (defun rk-php--setup ()
    (setq-default lsp-intelephense-storage-path (f-join paths-cache-directory "lsp-intelephense-cache"))
    (lsp))
  :hook
  (php-mode . rk-php--setup))

(provide 'rk-php)

;;; rk-php.el ends here
