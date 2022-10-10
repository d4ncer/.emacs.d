;;; rk-yaml.el --- Configuration for YAML mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :straight t
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(use-package prettier
  :straight t
  :after yaml-mode
  :if (executable-find "prettier")
  :init
  (add-hook 'yaml-mode-hook #'prettier-mode))

(provide 'rk-yaml)

;;; rk-yaml.el ends here
