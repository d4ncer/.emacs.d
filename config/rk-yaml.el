;;; rk-yaml.el --- Configuration for YAML mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(provide 'rk-yaml)

;;; rk-yaml.el ends here
