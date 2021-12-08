;;; rk-expand-region.el --- Configuration for expand-region.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package expand-region
  :straight t
  :init
  (rk-leader-def "v" '(er/expand-region :wk "expand"))
  :custom
  (expand-region-contract-fast-key "V")
  (expand-region-reset-fast-key "r"))

(provide 'rk-expand-region)

;;; rk-expand-region.el ends here
