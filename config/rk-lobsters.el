;;; rk-lobsters.el --- Configuration for lobste.rs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package ivy-lobsters
  :straight t
  :commands
  (ivy-lobsters)

  :init
  (rk-leader-def
    "a m l" '(ivy-lobsters :wk "lobsters")))

(provide 'rk-lobsters)

;;; rk-lobsters.el ends here
