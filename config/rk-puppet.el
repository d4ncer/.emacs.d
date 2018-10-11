;;; rk-puppet.el --- Puppet config  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package puppet-mode
  :defer t
  :straight t)

(provide 'rk-puppet)

;;; rk-puppet.el ends here
