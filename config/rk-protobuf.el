;;; rk-protobuf.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package protobuf-mode
  :straight t
  :mode (("\\.proto\\'" . protobuf-mode)))

(provide 'rk-protobuf)

;;; rk-protobuf.el ends here
