;;; rk-bazel.el --- Config for the Bazel build tool  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package bazel
  :straight t)

(provide 'rk-bazel)

;;; rk-bazel.el ends here
