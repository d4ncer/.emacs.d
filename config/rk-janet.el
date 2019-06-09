;;; rk-janet.el --- Config for Janet lang  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package janet-mode
  :straight
  (:host github :repo "ALSchwalm/janet-mode" :branch "master"))

(provide 'rk-janet)

;;; rk-janet.el ends here
