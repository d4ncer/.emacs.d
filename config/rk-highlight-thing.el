;;; rk-highlight-thing.el --- Configuration for highlight-thing.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package highlight-thing
  :straight t
  :init
  (add-hook 'prog-mode-hook #'highlight-thing-mode)
  :preface
  (require 'seq)
  (defun rk-highlight-thing--face-ancestors (face)
    (let (result)
      (while (and face (not (equal face 'unspecified)))
        (setq result (cons face result))
        (setq face (face-attribute face :inherit)))
      (nreverse result)))

  (defun rk-highlight-thing--should-highlight-p (res)
    "Do not highlight symbol if looking at certain faces."
    (when res
      (let ((excluded-faces '(font-lock-string-face
                              font-lock-keyword-face
                              font-lock-comment-face
                              font-lock-preprocessor-face
                              font-lock-builtin-face))
            (faces (seq-mapcat #'rk-highlight-thing--face-ancestors (face-at-point nil t))))
        (null (seq-intersection faces excluded-faces)))))

  :custom
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-delay-seconds 0.5)
  (highlight-thing-limit-to-defun nil)
  (highlight-thing-case-sensitive-p t)
  :config
  (advice-add 'highlight-thing-should-highlight-p :filter-return
              #'rk-highlight-thing--should-highlight-p))

(provide 'rk-highlight-thing)

;;; rk-highlight-thing.el ends here
