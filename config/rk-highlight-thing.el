;;; rk-highlight-thing.el --- Configuration for highlight-thing.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package highlight-thing
  :straight t
  :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-delay-seconds 0.1)
  (highlight-thing-limit-to-defun nil)
  (highlight-thing-case-sensitive-p t)
  :config
  (set-face-attribute 'highlight-thing nil :inherit 'highlight))

(use-package highlight-thing
  :straight t
  :preface
  (defun face-ancestors (face)
    "List all faces that FACE transitively inherits from."
    (let (result)
      (while (and face (not (equal face 'unspecified)))
        (setq result (cons face result))
        (setq face (face-attribute face :inherit)))
      (nreverse result)))
  (defun rk-highlight-thing--should-highlight-p (res)
    (unless (or (bound-and-true-p lsp-ui-mode)
                (bound-and-true-p tide-mode))
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))

  :config
  (advice-add 'highlight-thing-should-highlight-p :filter-return
              #'rk-highlight-thing--should-highlight-p))

(provide 'rk-highlight-thing)

;;; rk-highlight-thing.el ends here
