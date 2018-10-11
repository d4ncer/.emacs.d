;;; rk-markdown.el --- Configuration for Markdown files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :general
  (:keymaps 'markdown-mode-map :states '(normal motion insert visual)
            "s-b" #'markdown-insert-bold
            "s-i" #'markdown-insert-italic
            "s-s" #'markdown-insert-strike-through
            "M-j" #'markdown-move-list-item-down
            "M-k" #'markdown-move-list-item-up
            "M-h" #'markdown-promote-list-item
            "M-l" #'markdown-demote-list-item)
  (:keymaps 'gfm-mode-map :states '(normal motion insert visual)
            "s-b" #'markdown-insert-bold
            "s-i" #'markdown-insert-italic
            "s-s" #'markdown-insert-strike-through
            "M-j" #'markdown-move-list-item-down
            "M-k" #'markdown-move-list-item-up
            "M-h" #'markdown-promote-list-item
            "M-l" #'markdown-demote-list-item)
  :init
  (progn
    (rk-local-leader-def :keymaps 'markdown-mode-map
      "f" '(markdown-fill-paragraph :wk "wrap")

      "b" '(markdown-insert-bold :wk "bold")
      "t" '(markdown-insert-italic :wk "italic")
      "s" '(markdown-insert-strike-through :wk "strike through")

      "i"  '(:ignore t :wk "insert")
      "il" '(markdown-insert-link :wk "link")
      "ii" '(markdown-insert-image :wk "image")
      "ic" '(markdown-insert-gfm-code-block :wk "code block")
      "i-" '(markdown-insert-hr)

      "o"  '(:ignore t :wk "open")
      "oo" '(markdown-open :wk "open with Marked")
      "of" '(markdown-follow-thing-at-point "follow thing"))

    (rk-local-leader-def :keymaps 'gfm-mode-map
      "f" '(markdown-fill-paragraph :wk "wrap")

      "b" '(markdown-insert-bold :wk "bold")
      "t" '(markdown-insert-italic :wk "italic")
      "s" '(markdown-insert-strike-through :wk "strike through")

      "i"  '(:ignore t :wk "insert")
      "il" '(markdown-insert-link :wk "link")
      "ii" '(markdown-insert-image :wk "image")
      "ic" '(markdown-insert-gfm-code-block :wk "code block")
      "i-" '(markdown-insert-hr)

      "o"  '(:ignore t :wk "open")
      "oo" '(markdown-open :wk "open with Marked")
      "of" '(markdown-follow-thing-at-point "follow thing"))

    (setq markdown-open-command "/usr/local/bin/mark")
    (setq markdown-command "multimarkdown")))

(provide 'rk-markdown)

;;; rk-markdown.el ends here
