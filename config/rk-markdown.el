;;; rk-markdown.el --- Configuration for Markdown files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (autoload 'evil-define-key "evil-core")
  (require 'use-package))

(require 'spacemacs-keys)

(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode
             markdown-fill-paragraph
             markdown-insert-footnote
             markdown-insert-link
             markdown-insert-image
             markdown-insert-gfm-code-block
             markdown-insert-hr
             markdown-follow-thing-at-point)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn

    (evil-define-key 'normal markdown-mode-map (kbd "M-j") #'markdown-move-list-item-down)
    (evil-define-key 'normal markdown-mode-map (kbd "M-k") #'markdown-move-list-item-up)
    (evil-define-key 'normal markdown-mode-map (kbd "M-h") #'markdown-promote-list-item)
    (evil-define-key 'normal markdown-mode-map (kbd "M-l") #'markdown-demote-list-item)
    (evil-define-key 'normal markdown-mode-map (kbd "M-J") #'markdown-move-subtree-down)
    (evil-define-key 'normal markdown-mode-map (kbd "M-K") #'markdown-move-subtree-up)
    (evil-define-key 'normal markdown-mode-map (kbd "M-H") #'markdown-promote-subtree)
    (evil-define-key 'normal markdown-mode-map (kbd "M-L") #'markdown-demote-subtree)

    (spacemacs-keys-declare-prefix-for-mode 'markdown-mode "m i" "insert")
    (spacemacs-keys-declare-prefix-for-mode 'markdown-mode "m h" "header")
    (spacemacs-keys-declare-prefix-for-mode 'markdown-mode "m o" "open")
    (spacemacs-keys-set-leader-keys-for-major-mode 'markdown-mode
      "." #'markdown-complete
      "f" #'markdown-fill-paragraph
      "if" #'markdown-insert-footnote
      "il" #'markdown-insert-link
      "ii" #'markdown-insert-image
      "ic" #'markdown-insert-gfm-code-block
      "i-" #'markdown-insert-hr

      "b" #'markdown-insert-bold
      "t" #'markdown-insert-italic
      "s" #'markdown-insert-strike-through

      "1" #'markdown-insert-header-atx-1
      "2" #'markdown-insert-header-atx-2
      "3" #'markdown-insert-header-atx-3
      "4" #'markdown-insert-header-atx-4
      "5" #'markdown-insert-header-atx-5
      "!" #'markdown-insert-header-setext-1
      "@" #'markdown-insert-header-setext-2

      "hh" #'markdown-insert-header-dwim
      "hH" #'markdown-insert-header-setext-dwim
      "hp" #'markdown-promote
      "hd" #'markdown-demote

      "oo" #'markdown-open
      "of" #'markdown-follow-thing-at-point))
  :init
  (progn
    (setq markdown-open-command "/usr/local/bin/mark")
    (setq markdown-command "multimarkdown")))

(provide 'rk-markdown)

;;; rk-markdown.el ends here
