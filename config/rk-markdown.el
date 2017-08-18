;;; rk-markdown.el --- Configuration for Markdown files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode
             markdown-fill-paragraph)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (spacemacs-keys-set-leader-keys-for-major-mode 'markdown-mode
    "f" #'markdown-fill-paragraph)
  :init (setq markdown-command "multimarkdown"))

(provide 'rk-markdown)

;;; rk-markdown.el ends here
