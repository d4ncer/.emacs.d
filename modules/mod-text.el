;;; mod-text.el --- Text modes and templates -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains text mode configurations and file template systems:
;; - text-mode (general text editing)
;; - tempel (snippet system)
;; - autoinsert (file template insertion)
;; - +file-templates (custom template system)
;; - pdf-tools (PDF viewer)

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; Text & programming modes

(use-package text-mode
  ;; Emacs' general parent mode for non-programming-language text files.
  :mode  ("/LICENSE\\'")

  ;; Not sure of the performance impact of this... leave off for now.
  ;; :hook (text-mode-hook . visual-line-mode)
  )

(use-package tempel :ensure t
  ;; Text snippets.
  ;;
  :general
  (:keymaps 'tempel-map :states 'normal
            "<escape>" #'tempel-done)
  :custom
  (tempel-path (file-name-concat +templates-dir "*.eld"))
  :init
  (add-hook! '(prog-mode-hook text-mode-hook config-mode-hook)
    (add-hook 'completion-at-point-functions #'tempel-expand -90 t)))

(use-package autoinsert
  :after-call +first-buffer-hook +first-file-hook
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "file-templates/"))
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode +1))

(use-package +file-templates
  :after autoinsert
  :demand t
  :config
  (+define-file-template (rx ".el" eos) "emacs-lisp.eld"))

(use-package pdf-tools :ensure t
  ;; A better PDF viewer for Emacs
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (defun +unset-evil-cursor ()
    (setq-local evil-normal-state-cursor (list nil)))
  :hook (pdf-view-mode-hook . +unset-evil-cursor)
  :config
  (pdf-tools-install :no-query)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  :general
  (:keymaps 'pdf-view-mode-map :states '(normal motion)
            "j" #'pdf-view-next-line-or-next-page
            "k" #'pdf-view-previous-line-or-previous-page
            "J" #'pdf-view-next-page
            "K" #'pdf-view-previous-page
            "gg" #'pdf-view-first-page
            "G" #'pdf-view-last-page
            "=" #'pdf-view-enlarge
            "-" #'pdf-view-shrink
            "0" #'pdf-view-scale-reset
            "/" #'isearch-forward)
  :config
  (with-eval-after-load 'nano-modeline
    (add-hook 'pdf-view-mode-hook #'+nano-modeline-default)))

(provide 'mod-text)
;;; mod-text.el ends here
