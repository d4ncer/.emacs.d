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
;; - excali (Excalidraw editor and diagram source files)

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
  ;; NB. Don't use `:states' here. Tempel installs `tempel-map' via
  ;; `minor-mode-overriding-map-alist', which evil doesn't scan for auxiliary
  ;; keymaps, so state-scoped bindings are never consulted. Bind plainly
  ;; instead; evil leaves TAB alone in insert state, so these still win.
  :general
  (:keymaps 'tempel-map
            "TAB" #'tempel-next
            "<tab>" #'tempel-next
            "<backtab>" #'tempel-previous
            "S-TAB" #'tempel-previous)
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
            "/" #'isearch-forward))

;;; Excalidraw

(defun +excali-build (e)
  "Build the native rendering module for Elpaca package E."
  (let* ((default-directory (elpaca<-source-dir e))
         (emacs (expand-file-name invocation-name invocation-directory))
         (binary-dir (file-name-directory (file-truename emacs)))
         (include
          (seq-find
           (lambda (dir)
             (file-exists-p (expand-file-name "emacs-module.h" dir)))
           (list (expand-file-name "../include/" binary-dir)
                 (expand-file-name "../../../include/" binary-dir)
                 binary-dir
                 "/opt/homebrew/include/" "/usr/local/include/"))))
    (elpaca--make-process e
      :name "excali-module"
      :command (append (list "make" "module" (concat "EMACS=" emacs))
                       (when include
                         (list (concat "EMACS_MODULE_INCLUDE=" include))))
      :sentinel
      (lambda (process event)
        (elpaca--process-sentinel "Excali module built" process event)))))

(when (and (>= emacs-major-version 32)
           module-file-suffix
           (fboundp 'canvas-refresh)
           (image-type-available-p 'canvas))
  (use-package excali
    :ensure (:host github :repo "yibie/excali-mode"
             :files (:defaults "excali-module.*" "fonts")
             :build (:before elpaca-build-link +excali-build))
    :commands (excali-open excali-new)
    :mode ("\\.excalidsl\\'" . excali-dsl-mode)
    :init
    (autoload 'excali-dsl-mode "excali-dsl" nil t)
    (evil-set-initial-state 'excali-mode 'emacs)
    :general
    (:states '(normal motion insert)
             :prefix "SPC" :non-normal-prefix "C-SPC"
             "ax" '(nil :wk "Excalidraw")
             "axn" '(excali-new :wk "new drawing")
             "axo" '(excali-open :wk "open drawing"))))

(provide 'mod-text)
;;; mod-text.el ends here
