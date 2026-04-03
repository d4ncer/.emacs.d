;;; mod-ui.el --- UI, themes, fonts, and visual enhancements -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains all UI-related configuration including:
;; - Theme system (modus themes via +theme.el)
;; - Font configuration (JetBrainsMono, Helvetica Neue)
;; - Frame parameters and visual settings
;; - Visual enhancements (ligatures, modeline, pulsar, hl-todo)
;; - Custom faces

;;; Code:

(eval-and-compile
  (defvar +lisp-dir (file-name-concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path +lisp-dir)
  (require '+corelib))

;;; Theme

(use-package batppuccin-themes
  :ensure (:host github :repo "bbatsov/batppuccin-emacs")
  :demand t
  :config
  (+theme-update))

;;; Custom faces

(custom-theme-set-faces 'user
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

;;; Frame parameters

(use-package frame
  ;; Frame management settings
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-right-width 24)
  :init
  (window-divider-mode +1))

;;; Ligatures

(use-package ligature :ensure t
  ;; Teach Emacs how to display ligatures when available.
  :after-call +first-buffer-hook +first-file-hook
  :config

  (defun +read-ligatures (file)
    (with-temp-buffer
      (insert-file-contents-literally (file-name-concat +ligatures-dir file))
      (read (current-buffer))))

  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-ligatures "prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-ligatures "text-mode.eld"))

  (global-ligature-mode t))

;;; Visual highlighting

;; hl-todo disabled due to performance issues with tree-sitter modes
;; (use-package hl-todo :ensure t
;;   :disabled t)

(use-package pulsar :ensure t
  ;; Temporarily highlights the current line after performing certain operations
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  (pulsar-resolve-pulse-function-aliases nil)
  :config
  (pulsar-resolve-function-aliases)
  (require '+pulsar)

  (add-hook 'next-error-hook #'pulsar-pulse-line)

  (dolist (hook '(consult-after-jump-hook
                  imenu-after-jump-hook))
    (add-hook hook #'pulsar-recenter-top)
    (add-hook hook #'pulsar-reveal-entry))

  (dolist (hook '(org-agenda-after-show-hook
                  org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry))

  (define-advice flymake-goto-next-error (:after (&rest _) pulsar)
    (when pulsar-mode
      (pcase (cl-loop for o in (overlays-at (point))
                      for diag = (overlay-get o 'flymake-diagnostic)
                      when diag
                      return (flymake--severity (flymake-diagnostic-type diag)))
        (3 (pulsar-pulse-line-red))
        (2 (pulsar-pulse-line-yellow))
        (_ (pulsar-pulse-line-cyan)))))

  (delq! 'evil-goto-first-line pulsar-pulse-functions)
  (delq! 'evil-goto-line pulsar-pulse-functions)

  (define-advice evil-goto-line (:after (count) pulsar)
    "Don't pulse if moving to the first or last line via gg/G."
    (when (and pulsar-mode
               count ; nil if going to end of buffer
               (< 1 count ))
      (pulsar-pulse-line)))

  (define-advice evil-yank (:after (start end &rest _) pulsar)
    "Pulse yanked lines & regions."
    (when pulsar-mode
      (pulsar--create-pulse (cons start end) 'pulsar-generic)))

  (define-advice evil-jump-item (:after (&rest _) pulsar)
    "Pulse if jumping to a different line."
    (unless (region-active-p)
      (pulsar-pulse-line)))

  ;; Show a pulse indicating success or failure of eval-expression, eval-region,
  ;; etc.
  :config
  (define-advice eval-region (:around (fn start end &rest args) pulsar)
    "Pulse evaluated regions."
    (+with-eval-pulse start end
      (apply fn start end args)))

  (define-advice eval-last-sexp (:around (fn &rest args) pulsar)
    "Pulse evaluated expressions."
    (pcase-let ((`(,start . ,end) (or (bounds-of-thing-at-point 'sexp)
                                      (cons (ignore-errors (save-excursion
                                                             (backward-sexp)
                                                             (point)))
                                            (point)))))
      (+with-eval-pulse start end
        (apply fn args)))))

(use-package hl-line
  ;; Highlight the current line.
  :custom
  (hl-line-sticky-flag nil))

(use-package whitespace
  ;; Visualise whitespace characters.
  :config
  (delq! 'newline whitespace-style)
  (delq! 'newline-mark whitespace-style))

;;; Modeline

(use-package doom-modeline :ensure t
  :demand t
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-persp-name nil)
  (doom-modeline-workspace-name nil)
  (doom-modeline-modal nil)
  (doom-modeline-env-version nil)
  (doom-modeline-height 28)
  (doom-modeline-bar-width 0)
  (doom-modeline-hud nil)
  :config
  (doom-modeline-mode 1)
  (setq-default header-line-format nil)

  ;; Custom org segment: show title instead of filename
  (doom-modeline-def-segment +org-buffer-name
    "Show org title, narrowed heading, or <UNTITLED>.org."
    (when (derived-mode-p 'org-mode)
      (concat
       (doom-modeline-spc)
       (propertize
        (cond ((buffer-narrowed-p)
               (format "%s [%s]" (or (buffer-base-buffer) (buffer-name))
                       (org-link-display-format
                        (substring-no-properties
                         (or (org-get-heading 'no-tags) "-")))))
              (t (or (org-get-title) "<UNTITLED>.org")))
        'face (doom-modeline-face 'doom-modeline-buffer-file)))))

  (doom-modeline-def-modeline '+org
    '(bar modals +org-buffer-name buffer-position)
    '(check vcs major-mode misc-info))

  (add-to-list 'doom-modeline-mode-alist '(org-mode . +org))

  )

(provide 'mod-ui)
;;; mod-ui.el ends here
