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

;;; Custom faces

(custom-theme-set-faces 'user
                        '(region ((t (:foreground unspecified :background unspecified :inherit modus-themes-search-lazy))))
                        '(iedit-occurrence ((t (:inherit modus-themes-search-replace))))
                        ;; Set a light modeline
                        '(mode-line ((t (:height 10 :background "#bbb" :box nil))))
                        '(mode-line-inactive ((t (:height 10 :background "#ddd" :box nil))))
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

(use-package hl-todo :ensure t
  ;; Display TODO comments with special highlights.
  :hook (prog-mode-hook yaml-ts-mode-hook conf-mode-hook)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("KLUDGE" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold))))

(use-package pulsar :ensure t
  ;; Temporarily highlights the current line after performing certain operations
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  :config
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
      (pulsar--pulse nil 'pulsar-generic start end)))

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

(use-package nano-modeline :ensure (:host github :repo "rougier/nano-modeline" :branch "rewrite")
  :demand t
  :init
  ;; Improve org-mode mode line
  ;; TODO this isn't wired up (yet)
  (defun +modeline-org-buffer-name (&optional name)
    (propertize
     (cond (name
            name)
           ((buffer-narrowed-p)
            (format "%s [%s]" (or (buffer-base-buffer) (buffer-name))
                    (org-link-display-format
                     (substring-no-properties
                      (or (org-get-heading 'no-tags) "-")))))
           ((file-equal-p (file-name-directory (buffer-file-name))
                          org-directory)
            (format "(org) %s" (car (last (s-split "-" (buffer-file-name))))))
           (t
            (buffer-name)))
     'face (nano-modeline-face 'name)))

  (defcustom nano-modeline-format-org
    (cons
     '(nano-modeline-element-buffer-status
       nano-modeline-element-space
       +modeline-org-buffer-name
       nano-modeline-element-space
       nano-modeline-element-buffer-vc-mode)
     '(nano-modeline-element-buffer-position
       nano-modeline-element-window-status
       nano-modeline-element-space))
    "Custom format for org + org-node buffers."
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

  (defun +nano-modeline-org ()
    (nano-modeline nano-modeline-format-org))

  ;; Add flymake stats to modeline
  (require 'flymake)

  (defun +flymake-count-type (type)
    (let ((count 0))
      (dolist (d (flymake-diagnostics))
        (when (= (flymake--severity type)
                 (flymake--severity (flymake-diagnostic-type d)))
          (cl-incf count)))
      count))

  (defun +modeline-element-flymake-statistics ()
    "Display Flymake diagnostics statistics or loading status."
    (let* ((running (flymake-running-backends))
           (reporting (flymake-reporting-backends))
           (waiting (cl-set-difference running reporting))
           (face 'nano-modeline-face-default))
      (if waiting
          ;; Show loading indicator when backends are running but not yet reported
          (propertize "⟳" 'face 'nano-modeline-face-default)
        ;; Show diagnostics statistics
        (let ((error-count (+flymake-count-type :error))
              (warning-count (+flymake-count-type :warning))
              (note-count (+flymake-count-type :note)))

          ;; Change face if there are errors
          (when (> error-count 0)
            (setq face 'nano-modeline-face-buffer-marked))

          ;; Format the statistics
          (propertize
           (if (or (> error-count 0) (> warning-count 0) (> note-count 0))
               (format " (%d/%d/%d) " error-count warning-count note-count)
             "✔")
           'face face)))))

  (defcustom nano-modeline-format-prog
    (cons '(nano-modeline-element-buffer-status
            nano-modeline-element-space
            nano-modeline-element-buffer-name
            nano-modeline-element-space
            nano-modeline-element-buffer-mode
            nano-modeline-element-space
            nano-modeline-element-buffer-vc-mode)
          '(+modeline-element-flymake-statistics
            nano-modeline-element-half-space
            nano-modeline-element-buffer-position
            nano-modeline-element-window-status
            nano-modeline-element-space))
    "Format for programming modes with flymake statistics"
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

  (defun +nano-modeline-prog ()
    (nano-modeline nano-modeline-format-prog))

  (defun +nano-modeline-default ()
    (nano-modeline nano-modeline-format-default))

  (defun +nano-modeline-gptel ()
    (nano-modeline nano-modeline-format-gptel))

  ;; Turn off borders in nano modeline buttons

  (defun +nano-modeline-remove-border-in-buttons (orig-fun &rest args)
    "Advice to temporarily set nano-modeline-border-color to nil during function execution."
    (let ((nano-modeline-border-color nil))
      (apply orig-fun args)))

  ;; Install the advice
  (advice-add 'nano-modeline--button :around #'+nano-modeline-remove-border-in-buttons)

  :config
  (add-hook! '(text-mode-hook magit-mode-hook help-mode-hook helpful-mode-hook) #'+nano-modeline-default)
  (add-hook! 'gptel-mode-hook #'+nano-modeline-gptel)
  (add-hook 'prog-mode-hook #'+nano-modeline-prog)
  (setq-default mode-line-format ""))

(provide 'mod-ui)
;;; mod-ui.el ends here
