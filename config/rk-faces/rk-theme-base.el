;;; rk-theme-base.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

;; Solarized light for text
(defconst rk-theme-base-yellow "#b58900")
(defconst rk-theme-base-orange "#cb4b16")
(defconst rk-theme-base-red "#dc322f")
(defconst rk-theme-base-magenta "#d33682")
(defconst rk-theme-base-violet "#6c71c4")
(defconst rk-theme-base-blue "#268bd2")
(defconst rk-theme-base-cyan "#2aa198")
(defconst rk-theme-base-green "#859900")

;; Offwhites for highlights
(defconst rk-theme-base-light-red "#ffe0e0")

;; Default fg/bg
(defconst rk-theme-base-offblack "#333")
(defconst rk-theme-base-offwhite "#fdfdfd")

;; Greys
(defconst rk-theme-base-solarized-b03 "#002b36")
(defconst rk-theme-base-solarized-b02 "#073642")
(defconst rk-theme-base-solarized-b01 "#586e75")
(defconst rk-theme-base-solarized-b00 "#657b83")
(defconst rk-theme-base-solarized-b0 "#839496")
(defconst rk-theme-base-solarized-b1 "#93a1a1")
(defconst rk-theme-base-solarized-b2 "#eee8d5")
(defconst rk-theme-base-solarized-b3 "#fdf6e3")
(defconst rk-theme-base-solarized-b01 "#a09f93")
(defconst rk-theme-base-dark-grey "#747369")
(defconst rk-theme-base-light-grey "#e8e6df")

(defun rk-theme-base-make-theme (default-foreground default-background)
  `((default
      ((t
        :background ,default-background
        :foreground ,default-foreground
        :weight normal
        :family "Roboto Mono"
        :height 140)))

    (mode-line
     ((t :foreground ,rk-theme-base-solarized-b01 :background ,rk-theme-base-solarized-b01 :height 20)))

    (link
     ((t :weight light :underline ,rk-theme-base-light-grey)))

    (fringe
     ((t :background ,default-background)))

    (header-line
     ((t :background ,rk-theme-base-violet :foreground ,rk-theme-base-solarized-b3 :weight bold)))

    (rk-header-line-format-nonemphased-element
     ((t :weight light)))

    ;; General font-lock faces.

    (font-lock-keyword-face
     ((t :weight light)))

    (font-lock-builtin-face
     ((t :weight light)))

    (font-lock-variable-name-face
     ((t :weight normal)))

    (font-lock-function-name-face
     ((t :weight demibold)))

    (font-lock-constant-face
     ((t :weight normal)))

    (font-lock-type-face
     ((t :weight normal)))

    (font-lock-type-name-face
     ((t :weight normal)))

    (font-lock-string-face
     ((t :weight light)))

    (font-lock-comment-face
     ((t :weight demibold)))

    ;; Info faces

    (info-xref
     ((t :inherit link :weight normal)))

    (info-xref-visited
     ((t :inherit link-visited :weight normal)))

    (info-string
     ((t :inherit font-lock-string-face)))

    (info-node
     ((t :weight demibold)))

    (info-reference-item
     ((t :weight demibold)))

    (info-function-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-macro-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-command-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-special-form-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-syntax-class-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-user-option-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    (info-variable-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b01)))

    ;; Dired

    (dired-header
     ((t :weight bold)))

    (diredp-dir-heading
     ((t :inherit dired-header)))

    (diredp-dir-name
     ((t :inherit default :foreground ,rk-theme-base-blue)))

    (diredp-file-name
     ((t :inherit default)))

    (diredp-ignored-file-name
     ((t :inherit diredp-file-name :foreground ,rk-theme-base-solarized-b01)))

    (diredp-file-suffix
     ((t :foreground ,rk-theme-base-solarized-b01)))

    (diredp-compressed-file-suffix
     ((t :inherit diredp-file-suffix)))

    (diredp-number
     ((t :weight light)))

    (diredp-date-time
     ((t :foreground ,rk-theme-base-solarized-b01 :weight light)))

    (diredp-dir-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (diredp-no-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (diredp-rare-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (diredp-exec-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (diredp-read-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (diredp-write-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    ;; Info

    (info-double-quoted-name
     ((t :weight normal)))

    ;; Info

    (makefile-space
     ((t :background ,rk-theme-base-violet)))

    ;; web-mode

    (web-mode-function-call-face
     ((t :weight normal)))

    (web-mode-json-key-face
     ((t :weight normal)))

    ;; Pairs

    (show-paren-match
     ((t
       :weight bold
       :foreground ,rk-theme-base-cyan
       :underline ,rk-theme-base-cyan)))

    (show-paren-mismatch
     ((t
       :weight bold
       :foreground ,rk-theme-base-red
       :underline ,rk-theme-base-red)))

    ;; Highlights

    (region
     ((t :background ,rk-theme-base-solarized-b2)))

    (highlight
     ((t :background ,rk-theme-base-solarized-b2)))

    (iedit-occurrence
     ((t :weight normal :background ,rk-theme-base-light-red :foreground ,default-foreground)))

    (evil-search-highlight-persist-highlight-face
     ((t :inherit highlight :background nil)))

    (highlight-thing
     ((t :weight demibold :foreground ,rk-theme-base-blue)))

    (ahs-face
     ((t :inherit highlight)))

    (ahs-plugin-whole-buffer-face
     ((t :inherit highlight)))

    ;; Magit

    (magit-diff-file-heading
     ((t :weight normal)))

    (magit-section-heading
     ((t :weight demibold)))

    (magit-popup-disabled-argument
     ((t :foreground ,rk-theme-base-solarized-b01)))

    (magit-popup-option-value
     ((t :weight normal)))

    (magit-branch-local
     ((t :foreground ,rk-theme-base-blue :weight demibold)))

    (magit-branch-remote
     ((t :foreground ,rk-theme-base-green :weight demibold)))

    (magit-branch-current
     ((t :foreground ,rk-theme-base-blue :box 1 :weight demibold)))

    (magit-tag
     ((t :foreground ,rk-theme-base-yellow :weight demibold)))

    (magit-process-ng
     ((t :foreground ,rk-theme-base-red)))

    (magit-process-ok
     ((t :foreground ,rk-theme-base-green)))

    (magit-section-highlight
     ((t :background ,rk-theme-base-solarized-b2)))

    (magit-diff-hunk-heading-highlight
     ((t :background ,rk-theme-base-solarized-b1)))

    (magit-diff-context-highlight
     ((t :background ,rk-theme-base-solarized-b2)))

    ;; Outline, Org

    (org-link
     ((t :inherit link :weight normal)))

    (org-done
     ((t
       :inherit default
       :weight bold
       :foreground ,rk-theme-base-solarized-b01)))

    (org-document-info-keyword
     ((t :weight light)))

    (org-scheduled-today
     ((t :inherit default)))

    (org-date
     ((t :weight light)))

    (org-sexp-date
     ((t :weight light)))

    (org-date
     ((t :underline t)))

    (org-agenda-date-today
     ((t :foreground ,rk-theme-base-red :weight demibold)))

    (org-agenda-date-weekend
     ((t :inherit org-agenda-date)))

    (org-warning
     ((t :foreground ,rk-theme-base-red :weight normal)))

    (org-upcoming-deadline
     ((t :foreground ,rk-theme-base-yellow :weight normal)))

    (org-scheduled-previously
     ((t :foreground ,rk-theme-base-red :weight normal)))

    (org-formula
     ((t :weight light)))

    (org-tag
     ((t :weight light)))

    (org-table
     ((t :inherit default)))

    (org-scheduled
     ((t :inherit default)))

    (org-meta-line
     ((t :weight light)))

    (org-agenda-structure
     ((t :weight demibold)))

    (org-document-title
     ((t :weight bold)))

    (outline-1
     ((t :weight demibold)))
    (outline-2
     ((t :inherit outline-1)))
    (outline-3
     ((t :inherit outline-1)))
    (outline-4
     ((t :inherit outline-1)))
    (outline-5
     ((t :inherit outline-1)))
    (outline-6
     ((t :inherit outline-1)))
    (outline-7
     ((t :inherit outline-1)))
    (outline-8
     ((t :inherit outline-1)))
    (outline-9
     ((t :inherit outline-1)))

    ;; Message composition

    (message-header-name
     ((t :weight demibold)))

    (message-header-to
     ((t :weight normal)))

    (message-cited-text
     ((t :weight light :foreground ,rk-theme-base-solarized-b01)))

    (message-header-subject
     ((t :weight normal)))

    (message-header-other
     ((t :weight demibold)))

    ;; Neotree

    (neo-file-link-face
     ((t :weight light)))

    (neo-dir-link-face
     ((t :weight normal)))

    ;; Cargo & Rust

    (cargo-process--ok-face
     ((t :inherit success)))

    (cargo-process--warning-face
     ((t :inherit warning)))

    (cargo-process--error-face
     ((t :inherit error)))

    (cargo-process--standard-face
     ((t :weight demibold)))

    (cargo-process--standard-face
     ((t :weight demibold)))

    (rk-rust-faces-macro
     ((t :weight normal)))

    (rk-rust-faces-bool
     ((t :weight normal)))

    ;; Company

    (company-template-field
     ((t :inherit highlight)))

    ;; Misc faces

    (go-peg-mode-production-name
     ((t :weight demibold)))

    (sh-quoted-exec
     ((t :weight demibold)))

    (sh-heredoc
     ((t :inherit font-lock-string-face)))

    (hl-todo
     ((t :foreground ,rk-theme-base-red :weight bold)))

    (parenthesis
     ((t :weight light)))

    (link
     ((t :inherit default :underline t)))))

(provide 'rk-theme-base)

;;; rk-theme-base.el ends here
