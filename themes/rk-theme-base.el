;;; rk-theme-base.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

;; Cursor colours (ULTRA PASTEL)
(defconst rk-theme-cursor-yellow "#f1c40f")
(defconst rk-theme-cursor-blue "#3498db")
(defconst rk-theme-cursor-green "#2ecc71")
(defconst rk-theme-cursor-purple "#9b59b6")

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
(defconst rk-theme-base-offwhite-dark "#fbf1d4")

;; Greys
(defconst rk-theme-base-solarized-b03 "#002b36")
(defconst rk-theme-base-solarized-b02 "#073642")
(defconst rk-theme-base-solarized-b01 "#586e75")
(defconst rk-theme-base-solarized-b00 "#657b83")
(defconst rk-theme-base-solarized-b0 "#839496")
(defconst rk-theme-base-solarized-b1 "#93a1a1")
(defconst rk-theme-base-solarized-b2 "#eee8d5")
(defconst rk-theme-base-solarized-b3 "#fdf6e3")
(defconst rk-theme-base-dark-grey "#747369")
(defconst rk-theme-base-light-grey "#e8e6df")

(defconst rk-theme-font-family "Fira Code")

(defun rk-theme-base-make-theme (default-foreground default-background)
  `((default
      ((t
        :background ,default-background
        :foreground ,default-foreground
        :weight normal
        :family ,rk-theme-font-family
        :height 190)))

    (link
     ((t :weight light :underline ,rk-theme-base-light-grey)))

    (fringe
     ((t :background ,default-background)))

    (internal-border
     ((t :background ,rk-theme-base-violet)))

    ;; Mode & header line

    (mode-line
     ((t
       :height 140
       :background ,rk-theme-base-solarized-b2
       :foreground ,rk-theme-base-solarized-b02)))

    (mode-line-inactive
     ((t :inherit mode-line)))

    (mode-line-highlight
     ((t
       :background ,rk-theme-base-violet
       :foreground ,rk-theme-base-offwhite
       :box nil)))

    (doom-modeline-bar
     ((t :background ,rk-theme-base-violet)))

    (header-line
     ((t
       :height 140
       :weight bold
       :background ,rk-theme-base-violet
       :foreground ,rk-theme-base-offwhite
       :box (:line-width 8 :color ,rk-theme-base-violet))))

    (lv-separator
     ((t
       :background ,rk-theme-base-solarized-b2)))

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
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-macro-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-command-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-special-form-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-syntax-class-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-user-option-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

    (info-variable-ref-item
     ((t :weight demibold :foreground ,rk-theme-base-solarized-b1)))

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
     ((t :inherit diredp-file-name :foreground ,rk-theme-base-solarized-b1)))

    (diredp-file-suffix
     ((t :foreground ,rk-theme-base-solarized-b1)))

    (diredp-compressed-file-suffix
     ((t :inherit diredp-file-suffix)))

    (diredp-number
     ((t :weight light)))

    (diredp-date-time
     ((t :foreground ,rk-theme-base-solarized-b1 :weight light)))

    (diredp-dir-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    (diredp-no-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    (diredp-rare-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    (diredp-exec-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    (diredp-read-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    (diredp-write-priv
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

    ;; Info

    (info-double-quoted-name
     ((t :weight normal)))

    (makefile-space
     ((t :background ,rk-theme-base-violet)))

    ;; diff-hl

    (diff-hl-change
     ((t :foreground ,rk-theme-base-blue :background ,rk-theme-base-blue)))

    (diff-hl-delete
     ((t :foreground ,rk-theme-base-red :background ,rk-theme-base-red)))

    (diff-hl-insert
     ((t :foreground ,rk-theme-base-green :background ,rk-theme-base-green)))

    ;; web-mode

    (web-mode-function-call-face
     ((t :weight normal)))

    (web-mode-json-key-face
     ((t :weight normal)))

    ;; LSP

    (lsp-face-highlight-read
     ((t :background nil :weight demibold :foreground ,rk-theme-base-blue)))

    (lsp-face-highlight-write
     ((t :background nil :weight demibold :foreground ,rk-theme-base-magenta)))

    (lsp-face-highlight-textual
     ((t :background nil :weight demibold :foreground ,rk-theme-base-cyan)))

    (lsp-ui-peek-filename
     ((t :foreground ,rk-theme-base-violet)))

    (lsp-ui-peek-list
     ((t :background ,rk-theme-base-solarized-b2)))

    (lsp-ui-peek-peek
     ((t :background ,rk-theme-base-solarized-b2)))

    (lsp-ui-peek-highlight
     ((t :foreground ,rk-theme-base-blue)))

    (lsp-ui-peek-header
     ((t :foreground ,rk-theme-base-solarized-b2 :background ,rk-theme-base-solarized-b02)))

    (lsp-ui-peek-selection
     ((t :background ,rk-theme-base-solarized-b1 :foreground ,rk-theme-base-solarized-b03)))

    ;; Pairs

    (show-paren-match
     ((t
       :weight bold
       :foreground ,rk-theme-base-green
       :underline ,rk-theme-base-green)))

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

    (deadgrep-match-face
     ((t :inherit highlight :foreground ,rk-theme-base-blue)))

    (highlight-thing
     ((t :weight demibold :foreground ,rk-theme-base-blue)))

    (ahs-face
     ((t :inherit highlight)))

    (ahs-plugin-whole-buffer-face
     ((t :inherit highlight)))

    (hydra-posframe-face
     ((t :background ,rk-theme-base-solarized-b2)))

    (hydra-posframe-border-face
     ((t :background ,rk-theme-base-magenta)))

    ;; Magit

    (magit-diff-file-heading
     ((t :weight normal)))

    (magit-section-heading
     ((t :weight demibold)))

    (magit-popup-disabled-argument
     ((t :foreground ,rk-theme-base-solarized-b1)))

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

    (magit-signature-good
     ((t :foreground ,rk-theme-base-green)))

    ;; Outline, Org

    (org-link
     ((t :inherit link :weight normal)))

    (org-done
     ((t
       :inherit default
       :weight bold
       :foreground ,rk-theme-base-solarized-b1)))

    (org-document-info-keyword
     ((t :weight light)))

    (org-scheduled-today
     ((t :inherit default)))

    (org-date
     ((t :weight light :underline t)))

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
     ((t :weight light :foreground ,rk-theme-base-solarized-b1)))

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

    ;; Scala

    (scala-font-lock:var-keyword-face
     ((t :inherit error)))

    (scala-font-lock:var-face
     ((t :inherit font-lock-variable-name-face)))

    ;; Company

    (company-template-field
     ((t :inherit highlight)))

    (company-box-annotation
     ((t :foreground ,rk-theme-base-orange)))

    (company-box-background
     ((t :background ,rk-theme-base-solarized-b2)))

    (company-box-candidate
     ((t :foreground ,default-foreground)))

    (company-box-scrollbar
     ((t :foreground ,default-foreground :background ,rk-theme-base-violet)))

    (company-box-selection
     ((t :foreground ,rk-theme-base-offwhite :background ,rk-theme-base-violet)))

    ;; Misc faces

    (go-peg-mode-production-name
     ((t :weight demibold)))

    (sh-quoted-exec
     ((t :weight demibold)))

    (sh-heredoc
     ((t :inherit font-lock-string-face)))

    (hl-todo
     ((t :foreground nil :box t)))

    (parenthesis
     ((t :weight light)))

    (whitespace-line
     ((t :background ,rk-theme-base-offblack :foreground ,rk-theme-base-offwhite)))

    (whitespace-space
     ((t :background ,rk-theme-base-offwhite-dark :foreground ,rk-theme-base-offblack)))

    (whitespace-indentation
     ((t :background ,rk-theme-base-offwhite-dark :foreground ,rk-theme-base-offblack)))

    (whitespace-space-after-tab
     ((t :background ,rk-theme-base-offwhite-dark :foreground ,rk-theme-base-offblack)))

    (link
     ((t :inherit default :underline t)))))

(provide 'rk-theme-base)

;;; rk-theme-base.el ends here
