;;; rk-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defconst rk-theme-common-red "#ef5253")
(defconst rk-theme-common-orange "#e66b2b")
(defconst rk-theme-common-yellow "#e4b51c")
(defconst rk-theme-common-green "#7cc844")
(defconst rk-theme-common-cyan "#52cbb0")
(defconst rk-theme-common-blue "#33b5e1")
(defconst rk-theme-common-violet "#a363d5")
(defconst rk-theme-common-magenta "#d73c9a")

(defconst rk-theme-common-neutral-grey "#a09f93")
(defconst rk-theme-common-dark-grey "#747369")
(defconst rk-theme-common-light-grey "#e8e6df")

(defun rk-theme-common-make-theme (default-foreground default-background)
  `((default
      ((t
        :background ,default-background
        :foreground ,default-foreground
        :weight normal
        :family "Roboto Mono"
        :height 130)))

    (mode-line
     ((t :foreground ,rk-theme-common-neutral-grey :background ,rk-theme-common-neutral-grey :height 20)))

    (link
     ((((background light))
       :weight light :underline ,rk-theme-common-light-grey)
      (((background dark))
       :weight light :underline ,rk-theme-common-dark-grey)))

    (fringe
     ((t :background ,default-background)))

    (header-line
     ((t :background ,rk-theme-common-violet :foreground "white" :weight bold)))

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
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-macro-ref-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-command-ref-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-special-form-ref-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-syntax-class-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-user-option-ref-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    (info-variable-ref-item
     ((t :weight demibold :foreground ,rk-theme-common-neutral-grey)))

    ;; Dired

    (dired-header
     ((t :weight bold)))

    (diredp-dir-heading
     ((t :inherit dired-header)))

    (diredp-dir-name
     ((t :inherit default :foreground ,rk-theme-common-blue)))

    (diredp-file-name
     ((t :inherit default)))

    (diredp-ignored-file-name
     ((t :inherit diredp-file-name :foreground ,rk-theme-common-neutral-grey)))

    (diredp-file-suffix
     ((t :foreground ,rk-theme-common-neutral-grey)))

    (diredp-compressed-file-suffix
     ((t :inherit diredp-file-suffix)))

    (diredp-number
     ((t :weight light)))

    (diredp-date-time
     ((t :foreground ,rk-theme-common-neutral-grey :weight light)))

    (diredp-dir-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    (diredp-no-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    (diredp-rare-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    (diredp-exec-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    (diredp-read-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    (diredp-write-priv
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

    ;; Info

    (info-double-quoted-name
     ((t :weight normal)))

    ;; web-mode

    (web-mode-function-call-face
     ((t :weight normal)))

    (web-mode-json-key-face
     ((t :weight normal)))

    ;; Pairs

    (show-paren-match
     ((t
       :weight bold
       :foreground ,rk-theme-common-cyan
       :underline ,rk-theme-common-cyan)))

    (show-paren-mismatch
     ((t
       :weight bold
       :foreground ,rk-theme-common-red
       :underline ,rk-theme-common-red)))

    ;; Highlights

    (highlight
     ((((background light)) :background "#e0e0e0")
      (((background dark))  :background "#303030")))

    (iedit-occurrence
     ((((background light))
       :weight normal
       :background "#FFe0e0"
       :foreground ,default-foreground)
      (((background dark))
       :weight normal
       :background "#703030"
       :foreground ,default-foreground)))

    (evil-search-highlight-persist-highlight-face
     ((t :inherit highlight :background nil)))

    (highlight-thing
     ((t :weight demibold :foreground ,rk-theme-common-blue)))

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
     ((t :foreground ,rk-theme-common-neutral-grey)))

    (magit-popup-option-value
     ((t :weight normal)))

    ;; Outline, Org

    (org-link
     ((t :inherit link :weight normal)))

    (org-done
     ((t
       :inherit default
       :weight bold
       :foreground ,rk-theme-common-neutral-grey)))

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
     ((((background light))
       :foreground ,rk-theme-common-red :weight demibold)
      (((background dark))
       :foreground ,rk-theme-common-magenta :weight demibold)))

    (org-agenda-date-weekend
     ((t :inherit org-agenda-date)))

    (org-warning
     ((((background light))
       :foreground ,rk-theme-common-red :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-magenta :weight normal)))

    (org-upcoming-deadline
     ((((background light))
       :foreground ,rk-theme-common-yellow :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-yellow :weight normal)))

    (org-scheduled-previously
     ((((background light))
       :foreground ,rk-theme-common-red :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-magenta :weight normal)))

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
     ((t :weight light :foreground ,rk-theme-common-neutral-grey)))

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
     ((((background light))
       :foreground ,rk-theme-common-red :weight bold)
      (((background dark))
       :foreground ,rk-theme-common-magenta :weight bold)))

    (parenthesis
     ((t :weight light)))

    (link
     ((t :inherit default :underline t)))))

(provide 'rk-theme-common)

;;; rk-theme-common.el ends here
