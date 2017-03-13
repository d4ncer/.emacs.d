;;; rk-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst rk-theme-common-yellow "#b58900")
(defconst rk-theme-common-orange "#cb4b16")
(defconst rk-theme-common-red "red1")
(defconst rk-theme-common-pink "pink")
(defconst rk-theme-common-magenta "#d33682")
(defconst rk-theme-common-violet "#6c71c4")
(defconst rk-theme-common-blue "#268bd2")
(defconst rk-theme-common-cyan "#2aa198")
(defconst rk-theme-common-green "#859900")

(defconst rk-theme-common-neutral-grey "grey60")
(defconst rk-theme-common-dark-grey "grey40")
(defconst rk-theme-common-light-grey "grey80")

(defun rk-theme-common-make-theme (default-foreground default-background)
  `((default
      ((t
        :background ,default-background
        :foreground ,default-foreground
        :weight normal
        :family "Droid Sans"
        :height 130)))

    (mode-line
     ((t :foreground "gray40" :background "gray40" :height 20)))

    (link
     ((((background light))
       :weight light :underline ,rk-theme-common-light-grey)
      (((background dark))
       :weight light :underline ,rk-theme-common-dark-grey)))

    (fringe
     ((t :background ,default-background)))

    (header-line
     ((t :background ,rk-theme-common-blue :foreground "white" :weight bold)))

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
       :foreground ,rk-theme-common-pink :weight demibold)))

    (org-agenda-date-weekend
     ((t :inherit org-agenda-date)))

    (org-warning
     ((((background light))
       :foreground ,rk-theme-common-red :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-pink :weight normal)))

    (org-upcoming-deadline
     ((((background light))
       :foreground ,rk-theme-common-yellow :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-yellow :weight normal)))

    (org-scheduled-previously
     ((((background light))
       :foreground ,rk-theme-common-red :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-pink :weight normal)))

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

    ;; Ledger

    (ledger-font-comment-face
     ((t :weight light)))

    (ledger-font-posting-date-face
     ((t :inherit default)))

    (ledger-font-posting-account-face
     ((t :inherit default)))

    (ledger-font-xact-highlight-face
     ((t nil)))

    (ledger-font-other-face
     ((t :inherit default :weight demibold)))

    (ledger-font-directive-face
     ((t :inherit default)))

    (ledger-font-posting-amount-face
     ((t :inherit default)))

    ;; Mu4e

    (mu4e-title-face
     ((t :weight demibold)))

    (mu4e-header-key-face
     ((t :weight demibold)))

    (mu4e-highlight-face
     ((t :foreground ,rk-theme-common-blue :weight demibold)))

    (mu4e-header-highlight-face
     ((t :inherit region)))

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

    ;; Term

    (term-color-black
     ((t :foreground ,default-foreground)))

    (term-color-blue
     ((t :foreground ,rk-theme-common-blue)))

    (term-color-bold
     ((t :weight demibold)))

    (term-color-cyan
     ((t :foreground ,rk-theme-common-cyan)))

    (term-color-green
     ((t :foreground ,rk-theme-common-green)))

    (term-color-magenta
     ((t :foreground ,rk-theme-common-magenta)))

    (term-color-red
     ((t :foreground ,rk-theme-common-red)))

    (term-color-underline
     ((t :underline t)))

    (term-color-white
     ((t :foreground ,default-foreground)))

    (term-color-yellow
     ((t :foreground ,rk-theme-common-yellow)))

    ;; Scala

    (scala-font-lock:var-keyword-face
     ((((background light))
       :foreground ,rk-theme-common-red :weight normal)
      (((background dark))
       :foreground ,rk-theme-common-pink :weight normal)))

    (scala-font-lock:var-face
     ((t :inherit font-lock-variable-name-face)))

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
       :foreground ,rk-theme-common-pink :weight bold)))

    (parenthesis
     ((t :weight light)))

    (link
     ((t :inherit default :underline t)))))

(provide 'rk-theme-common)

;;; rk-theme-common.el ends here
