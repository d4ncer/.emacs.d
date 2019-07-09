;;; rk-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 's)
(require 'noflet)
(require 'definers)

(require 'paths)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'evil-define-key "evil-core")

(defalias #'yes-or-no-p #'y-or-n-p)

(global-unset-key (kbd "C-z"))

;; Ambient titlebar

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq frame-title-format nil))

;; Disable lockfiles

(setq create-lockfiles nil)

;; Non-native fullscreen

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil))

;; Set up keybinds for Straight

(rk-leader-def
  "P"  '(:ignore t :wk "packages")
  "Pp" '(straight-pull-all :wk "pull all package")
  "PP" '(straight-pull-package :wk "pull package")
  "Pr" '(straight-rebuild-all :wk "rebuild all packages")
  "PR" '(straight-rebuild-package :wk "rebuild package")
  "Pf" '(straight-freeze-versions :wk "freeze packages"))

;; Make <escape> quit as much as possible

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

;; Write custom settings outside init.el

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Fix colour issues for Spaceline
;; TODO: Enable this if we go back to Spaceline

;; (setq ns-use-srgb-colorspace nil)

;; Scroll smoothly.

(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") #'comment-indent-new-line)

;; Disable backup files
(setq make-backup-files nil)

;; Evil breaks cursor settings in Hydra buffers
(setq-default cursor-in-non-selected-windows nil)

;; Enable hideshow in all programming buffers.

(autoload 'hs-minor-mode "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Make files executable on save.

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Copy system clipboard to the kill-ring if an Emacs kill would overwrite it.
(setq save-interprogram-paste-before-kill t)

;; Don't prompt when following symlinks to vc files.
(setq vc-follow-symlinks t)

;; This should really be a thing out-of-the-box.

(defun rk-indent-buffer ()
  "Indent the entire buffer."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun rk-indent-dwim (&optional justify)
  "Indent the thing at point.
Knows how to fill strings and comments, or indent code.
Optional arg JUSTIFY will justify comments and strings."
  (interactive "*P")
  (-let [(_ _ _ string-p comment-p) (syntax-ppss)]
    (cond
     (string-p
      (let ((progress (make-progress-reporter "Filling paragraph")))
        (fill-paragraph justify)
        (progress-reporter-done progress)))
     (comment-p
      (let ((progress (make-progress-reporter "Filling comment")))
        (fill-comment-paragraph justify)
        (progress-reporter-done progress)))
     ((region-active-p)
      (indent-region (region-beginning) (region-end)))
     (t
      (let ((progress (make-progress-reporter "Indenting buffer")))
        (rk-indent-buffer)
        (progress-reporter-done progress))))))

(define-key prog-mode-map (kbd "C-`") #'rk-indent-dwim)

;; Window move hotkeys

(rk-leader-def
  "wh" '(windmove-left :wk "move left")
  "wl" '(windmove-right :wk "move right")
  "wk" '(windmove-up :wk "move up")
  "wj" '(windmove-down :wk "move down"))

;; Unlimited print length for eval-expression.
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; Convenience aliases

(defalias 'bb  'bury-buffer)
(defalias 'hex 'hexl-mode)
(defalias 'hff 'hexl-find-file)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'cal 'calendar)


;;; Saving behaviour

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable commands.

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;; Core advice

;; Do not prompt for confirmation for active processes.

(defun rk-basic-settings--suppress-no-process-prompt (fn &rest args)
  (noflet ((process-list () nil))
    (apply fn args)))

(advice-add #'save-buffers-kill-emacs :around #'rk-basic-settings--suppress-no-process-prompt)

;; Insert a leading space after comment start for new comment lines.

(defun rk-basic-settings--comment-insert-space (&rest _)
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(advice-add #'comment-indent-new-line :after #'rk-basic-settings--comment-insert-space)

;; Clean up whitespace when inserting yanked text.

(defun rk-basic-settings--yank-ws-cleanup (&rest _)
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(advice-add #'insert-for-yank :after #'rk-basic-settings--yank-ws-cleanup)

;; Process ANSI color codes in shell output.

(defun rk-basic-settings--display-ansi-codes (buf &rest _)
  (and (bufferp buf)
       (string= (buffer-name buf) "*Shell Command Output*")
       (with-current-buffer buf
         (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add #'display-message-or-buffer :before #'rk-basic-settings--display-ansi-codes)

;; Process ANSI color codes in compilation buffers.

(defun rk-basic-settings--ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'rk-basic-settings--ansi-colorize-buffer)

;; Generate random passwords.

(defun rk-generate-password ()
  "Generate a random password and copy it to the kill ring."
  (interactive)
  (kill-new (s-trim (shell-command-to-string "gpg --gen-random --armor 1 30")))
  (message "Password copied to kill-ring."))

;; Line transposition

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun rk-transpose-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun rk-transpose-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(global-set-key (kbd "C-j") #'rk-transpose-line-down)
(global-set-key (kbd "C-k") #'rk-transpose-line-up)

;;; Hide DOS EOL

(defun rk-basic-settings--hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook #'rk-basic-settings--hide-dos-eol)


;; Offer to open large files in fundamental mode.

(defvar rk-basic-settings--large-file-size (* 1024 1024)
  "Size in bytes above which a file is considered 'large'.")

(defconst rk-basic-settings--large-file-modes-list
  '(archive-mode
    doc-view-mode
    doc-view-mode-maybe
    ebrowse-tree-mode
    git-commit-mode
    image-mode
    pdf-view-mode
    tar-mode)
  "A list of modes that should not prompt for literal file editing.")

(defun rk-basic-settings--large-file? (size)
  (unless (memq major-mode rk-basic-settings--large-file-modes-list)
    (and size (> size rk-basic-settings--large-file-size))))

(defun rk-basic-settings--prompt-to-open-large-files-in-fundamental-mode ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename)))
         (tags-file? (when filename (equal "TAGS" (file-name-nondirectory filename)))))
    (when (and (rk-basic-settings--large-file? size)
               (not tags-file?)
               (y-or-n-p (format "`%s' is a large file.  Open in fundamental mode? " (file-name-nondirectory filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook #'rk-basic-settings--prompt-to-open-large-files-in-fundamental-mode)

;;; General variables

(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space t)
(setq-default line-spacing 2)

(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-buffer-choice t)
(setq ring-bell-function #'ignore)

;; Use conf mode for puppet templated conf files

(add-to-list 'auto-mode-alist '("\\.env\\.erb\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\.erb\\'" . conf-mode))

(add-to-list 'auto-mode-alist '("\\.kll\\'" . conf-mode))

;;; TODO: Move this into it's own package
(use-package all-the-icons
  :straight t)

(use-package page-break-lines
  :straight t
  :init
  (with-eval-after-load 'eshell
    (add-to-list 'page-break-lines-modes 'eshell-mode))
  :config
  (global-page-break-lines-mode 1))

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat paths-cache-directory "/abbrev_defs")))

(use-package window-numbering
  :defer t
  :commands (window-numbering-mode)
  :config
  (window-numbering-mode -1))

(use-package menu-bar
  :bind (("C-c e e" . toggle-debug-on-error))
  :if (bound-and-true-p menu-bar-mode)
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :if (bound-and-true-p tool-bar-mode)
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :if (display-graphic-p)
  :config
  (scroll-bar-mode -1))

(use-package align
  :bind (("C-x a a" . align-regexp))

  :config
  (setq-default case-fold-search nil))

(use-package simple
  :bind (("M-SPC" . cycle-spacing)))

(use-package recentf
  :defer t
  :preface
  (progn

    (defun rk-basic-settings-boring-filename-p (f)
      (memq (f-filename f)
            '("TAGS" ".DS_Store")))

    (defun rk-basic-settings-boring-extension-p (f)
      (seq-intersection (f-ext f)
                        '("gz" "zip" "tar")))

    (defun rk-basic-settings-child-of-boring-relative-dir-p (f)
      (seq-intersection (f-split f)
                        '(".git"
                          ".ensime_cache"
                          ".cargo"
                          ".stack_work"
                          ".g8"
                          "target"
                          "build"
                          "Maildir"
                          "dist")))

    (defun rk-basic-settings-child-of-boring-abs-dir-p (f)
      (let ((ignore-case (eq system-type 'darwin)))
        (seq-find (lambda (d)
                    (s-starts-with? d f ignore-case))
                  (list "/var/folders/"
                        "/usr/local/Cellar/"
                        "/tmp/"
                        (f-expand (concat user-emacs-directory "snippets/")))))))

  :config
  (progn
    (setq recentf-max-saved-items 1000)
    (setq recentf-save-file (concat paths-cache-directory "/recentf"))
    (setq recentf-exclude
          '(rk-basic-settings-boring-filename-p
            rk-basic-settings-boring-extension-p
            rk-basic-settings-child-of-boring-relative-dir-p
            rk-basic-settings-child-of-boring-abs-dir-p))))

(use-package calendar
  :config
  (setq diary-file (f-join "~/org" "diary")))

(use-package bookmark
  :defer t
  :config
  (progn
    (setq bookmark-save-flag nil)
    (setq bookmark-default-file (concat paths-cache-directory "/bookmarks"))))

(use-package iedit
  :straight t
  :config
  (progn
    (setq iedit-toggle-key-default nil)))

(use-package files
  :config
  (progn
    (setq kept-new-versions 6)
    (setq require-final-newline t)
    (setq delete-old-versions t)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq backup-directory-alist `((".*" . ,(concat paths-cache-directory "/autosave"))))
    (setq version-control t)))

(use-package highlight
  :straight (:repo "git@framagit.org:steckerhalter/highlight.el.git" :branch "master"))

(use-package select
  :config
  (setq select-enable-clipboard t))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error))

(use-package mule
  :config
  (setq default-input-method "TeX"))

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t))

(use-package hippie-exp
  :init
  (progn
    (global-set-key (kbd "M-/") #'hippie-expand)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map [remap evil-complete-previous] #'hippie-expand)))

  :config
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(use-package winner
  :preface
  (defvar rk-basic-settings-winner-boring-buffers
    '("*Completions*"
      "*Compile-Log*"
      "*inferior-lisp*"
      "*Fuzzy Completions*"
      "*Apropos*"
      "*Help*"
      "*cvs*"
      "*Buffer List*"
      "*Ibuffer*"
      "*esh command on file*"))
  :config
  (progn
    (winner-mode t)
    (setq winner-boring-buffers (append winner-boring-buffers rk-basic-settings-winner-boring-buffers))))

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat paths-cache-directory "/saveplace"))
    (save-place-mode +1)))

(use-package savehist
  :init
  (defconst savehist-file (concat paths-cache-directory "/savehist"))
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (savehist-mode +1)))

(use-package tramp
  :defer t
  :preface
  (progn
    (setq tramp-default-method "ssh")
    (setq tramp-auto-save-directory "/tmp")))

(use-package tramp-cache
  :defer t
  :config
  (setq tramp-persistency-file-name (concat paths-cache-directory "/tramp")))

(use-package autorevert
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (global-auto-revert-mode 1)))

(use-package goto-addr
  :defer t
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

(use-package ffap
  :defer t
  :config
  ;; Don't try to ping things that look like domain names
  (setq ffap-machine-p-known 'reject))

(use-package help
  :defer t
  :config
  ;; Always focus on help windows
  (setq help-window-select t))

(use-package time
  :commands (display-time-mode)
  :init
  (add-hook 'after-init-hook #'display-time-mode)
  :config
  (setq display-time-default-load-average nil))

(use-package hydra
  :straight t
  :config
  (setq lv-use-separator t))

(use-package hydra-posframe
  :straight (:host github :repo "Ladicle/hydra-posframe" :branch "master")
  :after hydra
  :custom
  (hydra-posframe-border-width 2)
  :hook (after-init . hydra-posframe-enable))

(use-package sql-indent
  :straight (:host github :repo "alex-hhh/emacs-sql-indent"
                   :branch "master")
  :after sql
  :config
  (add-hook 'sql-mode-hook #'sqlind-minor-mode))

(use-package autoinsert
  :preface
  (defvar auto-insert-alist nil)
  :init
  (auto-insert-mode +1)
  :config
  (setq auto-insert-query nil))

(use-package autoinsert-funcs
  :after autoinsert
  :defines (autoinsert-funcs-forms)
  :config
  (dolist (form autoinsert-funcs-forms)
    (push form auto-insert-alist)))

(use-package calc)

(use-package man
  :defer t
  :bind (:map
         Man-mode-map
         ("M-n" . Man-next-section)
         ("M-p" . Man-previous-section)))

(use-package info+
  :straight t
  :defer 3
  :defines (Info-fontify-angle-bracketed-flag)
  :init
  (progn
    (with-eval-after-load 'info
      (require 'info+))
    (setq Info-fontify-angle-bracketed-flag nil)))

(use-package avy
  :straight t
  :general
  ("C-' c" #'avy-goto-char
   "C-' '" #'avy-goto-word-1
   "C-' w" #'avy-goto-word-1
   "C-' l" #'avy-goto-line))

(use-package vlf
  :straight t
  :init
  (require 'vlf-setup))

(use-package world-time-mode
  :straight t
  :defer t
  :init
  (rk-leader-def "a m w" '(world-time-list :wk "world time"))
  :config
  (progn
    (setq display-time-world-list '(("Pacific/Auckland" "NZT")
                                    ("Asia/Kolkata" "India")
                                    ("UTC" "UTC")
                                    ("Europe/Berlin" "Germany")
                                    ("America/Los_Angeles" "Los Angeles")
                                    ("America/New_York" "New York")))
    (with-eval-after-load 'evil
      (evil-define-key 'normal world-time-table-mode-map (kbd "q") #'quit-window))
    (add-hook 'world-time-table-mode-hook 'hl-line-mode)))

(use-package popwin
  :straight t
  :config
  (progn
    ;; Dirty test this regex
    ;; (let* ((trp (rx "*" (or "Cargo" "go" "Racer" "restclient" "lsp" "Ledger") (zero-or-more char) "*"))
    ;;        (trp-1? (if (s-matches-p trp "*lsp-ui-imenu*") "YES" "NO")))
    ;;   (message trp-1?))
    (push (list (rx "*" (or "Cargo" "go" "Racer" "restclient" "Ledger" "lsp" "nim") (zero-or-more char) "*") :noselect t :regexp t) popwin:special-display-config)
    (popwin-mode 1)))

(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

(use-package smex
  :straight t
  :commands (smex-initialize)
  :defines (smex-save-file)
  :config
  (progn
    (setq smex-save-file (concat paths-cache-directory "/smex-items"))
    (smex-initialize)))

(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode)
  :init
  (setq emojify-emojis-dir (concat paths-cache-directory "/emojis")))

(use-package json-mode
  :straight t
  :init
  (setq json-mode-auto-mode-list '(".babelrc" ".eslintrc" "composer.lock"))
  :preface
  (defun rk-json--disable-python-checker ()
    (with-eval-after-load 'flycheck
      (setq-local flycheck-disabled-checkers '(json-python-json))))
  (defun rk-json--format-region-or-buffer ()
    "Format region or buffer."
    (interactive)
    (if (use-region-p)
        (json-pretty-print (region-beginning) (region-end))
      (json-pretty-print (point-min) (point-max))))
  :init
  (rk-local-leader-def :keymaps 'json-mode-map
    "." '(rk-json--format-region-or-buffer :wk "format"))
  :config
  (progn
    (add-hook 'json-mode-hook #'rk-json--disable-python-checker)
    (with-eval-after-load 'js
      (setq js-indent-level 2))
    (with-eval-after-load 'json-reformat
      (setq json-reformat:indent-width 2))))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun rk-csv--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'rk-csv--suppress-final-newline))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(provide 'rk-basic-settings)

;;; rk-basic-settings.el ends here
