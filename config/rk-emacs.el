;;; rk-emacs.el --- Normalize emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :custom
  ;; Write custom settings outside init.el
  (custom-file (concat user-emacs-directory "custom.el"))

  ;; Disable lockfiles
  (create-lockfiles nil)

  ;; Scroll smoothly.
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 101)

  ;; Instantly display current keystrokes in mini buffer
  (echo-keystrokes 0.02)

  ;; Disable backup files
  (make-backup-files nil)

  ;; Evil breaks cursor settings in Hydra buffers
  (cursor-in-non-selected-windows nil)

  ;; Copy system clipboard to the kill-ring if an Emacs kill would overwrite it.
  (save-interprogram-paste-before-kill t)

  ;; Don't prompt when following symlinks to vc files.
  (vc-follow-symlinks t)

  ;; Unlimited print length for eval-expression.
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)

  ;; Do not prompt for confirmation for active processes.
  (confirm-kill-processes nil)

  ;; Additional customizations
  (fill-column 80)
  (tab-width 4)
  (indent-tabs-mode nil)
  (sentence-end-double-space t)
  (line-spacing 2)
  (sentence-end-double-space nil)
  (delete-by-moving-to-trash nil)
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)
  (initial-buffer-choice t)
  (ring-bell-function #'ignore)

  (abbrev-file-name (concat paths-cache-directory "/abbrev_defs"))
  (case-fold-search nil)

  ;; Recentf
  (recentf-max-saved-items 1000)
  (recentf-save-file (concat paths-cache-directory "/recentf"))
  (recentf-exclude '(rk-emacs-boring-filename-p
                     rk-emacs-boring-extension-p
                     rk-emacs-child-of-boring-relative-dir-p
                     rk-emacs-child-of-boring-abs-dir-p))

  ;; Diary
  (diary-file (f-join paths--life-dir "diary"))

  ;; Bookmarks
  (bookmark-save-flag nil)
  (bookmark-default-file (concat paths-cache-directory "/bookmarks"))

  ;; iedit
  (iedit-toggle-key-default nil)

  ;; Files
  (kept-new-versions 6)
  (require-final-newline t)
  (delete-old-versions t)
  (confirm-nonexistent-file-or-buffer nil)
  (backup-directory-alist `((".*" . ,(concat paths-cache-directory "/autosave"))))
  (version-control t)

  ;; Select
  (select-enable-clipboard t)

  ;; Compilation
  (compilation-scroll-output 'first-error)

  ;; Mule
  (default-input-method "TeX")

  ;; Comint
  (comint-prompt-read-only t)

  ;; Savehist
  (savehist-file (concat paths-cache-directory "/savehist"))
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

  ;; Save place
  (save-place-file (concat paths-cache-directory "/saveplace"))

  ;; Auto revert
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)

  ;; FFAP
  (ffap-machine-p-known 'reject)

  ;; Help
  (help-window-select t)

  ;; Time
  (display-time-default-load-average nil)

  ;; Project
  (project-list-file (concat paths-cache-directory "/projects.cache"))

  ;; Auto-indent on RET
  ;; Unbind M-o on global map
  :general
  ("C-c e e" #'toggle-debug-on-error
   "C-c I" #'insert-char
   "M-o" nil)
  (:states 'insert
           "RET" #'comment-indent-new-line)
  (:keymaps 'xref--xref-buffer-mode-map :states 'normal
            "n" #'xref-next-line
            "p" #'xref-prev-line
            "N" #'xref-prev-line
            "q" #'quit-window
            "RET" #'xref-quit-and-goto-xref)
  :preface
  (defun rk-emacs--comment-insert-space (&rest _)
    (when (and comment-start
               (thing-at-point-looking-at (regexp-quote comment-start)))
      (unless (or (thing-at-point-looking-at (rx (+ space))))
        (just-one-space))))

  (defun rk-emacs--yank-ws-cleanup (&rest _)
    (whitespace-cleanup)
    (delete-trailing-whitespace))

  (defun rk-emacs--display-ansi-codes (buf &rest _)
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max)))))

  (defun rk-emacs--ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun rk-emacs--hide-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  (defvar rk-emacs--large-file-size (* 1024 1024)
    "Size in bytes above which a file is considered 'large'.")

  (defconst rk-emacs--large-file-modes-list
    '(archive-mode
      doc-view-mode
      doc-view-mode-maybe
      ebrowse-tree-mode
      git-commit-mode
      image-mode
      pdf-view-mode
      tar-mode)
    "A list of modes that should not prompt for literal file editing.")

  (defun rk-emacs--large-file? (size)
    (unless (memq major-mode rk-emacs--large-file-modes-list)
      (and size (> size rk-emacs--large-file-size))))

  (defun rk-emacs--prompt-to-open-large-files-in-fundamental-mode ()
    (let* ((filename (buffer-file-name))
           (size (nth 7 (file-attributes filename)))
           (tags-file? (when filename (equal "TAGS" (file-name-nondirectory filename)))))
      (when (and (rk-emacs--large-file? size)
                 (not tags-file?)
                 (y-or-n-p (format "`%s' is a large file.  Open in fundamental mode? " (file-name-nondirectory filename))))
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode))))

  (defun rk-emacs-boring-filename-p (f)
    (memq (f-filename f)
          '("TAGS" ".DS_Store")))

  (defun rk-emacs-boring-extension-p (f)
    (seq-intersection (f-ext f)
                      '("gz" "zip" "tar")))

  (defun rk-emacs-child-of-boring-relative-dir-p (f)
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

  (defun rk-emacs-child-of-boring-abs-dir-p (f)
    (let ((ignore-case (eq system-type 'darwin)))
      (seq-find (lambda (d)
                  (s-starts-with? d f ignore-case))
                (list "/var/folders/"
                      (f-join (s-trim (shell-command-to-string "brew --prefix")) "Cellar")
                      "/tmp/"
                      (f-expand (concat user-emacs-directory "snippets/"))))))

  (defvar rk-emacs--winner-boring-buffers
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

  :init
  (rk-leader-def
    ;; Set up keybinds for Straight
    "P"  '(:ignore t :wk "packages")
    "Pp" '(straight-pull-all :wk "pull all package")
    "PP" '(straight-pull-package :wk "pull package")
    "Pr" '(straight-rebuild-all :wk "rebuild all packages")
    "PR" '(straight-rebuild-package :wk "rebuild package")
    "Pf" '(straight-freeze-versions :wk "freeze packages")

    ;; Window move hotkeys
    "wh" '(windmove-left :wk "move left")
    "wl" '(windmove-right :wk "move right")
    "wk" '(windmove-up :wk "move up")
    "wj" '(windmove-down :wk "move down"))

  :config
  ;; Turn off visual noise
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Enabled recentf
  (recentf-mode +1)

  ;; Winner
  (winner-mode +1)
  (dolist (buffer rk-emacs--winner-boring-buffers)
    (add-to-list 'winner-boring-buffers buffer))

  ;; Savehist
  (savehist-mode +1)

  ;; Save place
  (save-place-mode +1)

  ;; Auto revert
  (global-auto-revert-mode 1)

  ;; Delete selection
  (delete-selection-mode +1)

  ;; Use conf mode for puppet templated conf files
  (add-to-list 'auto-mode-alist '("\\.env\\.erb\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.conf\\.erb\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.kll\\'" . conf-mode))

  ;; Convenience aliases
  (defalias #'yes-or-no-p #'y-or-n-p)
  (defalias 'bb  'bury-buffer)
  (defalias 'hex 'hexl-mode)
  (defalias 'hff 'hexl-find-file)
  (defalias 'qr  'query-replace)
  (defalias 'qrr 'query-replace-regexp)
  (defalias 'cal 'calendar)

  ;; Enable hideshow in all programming buffers.
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  ;; Make files executable on save.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Saving behaviour
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Core advice
  ;; Insert a leading space after comment start for new comment lines.
  (advice-add #'comment-indent-new-line :after #'rk-emacs--comment-insert-space)

  ;; Clean up whitespace when inserting yanked text.
  (advice-add #'insert-for-yank :after #'rk-emacs--yank-ws-cleanup)

  ;; Process ANSI color codes in shell output.
  (advice-add #'display-message-or-buffer :before #'rk-emacs--display-ansi-codes)

  ;; Process ANSI color codes in compilation buffers.
  (add-hook 'compilation-filter-hook #'rk-emacs--ansi-colorize-buffer)

  ;; Hide DOS EOL
  (add-hook 'after-change-major-mode-hook #'rk-emacs--hide-dos-eol)

  ;; Offer to open large files in fundamental mode.
  (add-hook 'find-file-hook #'rk-emacs--prompt-to-open-large-files-in-fundamental-mode)

  ;; Goto-addr
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)

  ;; Display time
  (add-hook 'after-init-hook #'display-time-mode)

  (when (file-exists-p custom-file)
    (load-file custom-file)))

(use-package emacs
  :after org
  :general
  ("C-j" #'rk-transpose-line-down
   "C-k" #'rk-transpose-line-up)
  :preface
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
      (indent-according-to-mode))))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "/tmp")
  (tramp-persistency-file-name (concat paths-cache-directory "/tramp")))

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                 ("file" "Link to document file." ":")))
  (bibtex-align-at-equal-sign t))

(use-package bibtex
  :after flyspell
  :hook (bibtex-mode . flyspell-mode))

(provide 'rk-emacs)

;;; rk-emacs.el ends here
