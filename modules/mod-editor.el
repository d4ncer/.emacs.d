;;; mod-editor.el --- Editor utilities and enhancements -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains editor utility packages including:
;; - File management (recentf, saveplace, autorevert)
;; - Structural editing (puni, electric-pair)
;; - Window/buffer history (winner)
;; - Whitespace management (ws-butler)
;; - Spell checking (ispell, spell-fu, flyspell-correct)
;; - Diff and merge (diff, ediff)
;; - Other utilities (helpful, align, rotate, newcomment)
;; - Code highlighting (highlight-thing)

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; Auto-revert

(use-package autorevert
  ;; Automatically revert buffers.
  ;;
  ;; This configuration is adapted from Doom; it disables the file watcher
  ;; mechanism and instead auto-reverts based on users switching windows &
  ;; buffers. This is much less resource-intensive.
  :config
  (defun +auto-revert-current-buffer-h ()
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +auto-revert-visible-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (+visible-buffers))
      (with-current-buffer buf
        (+auto-revert-current-buffer-h))))
  :hook
  (after-save-hook . +auto-revert-visible-buffers-h)
  (+switch-buffer-hook . +auto-revert-current-buffer-h)
  (+switch-window-hook . +auto-revert-current-buffer-h)
  :config
  (add-function :after after-focus-change-function #'+auto-revert-visible-buffers-h)

  :custom
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

;;; Structural editing

(use-package elec-pair
  ;; Automatically insert matching pairs.
  :after-call +first-file-hook +first-buffer-hook
  :init
  (electric-pair-mode +1))

(use-package lisp
  ;; Despite its name, provides many programming-language generic features.
  :general
  (:keymaps 'prog-mode-map :states 'normal "(" 'backward-sexp ")" 'forward-sexp))

(use-package puni :ensure t
  ;; Provides structured editing commands.
  :after-call +first-file-hook +first-buffer-hook
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:keymaps 'puni-mode-map :states 'insert "C-w" #'puni-backward-kill-word)
  (:keymaps 'puni-mode-map :states '(visual) "M-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "M-k" #'+kill-line
            "M-(" #'puni-wrap-round
            "M-[" #'puni-wrap-square
            "M-S-{" #'puni-wrap-curly))

;;; File management

(use-package recentf
  ;; Maintain a list of visited files.
  :after-call recentf consult-buffer
  :defer-incrementally t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package saveplace
  ;; Save buffer position when re-visiting files, even across Emacs sessions.
  :init (save-place-mode +1)

  :config
  (define-advice save-place-find-file-hook (:after-while (&rest _) recenter)
    "Recenter on cursor when loading a saved place."
    (when buffer-file-name (ignore-errors (recenter))))

  (define-advice save-place-alist-to-file (:around (fn &rest args) use-prin1-not-pp)
    "Use the faster prin1 for saving history."
    (cl-letf (((symbol-function #'pp) #'prin1))
      (apply fn args))))

;;; Window and buffer history

(use-package winner
  ;; Provides undo/redo for buffer & window layout changes.
  :general-config (:keymaps 'winner-mode-map
                            "M-<" #'winner-undo
                            "M->" #'winner-redo)
  :after-call +first-file-hook +first-buffer-hook
  :init
  (winner-mode +1)
  :custom
  (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                           "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                           "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  :config
  (with-eval-after-load 'evil
    (keymap-set evil-normal-state-map "C-." #'winner-redo)))

;;; Whitespace management

(use-package ws-butler :ensure t
  ;; Delete trailing whitespace on visited lines.
  :hook (prog-mode-hook text-mode-hook conf-mode-hook)
  :config
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))

;;; Diff and merge

(use-package diff
  ;; Support for Unix diff files.
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

(use-package ediff
  ;; Interactive file diff & merge UI.
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-show-clashes-only t)
  :config
  (with-eval-after-load 'org
    (defun +ad-ediff-reveal-org-content-around-hunk (&rest _)
      (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-reveal t))))))

    (advice-add 'ediff-next-difference :after #'+ad-ediff-reveal-org-content-around-hunk)
    (advice-add 'ediff-previous-difference :after #'+ad-ediff-reveal-org-content-around-hunk)))

;;; Other utilities

(use-package tabify
  ;; Tab-to-space conversion
  :custom
  (tabify-regexp "^\t* [ \t]+"))

(use-package comint
  ;; Emacs' basic system for hosting interactive command interpreters.
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048) ; double the default.
  )

(use-package compile
  ;; Integration for running compilers and other processes from inside Emacs.
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil) ; automatically save before compiling.
  (compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; Automatically truncate long compilation buffers.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package helpful :ensure t
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-data-mode-map) :states '(normal)
            "K" #'helpful-at-point))

(use-package align
  ;; Emacs has an extensible mode-specific alignment system. In this age of code
  ;; formatters it's not terribly useful, but I do use `align-regexp' from
  ;; time-to-time.
  :general ("C-x a a" #'align-regexp))

(use-package rotate :ensure t
  ;; Provides a few commands for arranging windows in pre-configured
  ;; layouts--very handy.
  ;;
  ;; TODO: Define my own version that ignores side-windows when re-arranging.
  :commands (rotate-layout)
  :config
  (setq rotate-functions '(rotate:even-horizontal rotate:even-vertical)))

(use-package newcomment
  ;; Provides comment-related commands and variables to customise their
  ;; behaviour.
  :custom
  (comment-empty-lines t)
  (comment-multi-line t)
  (comment-style 'extra-line)
  :config
  (setq-default comment-column 0)
  (with-eval-after-load 'evil

    ;; Teach "J" (evil-join) to delete comment delimiters as needed to join
    ;; lines.

    ;; Taken from doom, which itself adapts solutions in this github issue:
    ;; https://github.com/emacs-evil/evil/issues/606

    (define-advice evil-join (:around (fn beg end) join-comments)
      (if-let* (((not (= (line-end-position) (point-max))))
                (cend (save-excursion (goto-char end) (line-end-position)))
                (cbeg (save-excursion
                        (goto-char beg)
                        (and (+point-in-comment-p
                              (save-excursion
                                (goto-char (line-beginning-position 2))
                                (skip-syntax-forward " \t")
                                (point)))
                             (or (comment-search-backward (line-beginning-position) t)
                                 (comment-search-forward  (line-end-position) t)
                                 (and (+point-in-comment-p beg)
                                      (stringp comment-continue)
                                      (or (search-forward comment-continue (line-end-position) t)
                                          beg)))))))
          (let* ((count (count-lines beg end))
                 (count (if (> count 1) (1- count) count))
                 (fixup-mark (make-marker)))
            (uncomment-region (line-beginning-position 2)
                              (save-excursion
                                (goto-char cend)
                                (line-end-position 0)))
            (unwind-protect
                (dotimes (_ count)
                  (join-line 1)
                  (save-match-data
                    (when (or (and comment-continue
                                   (not (string-empty-p comment-continue))
                                   (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                              (and comment-start-skip
                                   (not (string-empty-p comment-start-skip))
                                   (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                      (replace-match "" t nil nil 1)
                      (just-one-space))))
              (set-marker fixup-mark nil)))
        ;; But revert to the default we're not in a comment, where
        ;; `fill-region-as-paragraph' is too greedy.
        (funcall fn beg end)))))

;;; Spell checking

(use-package ispell
  ;; Built-in spellchecker. I don't actually use it directly, but other packages
  ;; reference its configuration.
  :custom
  (ispell-dictionary "en_AU")
  (ispell-personal-dictionary (file-name-concat org-directory "aspell.en.pws"))
  :config
  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))
  (ispell-set-spellchecker-params))

(use-package spell-fu :ensure t
  ;; A more lightweight spell-checker than the built-in.
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:states '(normal motion)
           "zn" #'spell-fu-goto-next-error
           "zp" #'spell-fu-goto-previous-error
           "zg" #'spell-fu-word-add
           "zx" #'spell-fu-word-remove)

  :config
  (add-hook! 'spell-fu-mode-hook
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr")))

  (setq-hook! 'org-mode-hook
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote org-tag org-modern-tag org-verbatim)))

(use-package flyspell-correct :ensure t
  ;; Provides a nicer command for working with spelling corrections.
  :after spell-fu
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))

;;; Code highlighting

(use-package highlight-thing :ensure t
  :hook (prog-mode-hook . highlight-thing-mode)
  :custom
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-delay-seconds 0.2)
  (highlight-thing-limit-to-defun nil)
  (highlight-thing-case-sensitive-p t)
  :config
  (defun +face-ancestors (face)
    "List all faces that FACE transitively inherits from."
    (let (result)
      (while (and face (not (equal face 'unspecified)))
        (setq result (cons face result))
        (setq face (face-attribute face :inherit)))
      (nreverse result)))

  (defun +should-highlight-p (res)
    (unless (or (bound-and-true-p lsp-ui-mode)
                (bound-and-true-p tide-mode))
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'+face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))
  (advice-add 'highlight-thing-should-highlight-p :filter-return
              #'+should-highlight-p))

(provide 'mod-editor)
;;; mod-editor.el ends here
