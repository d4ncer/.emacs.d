;;; mod-core-extras.el --- Additional core utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains additional core utilities and system packages:
;; - File management (files, tramp, uniquify)
;; - Performance (bidi, scrolling, profiler)
;; - System integration (server, envrc, exec-path-from-shell, mise)
;; - Utilities (goto-addr, better-jumper, string-inflection, so-long)
;; - Search tools (grep, wgrep, xref)
;; - Other built-ins (paragraphs, indent, replace, proced)

;;; Code:

(use-package paragraphs
  ;; Emacs' core paragraph parser.
  :custom
  (sentence-end-double-space nil))

(defvar +auto-save-dir (file-name-concat user-emacs-directory "autosave/"))

(use-package files
  ;; General built-in file IO.
  :custom
  (backup-inhibited t)
  (require-final-newline t)
  (find-file-visit-truename t)
  (make-backup-files nil)
  (confirm-nonexistent-file-or-buffer nil)
  (auto-mode-case-fold nil)
  (version-control nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (insert-directory-program "gls")
  (backup-directory-alist `(("." . ,+auto-save-dir)))
  (auto-save-list-file-prefix (file-name-concat +auto-save-dir ".saves-"))
  (auto-save-file-name-transforms `((".*" ,+auto-save-dir t)))

  ;; Used by `find-sibling-file' to figure out what files are related.
  (find-sibling-rules
   `(
     ;; Tests -> impl in TS
     ,(list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
            (rx (backref 1) ".ts"))

     ;; Impl -> tests in TS
     ,(list (rx (group (+? any)) ".ts" eos)
            (rx (backref 1) ".test.ts")
            (rx (backref 1) ".integration.ts"))
     ))

  :config
  (define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
    "Prevent the editor blocking to inform you when an autosave file exists."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args))))

(use-package tramp
  ;; Provides remote editing support, e.g. over SSH connections.
  :after files
  :config
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-auto-save-directory (file-name-concat user-emacs-directory "tramp-autosave/")))

(use-package uniquify
  ;; Controls how buffers with conflicting names are managed.
  :custom
  (uniquify-buffer-name-style 'forward))


;; Disable bidirectional text by default.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

(use-package elpaca
  ;; Configure aspects of elpaca not required for initial package bootstrap.
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" #'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" #'quit-window))

(use-package profiler
  :config
  (defun +profiler-stop-and-report (&optional continue-p)
    "Stop the profiler and show results.

With optional prefix arg CONTINUE-P, keep profiling."
    (interactive "P")
    (let ((ran-p (profiler-running-p)))

      (unless continue-p
        (profiler-stop))
      (profiler-report)
      (when ran-p
        (if continue-p
            (message "Profiler still recording")
          (message "Profiler stopped")))))

  (defun +profile-find-file ()
    "Profile opening a file. Prompts for file to open."
    (interactive)
    (profiler-start 'cpu+mem)
    (message "Profiler started. Opening file...")
    (call-interactively #'find-file)
    (run-with-idle-timer 2.0 nil
                         (lambda ()
                           (profiler-stop)
                           (profiler-report)
                           (message "Profiling complete. Check *Profiler-Report* buffer."))))

  (defun +profile-next-command ()
    "Profile the next command you execute."
    (interactive)
    (profiler-start 'cpu+mem)
    (message "Profiler started. Execute your command, then run M-x +profiler-stop-and-report")
    (add-hook 'post-command-hook
              (lambda ()
                (message "Command executed. Run M-x +profiler-stop-and-report to see results.")
                (remove-hook 'post-command-hook '+profile-cleanup))
              nil t))

  (defun +benchmark-find-file (file-path &optional iterations)
    "Benchmark opening FILE-PATH multiple times.
ITERATIONS defaults to 5."
    (interactive "fFile to benchmark: \nP")
    (let* ((iterations (or iterations 5))
           (times '())
           (gc-times '()))
      (dotimes (i iterations)
        ;; Close buffer if already open
        (when-let* ((buf (get-file-buffer file-path)))
          (kill-buffer buf))
        ;; Force GC and measure
        (garbage-collect)
        (let* ((start-time (current-time))
               (start-gc-time (float-time)))
          ;; Open file
          (find-file file-path)
          (let* ((end-time (current-time))
                 (elapsed (float-time (time-subtract end-time start-time)))
                 (gc-stats (garbage-collect))
                 (gc-elapsed (float-time (time-subtract end-time start-time))))
            (push elapsed times)
            (message "Iteration %d/%d: %.3fs"
                     (1+ i) iterations elapsed))))
      (let ((avg (/ (apply #'+ times) (float iterations)))
            (min-time (apply #'min times))
            (max-time (apply #'max times)))
        (message "\n=== Benchmark Results ===\nFile: %s\nIterations: %d\nAverage: %.3fs\nMin: %.3fs\nMax: %.3fs"
                 file-path iterations avg min-time max-time)))))

(use-package goto-addr
  ;; Turns URLs in the buffer into clickable buttons.
  :init
  (defun +goto-address-maybe-h ()
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (goto-address)
      (goto-address-mode +1)))
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h)

  ;; Teach evil-ret to open URLs.
  :init
  (define-advice evil-ret (:before-until (&rest _) goto-addr)
    (when-let* ((url (thing-at-point 'url)))
      (browse-url url)
      t)))

(use-package better-jumper :ensure t
  :disabled t
  ;; Maintains a jump list so you can more easily get back to where you were if
  ;; a command takes you somewhere else.
  :after-call +first-file-hook +first-buffer-hook
  :preface
  (defun +set-jump-point ()
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)
  :init
  (better-jumper-mode +1)

  :config
  (add-hook 'kill-buffer-hook #'+set-jump-point)
  (advice-add #'consult-imenu :before #'+set-jump-point)
  (advice-add #'org-mark-ring-push :before #'+set-jump-point)
  (add-hook 'org-open-at-point-functions #'+set-jump-point)

  :general-config
  ([remap evil-jump-forward]  #'better-jumper-jump-forward
   [remap evil-jump-backward] #'better-jumper-jump-backward
   [remap xref-pop-marker-stack] #'better-jumper-jump-backward
   [remap xref-go-back] #'better-jumper-jump-backward
   [remap pop-tag-mark] #'better-jumper-jump-backward
   [remap xref-go-forward] #'better-jumper-jump-forward))

(use-package server
  ;; Use existing Emacs instances to edit files as $EDITOR.
  :if (display-graphic-p)
  :after-call +first-input-hook +first-file-hook
  :config
  (unless (server-running-p)
    (server-start)))

(use-package so-long
  ;; Improve performance of files with very long lines.
  :hook (elpaca-after-init-hook . global-so-long-mode))

;; Teach Emacs that C-i and C-m do in fact exist.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (lambda (&rest _args)
     (interactive)
     (if (when-let* ((keys (this-single-command-raw-keys)))
           (and (display-graphic-p)
                (not (cl-loop for event in events
                              if (cl-position event keys)
                              return t))
                ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                ;; broken all pre-existing FALLBACK keybinds.
                (key-binding (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                      key)
                             nil t)))
         key fallback))))

;; TODO: Why is this bound on some installations but not others? 🤔
(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "modules/"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "lisp/")))

(use-package string-inflection :ensure t
  ;; Provides commands for cycling different string casing styles for the ident
  ;; at point, e.g. UpperCamelCase, lowerCamelCase, etc.
  :general (:states '(normal insert) "M--" #'string-inflection-all-cycle))

(use-package envrc :ensure t
  ;; Adds direnv support.
  :hook (+first-file-hook . envrc-global-mode)
  :custom
  (envrc-show-summary-in-minibuffer nil) ; very noisy.
  )

(use-package exec-path-from-shell :ensure t
  ;; Use the shell to get some environment vars; necessary when the window
  ;; system runs Emacs under a very different process environment.
  ;;
  ;; Also, turns out we need this for direnv to work right in compilation buffers.
  :after-call +first-buffer-hook +first-file-hook
  :if (memq system-type '(darwin x))
  :demand t
  :config
  (pushnew! exec-path-from-shell-variables
            "SSH_AUTH_SOCK"
            "SSH_AGENT_PID")

  ;; Speed up by using a non-interactive shell.
  (delq! "-i" exec-path-from-shell-arguments)

  (exec-path-from-shell-initialize))

(use-package mise :ensure t
  :demand t
  :hook (+first-file-hook . global-mise-mode))


(use-package proced
  ;; User-process management UI.
  :custom
  (proced-enable-color-flag t))

(use-package replace
  ;; Defines search+replace functionality, including `occur'.
  :hook
  (occur-mode-hook . hl-line-mode))

(use-package grep
  ;; Buffers showing filesystem search results. The default program is grep;
  ;; change it to ripgrep.
  :custom
  (grep-use-headings t)
  (grep-template "rg --line-number --with-filename --null --regexp <R> <F>"))

(use-package wgrep :ensure t
  ;; Adds a mode for grep-like results buffers that allows you to edit the
  ;; underlying files directly.
  ;;
  ;; TODO: Replace with built-in `grep-edit-mode' once I'm on Emacs 31.
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package xref
  ;; Provides the interface for navigating symbol definitions & references, etc.
  :custom
  (xref-search-program 'ripgrep))

(use-package indent
  ;; Indentation behaviour.
  :custom
  (tab-first-completion 'word-or-paren-or-punct))

(provide 'mod-core-extras)
;;; mod-core-extras.el ends here
