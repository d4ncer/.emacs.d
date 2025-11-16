;;; mod-lsp.el --- LSP and tree-sitter configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains LSP and tree-sitter configuration including:
;; - treesit-auto for automatic tree-sitter grammar installation
;; - flymake for error checking
;; - flymake-posframe for visual error display
;; - eglot for LSP integration
;; - eldoc-box for documentation display

;;; Code:

;;; Tree-sitter

;; Performance optimizations for tree-sitter
(setq treesit-font-lock-level 3)  ; Use level 3 (good balance of features/performance)

;; Enable tree-sitter query caching to avoid recompiling queries
;; This prevents the 93MB memory allocation on every file open
(defvar +treesit-query-cache (make-hash-table :test 'equal)
  "Cache for compiled tree-sitter queries to avoid recompilation.")

;; Advice treesit-query-compile to use caching
(define-advice treesit-query-compile (:around (orig-fn language query &optional eager) cache)
  "Cache compiled tree-sitter queries to avoid expensive recompilation."
  (if eager
      ;; Don't cache eager queries
      (funcall orig-fn language query eager)
    ;; Check cache for non-eager queries
    (let* ((cache-key (cons language query))
           (cached (gethash cache-key +treesit-query-cache)))
      (or cached
          (let ((compiled (funcall orig-fn language query eager)))
            (puthash cache-key compiled +treesit-query-cache)
            compiled)))))

(use-package treesit-auto :ensure t
  ;; Automatic installation of treesitter grammars.
  :after-call +first-buffer-hook +first-file-hook
  :commands global-treesit-auto-mode
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

;;; Flymake - Error checking

(use-package flymake
  ;; Frontend for in-buffer error checking & navigation.
  ;;
  ;; c.f. `next-error' and friends, which operate on compilation & grep results
  ;; across any number of buffers.
  :config
  ;; Defer flymake to avoid blocking file open
  (add-hook 'prog-mode-hook
            (lambda ()
              (run-with-idle-timer 1.0 nil
                                   (lambda ()
                                     (when (buffer-live-p (current-buffer))
                                       (with-current-buffer (current-buffer)
                                         (flymake-mode 1))))))
            80)
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))

(use-package flymake-posframe :ensure '(flymake-posframe :type git :host github
                                        :repo "Ladicle/flymake-posframe")
  :after flymake
  :custom
  (flymake-posframe-warning-prefix "⚠️")
  (flymake-posframe-error-prefix "❌")
  (flymake-posframe-note-prefix "ℹ️")
  (flymake-posframe-default-prefix "❓")
  :config
  (custom-set-faces
   '(flymake-posframe-face ((t (:inherit nano-subtle))))
   '(flymake-posframe-border-face ((t (:inherit nano-subtle)))))
  :hook (flymake-mode-hook . flymake-posframe-mode))

;;; Eglot - LSP integration

(use-package eglot
  ;; Emacs' built-in LSP integration.
  :custom (eglot-code-action-indications '(eldoc-hint))
  :general
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states '(normal)
   "C-c C-r" #'eglot-rename))

(use-package eldoc-box :ensure t
  :demand t
  ;; Displays eldoc info in a floating box instead of the echo area.
  :config
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode))

  ;; Fix for Emacs 31: prevent negative coordinates that cause
  ;; "wrong-type-argument wholenump" errors when frame geometry changes
  (defun +eldoc-box--position-fix (pos)
    "Ensure eldoc-box position coordinates are non-negative.
POS is a cons (X . Y) representing frame position."
    (cons (max 0 (car pos))
          (max 0 (cdr pos))))

  (advice-add 'eldoc-box--default-at-point-position-function-1
              :filter-return #'+eldoc-box--position-fix)

  ;; Cleanup childframe when switching to buffers without eldoc-box modes
  ;; Fixes issue where childframe persists after switching buffers
  (defun +eldoc-box--cleanup-on-buffer-switch ()
    "Hide eldoc-box childframe if current buffer shouldn't display it."
    (when (and (frame-live-p eldoc-box--frame)
               (frame-parameter eldoc-box--frame 'visibility)
               (not (or eldoc-box-hover-mode eldoc-box-hover-at-point-mode)))
      (eldoc-box-quit-frame)))

  (add-hook '+switch-buffer-hook #'+eldoc-box--cleanup-on-buffer-switch)
  (add-hook 'window-configuration-change-hook #'+eldoc-box--cleanup-on-buffer-switch)

  :general
  (:states '(normal motion)
           "K" #'eldoc-box-help-at-point))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
