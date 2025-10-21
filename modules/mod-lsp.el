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
  :hook (prog-mode-hook . flymake-mode)
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
  :general
  (:states '(normal motion)
           "K" #'eldoc-box-help-at-point))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
