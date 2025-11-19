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
  :init
  (defun +toggle-eldoc-buffer ()
    "Toggle the eldoc documentation buffer.
If the eldoc buffer is visible, close it. Otherwise, show documentation
for the symbol at point in a dedicated buffer."
    (interactive)
    (let ((eldoc-buffer (get-buffer "*eldoc*")))
      (if (and eldoc-buffer (get-buffer-window eldoc-buffer 'visible))
          ;; Buffer is visible, close it
          (quit-window nil (get-buffer-window eldoc-buffer 'visible))
        ;; Buffer not visible, show it
        (eldoc-doc-buffer t))))

  :general
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states '(normal motion)
   "K" #'+toggle-eldoc-buffer
   "C-c C-r" #'eglot-rename))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
