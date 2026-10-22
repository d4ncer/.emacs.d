;;; mod-lsp.el --- LSP and tree-sitter configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains LSP and tree-sitter configuration including:
;; - Native tree-sitter mode selection and grammar installation
;; - flymake for error checking
;; - +flymake-posframe for visual error display
;; - eglot for LSP integration
;; - eldoc-box for documentation display

;;; Code:

;;; Tree-sitter

(use-package treesit
  :demand t
  :custom
  (treesit-font-lock-level 3)
  (treesit-auto-install-grammar 'ask)
  (treesit-enabled-modes
   '(bash-ts-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode css-ts-mode
     elixir-ts-mode heex-ts-mode java-ts-mode js-ts-mode json-ts-mode
     markdown-ts-mode mhtml-ts-mode python-ts-mode rust-ts-mode
     tsx-ts-mode typescript-ts-mode yaml-ts-mode))
  :config
  (dolist (entry (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "treesit-grammars.eld" +config-root))
                  (read (current-buffer))))
    (setf (alist-get (car entry) treesit-language-source-alist) (cdr entry))))

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
              (let ((buf (current-buffer)))
                (run-with-idle-timer 1.0 nil
                                     (lambda ()
                                       (when (buffer-live-p buf)
                                         (with-current-buffer buf
                                           (flymake-mode 1)))))))
            80)
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))

(use-package posframe :ensure t
  ;; Child-frame library, used by `+flymake-posframe'.
  :defer t)

(use-package +flymake-posframe
  ;; Local replacement for the unmaintained `flymake-posframe', which called the
  ;; internal `flymake--diag-text' -- removed in Emacs 30+, so every invocation
  ;; signalled from `post-command-hook'.
  :after flymake
  :custom
  (+flymake-posframe-warning-prefix "⚠️")
  (+flymake-posframe-error-prefix "❌")
  (+flymake-posframe-note-prefix "ℹ️")
  (+flymake-posframe-default-prefix "❓")
  :config
  (custom-set-faces
   '(+flymake-posframe-face ((t (:inherit +subtle))))
   '(+flymake-posframe-border-face ((t (:inherit +subtle)))))
  :hook (flymake-mode-hook . +flymake-posframe-mode))

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

  (with-eval-after-load 'evil
    (add-hook 'eglot-managed-mode-hook 'evil-normalize-keymaps))

  :general
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states 'normal
   "K" #'+toggle-eldoc-buffer
   "C-c C-r" #'eglot-rename))

(use-package eglot-tempel
  :ensure t
  :after eglot
  ;; `:demand' is required: without it, use-package has no autoload trigger for
  ;; this package, so the `:config' body is wrapped in a nested
  ;; `eval-after-load' on eglot-tempel itself that nothing ever fires.
  ;;
  ;; `eglot-tempel-mode' must also be on before a server connects, since eglot
  ;; sends `snippetSupport' at initialize time.
  :demand t
  :config
  (require '+eglot-tempel-compat)
  (+eglot-tempel-compat-mode 1)
  (eglot-tempel-mode t))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
