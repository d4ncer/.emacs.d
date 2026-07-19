;;; mod-languages.el --- Programming language configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains programming language configurations including:
;; - Lisp and Emacs Lisp
;; - Data formats (YAML, JSON)
;; - TypeScript/JavaScript
;; - Configuration files
;; - Markdown
;; - Elixir and Erlang
;; - Nix
;; - Tree-sitter mode remapping

;;; Code:

(eval-and-compile
  (defvar +lisp-dir (file-name-concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path +lisp-dir)
  (require '+corelib))

;;; Lisp and Emacs Lisp

(use-package lisp-mode
  ;; General configuration for all derived lisp modes.
  :config
  (add-hook! '(lisp-data-mode-hook emacs-lisp-mode-hook)
    (add-hook 'before-save-hook #'check-parens nil t)))

(use-package elisp-mode
  :general-config (:keymaps 'emacs-lisp-mode-map "C-c RET" #'pp-macroexpand-last-sexp)

  ;; Make lambdas look like λ.
  :hook (emacs-lisp-mode-hook . prettify-symbols-mode)

  :config
  (+local-leader-set-key 'emacs-lisp-mode-map
    "e" '(nil :which-key "eval")
    "es" '(eval-last-sexp :wk "last sexp")
    "eb" (list (defun +eval-buffer ()
                 (interactive)
                 (let ((inhibit-redisplay t))
                   (call-interactively #'eval-buffer)
                   (message "Buffer evaluated" ))
                 (when pulsar-mode
                   (pulsar--create-pulse (cons (point-min) (point-max)) 'pulsar-yellow)))
               :wk "buffer"))

  :init
  (use-package checkdoc
    :custom
    (checkdoc-force-docstrings-flag nil))

  (use-package +elisp
    :general (:keymaps 'emacs-lisp-mode-map "C-c C-c" #'+elisp-eval-dwim)

    :config
    (+local-leader-set-key 'emacs-lisp-mode-map
      "ee" '(+elisp-eval-dwim :wk "dwim"))

    ;; Improve plist indentation
    :autoload +elisp--calculate-lisp-indent-a
    :init
    (advice-add #'calculate-lisp-indent :override #'+elisp--calculate-lisp-indent-a))

  :custom
  (define-advice eval-region (:around (fn &rest args) clear-visual-state)
    (unwind-protect (apply fn args)
      (when (eq evil-state 'visual)
        (evil-normal-state)))))

;;; Data formats

(use-package yaml-ts-mode
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(use-package json-ts-mode)

(use-package kdl-mode
  :ensure (:host github :repo "taquangtrung/emacs-kdl-mode")
  :mode "\\.kdl\\'")

;;; TypeScript/JavaScript

(use-package typescript-ts-mode
  :config
  (pushnew! find-sibling-rules
            ;; Tests -> impl
            (list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
                  (rx (backref 1) ".ts"))
            ;; Impl -> tests
            (list (rx (group (+? any)) ".ts" eos)
                  (rx (backref 1) ".test.ts")
                  (rx (backref 1) ".integration.ts"))))

;;; Configuration and Markup

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-fontify-code-block-natively t)
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-ts-mode-hook . visual-line-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;###autoload
(define-derived-mode may-i-config-mode lisp-data-mode "Lisp-Data [may-i]"
  "Major-mode for may-i configuration files."
  (setcar font-lock-defaults
          (append '(lisp-el-font-lock-keywords
                    lisp-el-font-lock-keywords-1
                    lisp-el-font-lock-keywords-2)
                  (when (boundp 'elisp-semantic-font-lock-keywords)
                    '(elisp-semantic-font-lock-keywords)))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/may-i/config.lisp" eos)
                                    'may-i-config-mode))

(put 'defcontext 'lisp-indent-function 1)
(put 'with-facts 'lisp-indent-function 1)

;;; Elixir and Erlang

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook ((elixir-ts-mode-hook heex-ts-mode-hook) . eglot-ensure)
  :config
  ;; (with-eval-after-load 'eglot
  ;;   (let ((+elixir-ls-bin (file-name-concat user-emacs-directory "var/lsp-servers/elixir-ls/language_server.sh")))
  ;;     (add-to-list 'eglot-server-programs `((elixir-mode elixir-ts-mode heex-ts-mode) ,+elixir-ls-bin))))
  (with-eval-after-load 'eglot
    (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                     eglot-server-programs
                     nil nil #'equal)
          (eglot-alternatives
           '(("expert" "--stdio")                        ; Project-local via PATH/Nix
             ("/Users/rk/.local/bin/expert-ls" "--stdio")   ; Global fallback
             "start_lexical.sh"))))

  ;; Switching between files & tests

  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))

            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

;; Expert (as of 0.1.6) mishandles incremental `textDocument/didChange' edits at
;; the end of a document: it drops the leading indent of the final line, then
;; clamps the now out-of-range character offsets to 0, so typing "def" lands in
;; its copy as "fed". It then resolves `{:variable, :fed}' and returns zero
;; completions until the document is resynced.
;;
;; Eglot's payloads are correct -- verified against the wire log -- so work
;; around it by sending the whole buffer instead of ranges. `:emacs-messup' is
;; the sentinel `eglot--signal-textDocument/didChange' already checks to decide
;; on a full sync.
;;
;; Scoped to Expert alone: remove once upstream fixes the incremental path.

(defun +eglot-expert-p (server)
  "Return non-nil if SERVER is an Expert language server."
  (equal "Expert" (plist-get (eglot--server-info server) :name)))

(define-advice eglot--signal-textDocument/didChange
    (:before (&rest _) +expert-force-full-sync)
  "Force full-document sync on Expert servers."
  (when-let* (((not (eq eglot--recent-changes :emacs-messup)))
              (eglot--recent-changes)
              (server (eglot-current-server))
              ((+eglot-expert-p server)))
    (setq eglot--recent-changes :emacs-messup)))

;; Expert re-derives its own PATH by spawning a login shell (`/bin/zsh`) and so
;; never sees the per-project Elixir/Erlang that direnv + Nix put on this
;; buffer's `exec-path'. When the probe fails it silently falls back to its
;; bundled Elixir. Expert added an escape hatch in
;; https://github.com/expert-lsp/expert/pull/682: the `elixirExecutablePath' /
;; `erlangExecutablePath' settings, which it consults *before* the login-shell
;; probe (Expert.Port.project_executable/2). Changing either makes Expert
;; restart its engine node with that binary. It only reads these from a pushed
;; `workspace/didChangeConfiguration' (no pull, no initializationOptions), and
;; expects them at the top level of `settings' -- exactly the shape Eglot sends
;; `eglot-workspace-configuration' in. Resolve via `executable-find' rather than
;; hardcoding a /nix/store path so it survives `nix flake update'.

(defun +expert-pin-toolchain ()
  "Point Expert at the Elixir/Erlang on this buffer's (direnv/Nix) PATH."
  (when-let* ((server (eglot-current-server))
              ((+eglot-expert-p server))
              (elixir (executable-find "elixir")))
    (setq-local eglot-workspace-configuration
                (append (list :elixirExecutablePath elixir)
                        (when-let* ((erl (executable-find "erl")))
                          (list :erlangExecutablePath erl))))
    (eglot-signal-didChangeConfiguration server)))

(add-hook 'eglot-managed-mode-hook #'+expert-pin-toolchain)

(use-package inf-elixir :ensure t)

(use-package erlang :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :init
  (with-eval-after-load 'dired
    (pushnew! completion-ignored-extensions ".jam" ".vee" ".beam"))
  (with-eval-after-load 'dired-x
    (pushnew! dired-omit-extensions ".jam" ".vee" ".beam")))

;;; Nix

(use-package nix-ts-mode
  :ensure (:files ("nix-ts-mode.el"))
  :mode "\\.nix\\'")

;;; Tree-sitter mode remapping

;; Remap old modes to tree-sitter equivalents
(dolist (pair '((yaml-mode . yaml-ts-mode)
                (c-mode . c-ts-mode)
                (bash-mode . bash-ts-mode)
                (java-mode . java-ts-mode)
                (js2-mode . js-ts-mode)
                (javascript-mode . js-ts-mode)
                (typescript-mode . typescript-ts-mode)
                (js-json-mode . json-ts-mode)
                (css-mode . css-ts-mode)
                (rust-mode . rust-ts-mode)
                (python-mode . python-ts-mode)
                (nix-mode . nix-ts-mode)))
  (alist-set! major-mode-remap-alist (car pair) (cdr pair)))

;; Make shell-scripts etc executable on save.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'mod-languages)
;;; mod-languages.el ends here
