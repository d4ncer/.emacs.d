;;; +expert-compat.el --- Expert LSP compatibility -*- lexical-binding: t; -*-

;;; Commentary:
;; Expert 0.1.5/0.1.6 protocol and environment workarounds. Remove each
;; workaround after its regression is verified fixed in the deployed server.
;; Upstream: https://github.com/expert-lsp/expert

;;; Code:

(require 'eglot)

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

;; Expert 0.1.5 strictly validates the parameterless LSP `exit'
;; notification and rejects Eglot's empty object (`params: {}').  Omit the
;; field for Expert so an intentional shutdown does not look like a crash.
(defun +expert-omit-exit-params (args)
  "Omit PARAMS from an Expert `exit' notification in ARGS."
  (pcase-let ((`(,server ,method ,_params) args))
    (if (and (eq method :exit)
             (object-of-class-p server 'eglot-lsp-server)
             (+eglot-expert-p server))
        (list server method :jsonrpc-omit)
      args)))

(defun +expert-force-full-sync (&rest _)
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
  ;; `eglot-managed-mode-hook' runs both when Eglot starts and when it stops
  ;; managing a buffer.  During teardown `eglot-current-server' can still
  ;; return the cached (already dead) server, so do not try to notify it.
  (when (eglot-managed-p)
    (when-let* ((server (eglot-current-server))
                ((+eglot-expert-p server))
                (elixir (executable-find "elixir")))
      (setq-local eglot-workspace-configuration
                  (append (list :elixirExecutablePath elixir)
                          (when-let* ((erl (executable-find "erl")))
                            (list :erlangExecutablePath erl))))
      (eglot-signal-didChangeConfiguration server))))

(define-minor-mode +expert-compat-mode
  "Apply Expert-specific Eglot protocol and environment workarounds."
  :global t :group 'eglot
  (if +expert-compat-mode
      (progn
        (advice-add 'jsonrpc-notify :filter-args #'+expert-omit-exit-params)
        (advice-add 'eglot--signal-textDocument/didChange
                    :before #'+expert-force-full-sync)
        (add-hook 'eglot-managed-mode-hook #'+expert-pin-toolchain))
    (advice-remove 'jsonrpc-notify #'+expert-omit-exit-params)
    (advice-remove 'eglot--signal-textDocument/didChange
                   #'+expert-force-full-sync)
    (remove-hook 'eglot-managed-mode-hook #'+expert-pin-toolchain)))

(provide '+expert-compat)
;;; +expert-compat.el ends here
