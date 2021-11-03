;;; rk-rust.el --- Rust config  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 's)
(require 'definers)
(require 'general)
(require 'lsp)
(require 'rk-utils)

(use-package rust-mode
  :straight t
  :after lsp-mode
  :mode ("\\.rs\\'" . rust-mode)
  :preface
  (progn
    (defun rk-rust--set-local-vars ()
      (setq lsp-rust-server 'rust-analyzer)
      (setq-local compile-command "cargo build"))

    (defun rk-rust--setup ()
      (rk-rust--set-local-vars)
      (lsp)))

  :custom
  (rust-rustfmt-bin (executable-find "rustfmt"))
  (rust-format-on-save t)
  :config
  ;; Enable backtraces in rust programs run from Emacs.
  (setenv "RUST_BACKTRACE" "1")

  (general-def :keymaps 'rust-mode-map :states 'normal
    "J" #'rk-utils--chainable-aware-join-line)

  (rk-local-leader-def :keymaps 'rust-mode-map
    "." '(rust-format-buffer :wk "format"))

  (add-hook 'rust-mode-hook #'rk-rust--setup))

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package toml-mode
  :straight t
  :mode (("\\.toml\\'" . toml-mode)
         ("\\.Cargo\\.lock\\'" . toml-mode)
         ("\\.cargo/config\\'" . toml-mode)))

(use-package cargo
  :straight t
  :after rust-mode
  :config
  ;; Enable backtraces in Cargo processes started by Emacs.
  (setenv "RUST_BACKTRACE" "1"))

(use-package rust-faces
  :after rust-mode)

(use-package rust-hydra
  :after rust-mode
  :config
  (rk-local-leader-def :keymaps 'rust-mode-map
    "c" '(rust-hydra-transient-state/body :wk "rust hydra")))

(provide 'rk-rust)

;;; rk-rust.el ends here
