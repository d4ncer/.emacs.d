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

(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive t)
  :config
  ;; Enable backtraces in rust programs run from Emacs.
  (setenv "RUST_BACKTRACE" "1"))

(use-package rust-mode
  :after eglot
  :preface
  (defun rk-rust/set-local-vars ()
    (setq-local compile-command "cargo build"))
  (defun rk-rust/setup ()
    (rk-rust/set-local-vars)
    (eglot-ensure))
  :hook (rust-ts-mode . rk-rust/setup)
  :config
  (rk-local-leader-def :keymaps 'rust-mode-map
    "." '(rust-format-buffer :wk "format")))

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
    "c" '(rk-rust--main/body :wk "hydra")))

(provide 'rk-rust)

;;; rk-rust.el ends here
