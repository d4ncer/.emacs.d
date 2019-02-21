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

(autoload 'evil-define-key "evil")

(use-package rust-mode
  :straight t
  :after lsp-mode
  :mode ("\\.rs\\'" . rust-mode)
  :preface
  (progn
    (autoload 'company-indent-or-complete-common "company")
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 'evil-join "evil-commands")

    (setq rust-format-on-save (executable-find "rustfmt"))

    (defun rk-rust--set-local-vars ()
      (setq-local compile-command "cargo run"))

    (defun rk-rust--join-line ()
      "Join lines, deleting intermediate spaces for chained function calls."
      (interactive)
      (call-interactively #'evil-join)
      (when (thing-at-point-looking-at (rx (not space) (* space) "."))
        (delete-horizontal-space))))

  :init
  (progn
    ;; LSP
    (add-hook 'rust-mode-hook #'lsp)
    (add-hook 'lsp-rust-mode-hook #'flycheck-mode))

  :config
  (progn
    ;; Enable backtraces in rust programs run from Emacs.
    (setenv "RUST_BACKTRACE" "1")

    (general-def :keymaps 'rust-mode-map :states 'normal
      "J" #'rk-rust--join-line)

    (rk-local-leader-def :keymaps 'rust-mode-map
      "." '(rust-format-buffer :wk "format"))

    (add-hook 'rust-mode-hook #'rk-rust--set-local-vars)))

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
