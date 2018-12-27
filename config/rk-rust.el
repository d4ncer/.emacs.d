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

  :config
  (progn
    ;; Enable backtraces in rust programs run from Emacs.
    (setenv "RUST_BACKTRACE" "1")

    (general-def :keymaps 'rust-mode-map :states '(insert)
      "TAB" #'company-indent-or-complete-common)
    (general-def :keymaps 'rust-mode-map :states 'normal
      "J" #'rk-rust--join-line)

    (rk-local-leader-def :keymaps 'rust-mode-map
      "." '(rust-format-buffer :wk "format"))

    (add-hook 'rust-mode-hook #'rk-rust--set-local-vars)))

;; Similar to JS, need to set this up right. As of now, it seems lackluster :(
;; (use-package lsp-rust
;;   :straight t
;;   :after rust-mode
;;   :config
;;   (progn
;;     (autoload 'flycheck-mode "flycheck")
;;     (autoload 'rust-mode-hook "rust-mode")
;;     (add-hook 'rust-mode-hook #'lsp-rust-enable)
;;     (add-hook 'rust-mode-hook #'flycheck-mode)))

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :straight t
  :after rust-mode
  :commands (racer-find-definition racer-describe)
  :general
  (:keymaps 'rust-mode-map :states '(normal motion)
            "gd" #'racer-find-definition
            "K" #'racer-describe)
  :hook (rust-mode . racer-mode)
  :config
  (progn
    (add-hook 'racer-mode-hook #'eldoc-mode)

    (evil-set-initial-state 'racer-help-mode 'motion)

    ;; Teach compile.el about sources installed via rustup.
    (let ((base (file-name-directory racer-rust-src-path)))
      (add-to-list 'compilation-search-path base t))))

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

;; Rust backtraces sometimes contain absolute paths from travis builds. Rewrite
;; these to paths relative to the rustup sources directory.

(use-package compile
  :preface
  (progn
    (defun rk-rust--rewrite-compilation-buffer (&optional buf &rest _)
      (with-current-buffer (or buf (current-buffer))
        (save-excursion
          (goto-char (or compilation-filter-start (point-min)))
          (let ((inhibit-read-only t)
                (bad-path "/Users/travis/build/rust-lang/rust/"))
            (while (search-forward-regexp (rx-to-string `(or "" ,bad-path)) nil t)
              (replace-match "" t t)))))))
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'compilation-filter-hook #'rk-rust--rewrite-compilation-buffer)
    (add-to-list 'compilation-finish-functions #'rk-rust--rewrite-compilation-buffer)))

(provide 'rk-rust)

;;; rk-rust.el ends here
