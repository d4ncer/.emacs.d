;;; rk-go.el --- Configuration for golang.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 's)
(require 'definers)
;; (require 'lsp)

(autoload 'projectile-project-p "projectile")

;; TODO: Replace this with the LSP once it stops sucking
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

;; (use-package go-mode
;;   :straight t
;;   :mode ("\\.go\\'" . go-mode)
;;   :preface
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("gopls" "-logfile=/tmp/gopls/log-1" "server"))
;;                     :major-modes '(go-mode)
;;                     :priority -3
;;                     :server-id 'gopls))
;;   :config
;;   (add-hook 'go-mode-hook #'lsp)
;;   (add-hook 'lsp-go-mode-hook #'flycheck-mode))

(use-package go-tag
  :straight t
  :commands (go-tag-add
             go-tag-remove)
  :after go-mode
  :config
  (progn
    (setq go-tag-args (list "-transform" "camelcase"))
    (rk-local-leader-def :keymaps 'go-mode-map
      "T"   '(:ignore t :wk "tags")
      "T a" '(go-tag-add :wk "add")
      "T r" '(go-tag-remove :wk "remove"))))

(use-package go-keyify
  :after go-mode
  :config
  (rk-local-leader-def :keymaps 'go-mode-map
    "r k" '(go-keyify :wk "keyify")))

(use-package rk-go-run
  :after go-mode
  :config
  (progn
    (rk-local-leader-def :keymaps 'go-mode-map
      "t"   '(:ignore t :wk "test")
      "t t" '(rk-go-run-test-current-function :wk "current fn")
      "t s" '(rk-go-run-test-current-suite :wk "current suite")
      "t p" '(rk-go-run-package-tests :wk "package")
      "t P" '(rk-go-run-package-tests-nested :wk "package (nested)")
      "x"   '(rk-go-run-main :wk "run"))
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*go " (or "test" "run") "*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))))

(provide 'rk-go)

;;; rk-go.el ends here
