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

;; TODO: Replace this with the LSP once it stops sucking
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)

  :general
  (:keymaps 'go-mode-map :states 'normal
            "gd" #'godef-jump
            "K" #'godoc-at-point)
  (:keymaps 'go-mode-map :states 'insert
            "M-." #'godef-jump)

  :preface
  (progn
    (defun rk-go--go-modules-p ()
      (f-exists-p (f-join (projectile-project-p) "go.mod")))

    (defun rk-go--lookup-go-root ()
      (-let* ((default-directory (or (projectile-project-p) default-directory))
              (output (s-lines (s-trim (shell-command-to-string "go env"))))
              ((&alist "GOROOT" go-root)
               (--map (-let* (((var val) (s-split "=" it))
                             ((_ val) (s-match (rx "\"" (group (*? nonl)) "\"") val)))
                       (cons var val))
                     output)))
        go-root))

    (defun rk-go--set-local-vars ()
      (setq-local tab-width 4)
      (setq-local indent-tabs-mode t)
      (setq-local compile-command "go build -v")
      (with-no-warnings
        (setq-local evil-shift-width 4))
      (if (rk-go--go-modules-p)
          (setenv "GO111MODULE" "on")
        (setenv "GO111MODULE" "off"))
      (unless (getenv "GOROOT")
        (setenv "GOROOT" (rk-go--lookup-go-root)))))

  :config
  (progn
    (setq gofmt-command "goimports")
    (setq gofmt-show-errors nil)
    (rk-local-leader-def :keymaps 'go-mode-map
      "r" '(:ignore t :wk "refactor")

      "g" '(:ignore t :wk "goto")
      "g g" '(godef-jump :wk "jump to def")
      "g d" '(godef-describe :wk "describe")
      "g w" '(godef-jump-other-window :wk "jump to def (other window)")
      "g c" '(go-coverage :wk "coverage")

      "i" '(:ignore t :wk "imports")
      "i g" '(go-goto-imports :wk "go to imports")
      "i a" '(go-import-add :wk "add import")
      "i r" '(go-remove-unused-imports :wk "remove unused"))

    (add-hook 'go-mode-hook #'rk-go--set-local-vars)
    (add-hook 'before-save-hook #'gofmt-before-save))

  :functions (gofmt-before-save godoc-at-point))

(use-package go-rename
  :straight t
  :after go-mode
  :config
  (rk-local-leader-def :keymaps 'go-mode-map
    "r r" '(go-rename :wk "rename")))

(use-package company-go
  :straight (:host github :repo "mdempsky/gocode"
                   :files ("emacs-company/company-go.el")
                   :branch "master")
  :after go-mode
  :preface
  (progn
    (autoload 'company-mode "company")

    (defun rk-go-company-setup ()
      (set (make-local-variable 'company-backends) '(company-go))
      (company-mode)))

  :config
  (progn
    (with-no-warnings
      (setq company-go-show-annotation t))
    (add-hook 'go-mode-hook #'rk-go-company-setup)))

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
