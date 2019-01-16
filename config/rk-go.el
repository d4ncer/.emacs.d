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

(autoload 'projectile-project-p "projectile")

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
    (defun rk-go-lookup-go-root ()
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
      (unless (getenv "GOROOT")
        (setenv "GOROOT" (rk-go-lookup-go-root)))))

  :config
  (progn
    (setq gofmt-command "goreturns")
    (setq godoc-at-point-function 'godoc-gogetdoc)
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
  :straight t
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

(use-package go-eldoc
  :straight t
  :after go-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

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
