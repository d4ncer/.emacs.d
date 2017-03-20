;;; rk-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-define-key "evil-core")
(autoload 'projectile-project-p "projectile")
(autoload 'f-join "f")
(autoload 'f-split "f")

(use-package web-mode
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :defer t

  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    ;; Disable web-mode-reload binding
    (define-key web-mode-map (kbd "C-c C-r") nil)

    ;; Use line comments when commenting in JS.

    (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))))

(use-package rk-web-modes
  :defer t
  :mode (("\\.json\\'" . rk-web-json-mode)
         ("\\.eslintrc\\'" . rk-web-json-mode)
         ("\\.babelrc\\'" . rk-web-json-mode)
         ("\\.es6\\'"  . rk-web-js-mode)
         ("\\.tsx?\\'"  . rk-web-typescript-mode)
         ("\\.jsx?\\'" . rk-web-js-mode)
         ("\\.css\\'"  . rk-web-css-mode)
         ("\\.scss\\'"  . rk-web-css-mode)
         ("\\.html\\'" . rk-web-html-mode))
  :defines (flycheck-html-tidy-executable)
  :config
  (with-eval-after-load 'flycheck
    (let ((tidy-bin "/usr/local/bin/tidy"))
      (when (file-exists-p tidy-bin)
        (setq flycheck-html-tidy-executable tidy-bin)))

    (flycheck-add-mode 'typescript-tslint 'rk-web-typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
    (flycheck-add-mode 'css-csslint 'rk-web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'rk-web-json-mode)
    (flycheck-add-mode 'html-tidy 'rk-web-html-mode)))

(use-package flycheck
  :defer t
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :preface

  (defun rk-web--add-node-modules-bin-to-path ()
    "Use binaries from node_modules, where available."
    (when-let (root (projectile-project-p))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)

    (add-hook 'rk-web-typescript-mode-hook #'rk-web--add-node-modules-bin-to-path)
    (add-hook 'rk-web-js-mode-hook #'rk-web--add-node-modules-bin-to-path)))

(use-package emmet-mode
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
  :preface
  (progn
    (defun rk-web--set-jsx-classname-on ()
      (setq-local emmet-expand-jsx-className? t))

    (defun rk-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'rk-web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((and (derived-mode-p 'rk-web-js-mode)
             (buffer-file-name)
             (memq "components" (f-split (buffer-file-name))))
        (emmet-mode +1)))))

  :init
  (add-hook 'web-mode-hook #'rk-web--maybe-emmet-mode)
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line)
    (add-hook 'rk-web-js-mode-hook #'rk-web--set-jsx-classname-on)))

(use-package rk-flow-checker
  :disabled t
  :defer t
  :after flycheck)

(use-package flycheck-flow
  :after flycheck
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'rk-web-js-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package rk-flow
  :after rk-web-modes
  :commands (rk-flow-insert-flow-annotation
             rk-flow-type-at)
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'rk-web-js-mode
    "fi" #'rk-flow-insert-flow-annotation
    "ft" #'rk-flow-type-at))

(use-package tern
  :defer t
  :functions (tern-mode)
  :commands (tern-find-definition tern-pop-find-definition)
  :init
  (add-hook 'rk-web-js-mode-hook #'tern-mode)
  :config
  (progn
    (setq tern-command (add-to-list 'tern-command "--no-port-file" t))

    (unless (getenv "NODE_PATH")
      (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

    (spacemacs-keys-set-leader-keys-for-major-mode 'rk-web-js-mode
      "fT" #'tern-find-definition
      "fD" #'tern-pop-find-definition)))

(use-package company-tern
  :after rk-web-modes
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

(use-package aggressive-indent
  :defer t
  :preface
  (defun rk-web--in-flow-strict-object-type? ()
    (when (derived-mode-p 'rk-web-js-mode)
      (-let [(depth start) (syntax-ppss)]
        (and (plusp depth)
             (eq (char-after start) ?{)
             (eq (char-after (1+ start)) ?|)))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(rk-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'rk-web--in-flow-strict-object-type?)))


(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
