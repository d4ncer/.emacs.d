;;; rk-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)
(require 'dash)
(require 'f)
(require 's)
(require 'flycheck)
(require 'lsp)
(require 'ht)
(require 'rk-utils)
(require 'projectile)

(defconst rk-web--node-js-lts-version "v16.13.0"
  "The version of Node to use by default if .nvmrc isn't found.")

(defconst rk-web--prettier-default-args
  (list "--single-quote" "true" "--trailing-comma" "es5")
  "Default values for prettier.")

(use-package web-mode
  :straight t
  :demand t
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :general
  (:keymaps 'web-mode-map
            "C-c C-r" nil)
  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)

  (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))

  ;; Change default indentation behaviour.

  (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
  (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
  (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil))

(use-package rk-web-modes
  :mode (("\\.es6\\'"  . rk-web-js-mode)
         ("\\.jsx?\\'" . rk-web-js-mode)
         ("\\.css\\'"  . rk-web-css-mode)
         ("\\.scss\\'"  . rk-web-css-mode)
         ("\\.html\\'" . rk-web-html-mode))

  :preface
  (defun rk-web--add-custom-eslint-rules-dir ()
    (-when-let* ((root (projectile-project-p))
                 (rules-dir (f-join root "rules"))
                 (rules-dir-p (f-exists-p rules-dir)))
      (setq-local flycheck-eslint-rules-directories (-list rules-dir))))

  :config
  (general-def :keymaps 'rk-web-js-mode-map :states 'normal
    "J" #'rk-utils--chainable-aware-join-line)

  ;; Use custom ESLint rules if a "rules" dir exists in project root
  (add-hook 'rk-web-js-mode-hook #'rk-web--add-custom-eslint-rules-dir))

(use-package rk-web-modes
  :after flycheck
  :config
  (setq flycheck-html-tidy-executable (locate-file "tidy" exec-path))
  (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
  (flycheck-add-mode 'css-csslint 'rk-web-css-mode)
  (flycheck-add-mode 'json-jsonlint 'rk-web-json-mode)
  (flycheck-add-mode 'html-tidy 'rk-web-html-mode))

(use-package emmet-mode
  :straight t
  :defer t
  :after rk-web-modes
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
  :general (:keymaps 'emmet-mode-keymap :states '(normal insert)
                     "M-;" #'emmet-expand-line)
  :custom
  (emmet-move-cursor-between-quotes t)
  :preface
  (defun rk-web--react-in-buffer-p ()
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward "React" nil t))))

  (defun rk-web--maybe-emmet-mode ()
    (cond
     ((derived-mode-p 'rk-web-html-mode 'html-mode 'nxml-mode)
      (emmet-mode +1))

     ((and (derived-mode-p 'rk-web-js-mode)
           (rk-web--react-in-buffer-p))
      (progn
        (setq-local emmet-expand-jsx-className? t)
        (emmet-mode +1)))))

  :init
  (add-hook 'web-mode-hook #'rk-web--maybe-emmet-mode))

(use-package rk-flycheck-stylelint
  :after flycheck
  :preface
  (defun rk-web--set-stylelintrc ()
    "Set either local or root stylelintrc"
    (-if-let* ((root (projectile-project-p))
               (root-rc (f-join root ".stylelintrc.json")))
        (setq-local flycheck-stylelintrc root-rc))
    (f-join user-emacs-directory "lisp" ".stylelintrc.json"))
  :config
  (flycheck-add-mode 'css-stylelint 'rk-web-css-mode)
  (add-hook 'rk-web-css-mode-hook #'rk-web--set-stylelintrc))

(use-package prettier
  :straight t
  :after rk-web-modes
  :init
  (add-hook 'rk-web-js-mode-hook #'prettier-mode)
  (add-hook 'rk-web-css-mode-hook #'prettier-mode)
  (add-hook 'rk-web-html-mode-hook #'prettier-mode))

(use-package add-node-modules-path
  :straight t
  :after rk-web-modes
  :init
  (add-hook 'rk-web-css-mode-hook #'add-node-modules-path)
  (add-hook 'rk-web-js-mode-hook #'add-node-modules-path))

(use-package stylefmt
  :straight t
  :after rk-web-modes
  :commands (stylefmt-enable-on-save stylefmt-format-buffer)
  :config
  (rk-local-leader-def :keymaps 'rk-web-css-mode-map
    "." '(stylefmt-format-buffer :wk "format")))

(use-package tree-sitter-langs
  :after rk-web-modes
  :config
  (dolist (entry '((rk-web-css-mode . css) (rk-web-html-mode . html) (rk-web-js-mode . javascript))) (add-to-list 'tree-sitter-major-mode-language-alist entry)))

(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
