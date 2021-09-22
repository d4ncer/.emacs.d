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

(defconst rk-web--node-js-lts-version "v10.15.1"
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
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)))

(use-package rk-web-modes
  :defer t
  :mode (("\\.es6\\'"  . rk-web-js-mode)
         ("\\.jsx?\\'" . rk-web-js-mode)
         ("\\.css\\'"  . rk-web-css-mode)
         ("\\.scss\\'"  . rk-web-css-mode)
         ("\\.html\\'" . rk-web-html-mode)
         ("\\.tsx\\'" . rk-web-tsx-mode))

  ;; :hook
  ;; (rk-web-tsx-mode . lsp)

  :preface
  (defun rk-web--add-custom-eslint-rules-dir ()
    (-when-let* ((root (projectile-project-p))
                 (rules-dir (f-join root "rules"))
                 (rules-dir-p (f-exists-p rules-dir)))
      (setq-local flycheck-eslint-rules-directories (-list rules-dir))))

  :config
  (progn
    (general-def :keymaps 'rk-web-js-mode-map :states 'normal
      "J" #'rk-utils--chainable-aware-join-line)

    ;; Add all possible JS runtimes

    (dolist (name (list "node" "nodejs" "gjs" "rhino"))
      (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'rk-web-js-mode)))

    ;; Use custom ESLint rules if a "rules" dir exists in project root

    (add-hook 'rk-web-js-mode-hook #'rk-web--add-custom-eslint-rules-dir)

    ;; Setup post flycheck load

    (with-eval-after-load 'flycheck
      (setq flycheck-html-tidy-executable (locate-file "tidy" exec-path))
      (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
      (flycheck-add-mode 'javascript-eslint 'rk-web-tsx-mode)
      (flycheck-add-mode 'css-csslint 'rk-web-css-mode)
      (flycheck-add-mode 'json-jsonlint 'rk-web-json-mode)
      (flycheck-add-mode 'html-tidy 'rk-web-html-mode))))

(use-package emmet-mode
  :straight t
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
  :general (:keymaps 'emmet-mode-keymap :states '(normal insert)
                     "M-;" #'emmet-expand-line)
  :preface
  (progn
    (defun rk-web--react-in-buffer-p ()
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (search-forward "React" nil t))))

    (defun rk-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'rk-web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((and (derived-mode-p 'rk-web-js-mode 'rk-web-tsx-mode)
             (rk-web--react-in-buffer-p))
        (progn
          (setq-local emmet-expand-jsx-className? t)
          (emmet-mode +1))))))

  :init
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (add-hook 'typescript-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'rk-web--maybe-emmet-mode)))

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
  (progn
    (flycheck-add-mode 'css-stylelint 'rk-web-css-mode)
    (add-hook 'rk-web-css-mode-hook #'rk-web--set-stylelintrc)))

(use-package prettier-js
  :straight t
  :preface
  (defun rk-web--prettier-enable-p ()
    "Enable prettier if no .prettierdisable is found in project root."
    (-when-let (root (projectile-project-p))
      (not (f-exists? (f-join root ".prettierdisable")))))

  (defun rk-web--init-prettier-config ()
    "Set up prettier config & binary for file if applicable."
    (-if-let* ((root (projectile-project-p))
               (prettier-bin (f-join root "node_modules/.bin/prettier"))
               (prettier-bin-p (f-exists? prettier-bin))
               (prettier-config (s-trim (shell-command-to-string
                                         (s-join " " (list prettier-bin "--find-config-path" (buffer-file-name)))))))
        (progn
          (setq-local prettier-js-command prettier-bin)
          (setq-local prettier-js-args (list "--config" prettier-config)))
      (setq-local prettier-js-args rk-web--prettier-default-args)))

  (defun rk-web--init-prettier ()
    (when (rk-web--prettier-enable-p)
      (progn
        (rk-web--init-prettier-config)
        (prettier-js-mode +1))))

  (defun rk-web--maybe-setup-prettier ()
    (when (or (equal major-mode 'typescript-mode)
              (equal major-mode 'graphql-mode)
              (and (derived-mode-p 'web-mode)
                   (-contains-p '("javascript" "jsx" "html" "css") web-mode-content-type)))
      (rk-web--init-prettier)))

  :init
  (add-hook 'graphql-mode-hook #'rk-web--maybe-setup-prettier)
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-prettier)
  (add-hook 'rk-web-tsx-mode-hook #'rk-web--maybe-setup-prettier)
  (add-hook 'typescript-mode-hook #'rk-web--maybe-setup-prettier))

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

(use-package fnm
  :after rk-web-modes
  :commands (fnm-use fnm-use-for-buffer)
  :preface
  (defun rk-web--maybe-use-fnm ()
    (-if-let* ((project-nvmrc (locate-dominating-file default-directory ".nvmrc"))
               (file-p (f-exists-p project-nvmrc))
               (fnm-exec-p (executable-find "fnm")))
        (fnm-use-for-buffer)
      (fnm-use rk-web--node-js-lts-version)))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-use-fnm))

(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
