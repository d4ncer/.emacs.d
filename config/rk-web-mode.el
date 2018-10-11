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

(autoload 'flow-minor-tag-present-p "flow-minor-mode")
(autoload 'flow-minor-configured-p "flow-minor-mode")

(defconst rk-web--prettier-default-args
  (list "--single-quote" "true" "--trailing-comma" "es5")
  "Default values for prettier.")

(defun rk-in-flow-buffer-p ()
  "Check if the buffer is a valid Flow buffer."
  (and (eq major-mode 'rk-web-js-mode)
       (flow-minor-configured-p)
       (flow-minor-tag-present-p)))

(use-package web-mode
  :straight t
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
         ("\\.jsx?\\'" . rk-web-js-mode)
         ("\\.css\\'"  . rk-web-css-mode)
         ("\\.scss\\'"  . rk-web-css-mode)
         ("\\.html\\'" . rk-web-html-mode))
  :defines (flycheck-html-tidy-executable)
  :config
  (progn
    (dolist (name (list "node" "nodejs" "gjs" "rhino"))
      (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'rk-web-js-mode)))

    (with-eval-after-load 'flycheck
      (let ((tidy-bin "/usr/local/bin/tidy"))
        (when (file-exists-p tidy-bin)
          (setq flycheck-html-tidy-executable tidy-bin)))

      (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
      (flycheck-add-mode 'css-csslint 'rk-web-css-mode)
      (flycheck-add-mode 'json-jsonlint 'rk-web-json-mode)
      (flycheck-add-mode 'html-tidy 'rk-web-html-mode))))

(use-package flycheck
  :defer t
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")

    (defun rk-web--add-custom-eslint-rules-dir ()
      (-when-let* ((root (projectile-project-p))
                   (rules-dir (f-join root "rules"))
                   (rules-dir-p (f-exists-p rules-dir)))
        (setq-local flycheck-eslint-rules-directories (-list rules-dir)))))

  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (add-to-list 'flycheck-disabled-checkers 'css-csslint)
    (add-hook 'rk-web-js-mode-hook #'rk-web--add-custom-eslint-rules-dir)))

;; TEMPORARY WHILE WE FIX ISSUES
;; (use-package rk-lsp-js
;;   :after rk-web-modes
;;   :commands (rk-lsp-js--setup)
;;   :config
;;   (progn
;;     (autoload 'rk-web-js-mode-hook "rk-web-modes")
;;     (rk-lsp-js--setup)
;;     (add-hook 'rk-web-js-mode-hook #'lsp-js-flow-enable)
;;     (add-hook 'lsp-js-flow-mode-hook 'flycheck-mode)))

(use-package emmet-mode
  :straight t
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
  :preface
  (progn
    (defun rk-web--buffer-contains-react ()
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (search-forward "React" nil t))))

    (defun rk-web--set-jsx-classname-on ()
      (setq-local emmet-expand-jsx-className? t))

    (defun rk-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'rk-web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((and (derived-mode-p 'rk-web-js-mode)
             (rk-web--buffer-contains-react))
        (emmet-mode +1)))))

  :general
  (:keymaps 'emmet-mode-map
            "C-/" #'emmet-expand-line)
  :init
  (add-hook 'web-mode-hook #'rk-web--maybe-emmet-mode)
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (add-hook 'rk-web-js-mode-hook #'rk-web--set-jsx-classname-on)))

(use-package rk-flycheck-stylelint
  :after flycheck
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")
    (defun rk-web--set-stylelintrc ()
      "Set either local or root stylelintrc"
      (-if-let* ((root (projectile-project-p))
                 (root-rc (f-join root ".stylelintrc.json")))
          (setq-local flycheck-stylelintrc root-rc))
      (f-join user-emacs-directory "lisp" ".stylelintrc.json")))
  :config
  (progn
    (flycheck-add-mode 'css-stylelint 'rk-web-css-mode)
    (add-hook 'rk-web-css-mode-hook #'rk-web--set-stylelintrc)))

;; Flow

(use-package flow-minor-mode
  :straight (:host github :repo "d4ncer/flow-minor-mode"
                   :branch "master")
  :general
  (:keymaps 'flow-minor-mode-map :states 'normal
            "gd" #'flow-minor-jump-to-definition
            "K"  #'flow-minor-type-at-pos)
  :after rk-web-modes
  :preface
  (progn
    (autoload 's-matches? "s")
    (autoload 'sp-get-enclosing-sexp "smartparens")

    (defun rk-flow-toggle-sealed-object ()
      "Toggle between a sealed & unsealed object type."
      (interactive)
      (-let [(&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)]
        (save-excursion
          (cond ((equal op "{")
                 (goto-char (1- end))
                 (insert "|")
                 (goto-char (1+ beg))
                 (insert "|"))
                ((equal op "{|")
                 (goto-char (- end 2))
                 (delete-char 1)
                 (goto-char (1+ beg))
                 (delete-char 1))
                (t
                 (user-error "Not in an object type"))))))

    (defun rk-flow-insert-flow-annotation ()
      "Insert a flow annotation at the start of this file."
      (interactive)
      (unless (not (rk-in-flow-buffer-p))
        (user-error "Buffer already contains an @flow annotation"))
      (save-excursion
        (goto-char (point-min))
        (insert "// @flow\n")
        (message "Inserted @flow annotation.")))

    (defun rk-flow-setup-bindings ()
      (rk-local-leader-def :keymaps 'rk-web-js-mode-map
        "f" '(:ignore t :wk "flow")
        "fs" '(flow-minor-suggest :wk "suggest")
        "fS" '(flow-minor-status :wk "status")
        "fc" '(flow-minor-coverage :wk "coverage")
        "fo" '(rk-flow-toggle-sealed-object :wk "toggle object seal")))

    (defun rk-flow-setup ()
      (progn
        (flow-minor-enable-automatically)
        (rk-flow-setup-bindings))))

  :init
  (setq flow-minor-use-eldoc-p nil)
  :config
  (progn
    (rk-local-leader-def :keymaps 'rk-web-js-mode-map
      "a" '(rk-flow-insert-flow-annotation :wk "insert @flow"))
    (add-hook 'rk-web-js-mode-hook #'rk-flow-setup)))

(use-package flycheck-flow
  :straight t
  :after flycheck
  :preface
  (defun rk-web--maybe-setup-flycheck-flow ()
    "Setup company-flow if buffer if applicable."
    (if (rk-in-flow-buffer-p)
        (progn
          (flycheck-add-mode 'javascript-flow 'rk-web-js-mode)
          (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
      (setq-local flycheck-disabled-checkers '(javascript-flow))))
  :config
  (progn
    (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-flycheck-flow)))

(use-package company-flow
  :straight t
  :after rk-web-modes
  :preface
  (defun rk-web--maybe-setup-company-flow ()
    "Setup company-flow if buffer if applicable."
    (if (rk-in-flow-buffer-p)
        (progn
          (setq-local company-flow-modes '(rk-web-js-mode))
          (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-flow)))
      (let ((rk-web--non-flow-backends (-remove (lambda (backend) (eq backend 'company-flow)) company-backends)))
        (setq-local company-backends rk-web--non-flow-backends))))
  :config
  (progn
    (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-company-flow)))

(use-package prettier-js
  :straight t
  :after rk-web-modes
  :preface
  (progn
    (autoload 'f-exists? "f")
    (autoload 'json-read-file "json")
    (defun rk-web--prettier-enable-p ()
      "Enable prettier if no .prettierdisable is found in project root."
      (-when-let (root (projectile-project-p))
        (not (f-exists? (f-join root ".prettierdisable")))))

    (defun rk-web--setup-prettier-local-binary-and-config ()
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

    (defun rk-web--setup-prettier ()
      (when (rk-web--prettier-enable-p)
        (progn
          (rk-local-leader-def :keymaps 'rk-web-js-mode-map
            "." '(prettier-js :wk "format"))
          (rk-web--setup-prettier-local-binary-and-config)
          (prettier-js-mode +1))))

    (defun rk-web--maybe-setup-prettier ()
      (when (and (derived-mode-p 'web-mode)
                 (-contains-p '("javascript" "jsx") web-mode-content-type))
        (rk-web--setup-prettier))))

  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-prettier))

(use-package add-node-modules-path
  :straight t
  :after rk-web-modes
  :config
  (progn
    (add-hook 'rk-web-css-mode-hook #'add-node-modules-path)
    (add-hook 'rk-web-js-mode-hook #'add-node-modules-path)))

(use-package aggressive-indent
  :defer t
  :preface
  (progn
    (autoload 'plusp "cl")
    (defun rk-web--in-flow-strict-object-type? ()
      (when (derived-mode-p 'rk-web-js-mode)
        (-let [(depth start) (syntax-ppss)]
          (and (plusp depth)
               (eq (char-after start) ?{)
               (eq (char-after (1+ start)) ?|))))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(rk-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'rk-web--in-flow-strict-object-type?)))

(use-package stylefmt
  :straight t
  :after rk-web-modes
  :commands (stylefmt-enable-on-save stylefmt-format-buffer)
  :config
  (rk-local-leader-def :keymaps 'rk-web-css-mode-map
    "." '(stylefmt-format-buffer :wk "format")))

(use-package nvm
  :straight t
  :after rk-web-modes
  :preface
  (defun rk-web--maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-use-nvm))

(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
