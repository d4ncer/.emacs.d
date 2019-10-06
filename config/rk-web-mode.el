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

(autoload 'flow-minor-tag-present-p "flow-minor-mode")
(autoload 'flow-minor-configured-p "flow-minor-mode")
(autoload 'flow-minor-binary "flow-minor-mode")

(autoload 'projectile-project-p "projectile")

(defconst rk-web--node-js-lts-version "v10.15.1"
  "The version of Node to use by default if .nvmrc isn't found.")

(defconst rk-web--prettier-default-args
  (list "--single-quote" "true" "--trailing-comma" "es5")
  "Default values for prettier.")

(defvar rk-web--flow-lsp-buffer-cache (ht-create)
  "A cache to store if a previously visited buffer has Flow LSP capabilities.")

(defun rk-in-flow-buffer-p ()
  "Check if the buffer is a valid Flow buffer."
  (and (eq major-mode 'rk-web-js-mode)
       (flow-minor-configured-p)
       (flow-minor-tag-present-p)))

;; TODO: This is unused due to how slow everything gets.
(defun rk-web--setup-flow-lsp-flycheck ()
  "Setup Flycheck checkers for LSP + Flow."
  (when lsp-mode
    (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))

(defun rk-web--setup-flow-lsp ()
  "Setup Flow with the LSP."
  (progn
    (add-to-list 'lsp-language-id-configuration '(rk-web-js-mode . "flow"))
    (setq-local lsp-clients-flow-server (flow-minor-binary))
    (lsp)))

(defun rk-web--setup-flow-sans-lsp ()
  "Setup Flow with `flow-minor-mode', `company-flow', and `flycheck-flow'."
  (progn
    (with-eval-after-load 'flow-minor-mode
      (general-def
        :keymaps 'flow-minor-mode-map
        :states 'normal
        "gd" #'flow-minor-jump-to-definition
        "K" #'flow-minor-type-at-pos))))

(defun rk-web--setup-flow ()
  "Setup Flow either via the LSP or normally."
  (if (rk-web--flow-lsp-p)
      (rk-web--setup-flow-lsp)
    (rk-web--setup-flow-sans-lsp)))

(defun rk-web--flow-lsp-p ()
  "Check if the Flow version used by the buffer has LSP capabilities."
  (when (rk-in-flow-buffer-p)
    (-if-let* ((fname (buffer-file-name))
               (cache-hit (ht-contains-p rk-web--flow-lsp-buffer-cache fname)))
        (ht-get rk-web--flow-lsp-buffer-cache fname)
      (-if-let* ((flow-version-cmd (s-concat (flow-minor-binary) " version --json"))
                 (json-output (ignore-errors (json-read-from-string (shell-command-to-string flow-version-cmd))))
                 (semver (assoc 'semver json-output))
                 (flow-version (cdr semver))
                 (flow-major-version (nth 1 (s-split "\\." flow-version)))
                 (flow-major-version-int (string-to-number flow-major-version))
                 (flow-lsp-capable-p (> flow-major-version-int 100)))
          (progn
            (ht-set! rk-web--flow-lsp-buffer-cache fname flow-lsp-capable-p)
            flow-lsp-capable-p)
        nil))))

(defun rk-web--flow-clear-buffer-cache ()
  "Clear Flow buffer cache."
  (interactive)
  (message "Clearing Flow buffer cache...")
  (ht-clear! rk-web--flow-lsp-buffer-cache))

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
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)))

(use-package jsonnet-mode
  :straight t
  :preface
  (defun rk-jsonnet--disable-hideshow ()
    (autoload 'hs-minor-mode "hideshow")
    (remove-hook 'prog-mode-hook #'hs-minor-mode t))
  :config
  (add-hook 'jsonnet-mode-hook #'rk-jsonnet--disable-hideshow)
  (rk-local-leader-def :keymaps 'jsonnet-mode-map
    "." '(jsonnet-reformat-buffer :wk "format")
    "b" '(jsonnet-eval :wk "eval buffer")))

(use-package rk-web-modes
  :defer t
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
  (progn
    (general-def :keymaps 'rk-web-js-mode-map :states 'normal
      "J" #'rk-utils--chainable-aware-join-line)

    ;; Add all possible JS runtimes

    (dolist (name (list "node" "nodejs" "gjs" "rhino"))
      (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'rk-web-js-mode)))

    ;; Use custom ESLint rules if a "rules" dir exists in project root

    (add-hook 'rk-web-js-mode-hook #'rk-web--add-custom-eslint-rules-dir)

    ;; Set up LSP for Flow buffers
    (add-hook 'rk-web-js-mode-hook #'rk-web--setup-flow)

    ;; Setup post flycheck load

    (with-eval-after-load 'flycheck
      (setq flycheck-html-tidy-executable (locate-file "tidy" exec-path))
      (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
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

       ((and (derived-mode-p 'rk-web-js-mode)
             (rk-web--react-in-buffer-p))
        (progn
          (setq-local emmet-expand-jsx-className? t)
          (emmet-mode +1))))))

  :init
  (progn
    (setq emmet-move-cursor-between-quotes t)
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

;; Flow

(use-package flow-minor-mode
  :demand t
  :straight (:host github :repo "d4ncer/flow-minor-mode"
                   :branch "master")
  :after rk-web-modes
  :preface
  (progn
    (autoload 's-matches? "s")
    (autoload 'sp-get-enclosing-sexp "smartparens")

    (defun rk-flow--toggle-sealed-object ()
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

    (defun rk-flow--insert-flow-annotation ()
      "Insert a flow annotation at the start of this file."
      (interactive)
      (unless (not (rk-in-flow-buffer-p))
        (user-error "Buffer already contains an @flow annotation"))
      (save-excursion
        (goto-char (point-min))
        (insert "// @flow\n")
        (message "Inserted @flow annotation."))))

  :init
  (setq flow-minor-use-eldoc-p nil)
  :config
  (progn
    (rk-local-leader-def :keymaps 'rk-web-js-mode-map
      "a" '(rk-flow--insert-flow-annotation :wk "insert @flow"))
    (add-hook 'rk-web-js-mode-hook #'flow-minor-enable-automatically)))

;; Fallbacks for non-LSP enabled Flow buffers

(use-package flycheck-flow
  :straight t
  :after (flycheck rk-web-modes)
  :preface
  (defun rk-web--maybe-setup-flycheck-flow ()
    "Setup company-flow if buffer if applicable."
    (if (not (rk-web--flow-lsp-p))
        (progn
          (flycheck-add-mode 'javascript-flow 'rk-web-js-mode)
          (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
      (setq-local flycheck-disabled-checkers '(javascript-flow))))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-flycheck-flow))

(use-package company-flow
  :straight t
  :after rk-web-modes
  :preface
  (defun rk-web--maybe-setup-company-flow ()
    "Setup company-flow if buffer if applicable."
    (if (not (rk-web--flow-lsp-p))
        (progn
          (setq-local company-flow-modes '(rk-web-js-mode))
          (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-flow)))
      (let ((rk-web--non-flow-backends (-remove (lambda (backend) (eq backend 'company-flow)) company-backends)))
        (setq-local company-backends rk-web--non-flow-backends))))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-company-flow))

(use-package prettier-js
  :straight t
  :after rk-web-modes
  :preface
  (progn
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
          (rk-local-leader-def :keymaps 'rk-web-js-mode-map
            "." '(prettier-js :wk "format"))
          (rk-local-leader-def :keymaps 'rk-web-html-mode-map
            "." '(prettier-js :wk "format"))
          (rk-local-leader-def :keymaps 'rk-web-css-mode-map
            "." '(prettier-js :wk "format"))
          (rk-web--init-prettier-config)
          (prettier-js-mode +1))))

    (defun rk-web--maybe-setup-prettier ()
      (when (and (derived-mode-p 'web-mode)
                 (-contains-p '("javascript" "jsx" "html" "css") web-mode-content-type))
        (rk-web--init-prettier))))

  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-prettier))

(use-package add-node-modules-path
  :straight t
  :after rk-web-modes
  :config
  (progn
    (add-hook 'rk-web-css-mode-hook #'add-node-modules-path)
    (add-hook 'rk-web-js-mode-hook #'add-node-modules-path)))

(use-package stylefmt
  :straight t
  :after rk-web-modes
  :commands (stylefmt-enable-on-save stylefmt-format-buffer)
  :config
  (rk-local-leader-def :keymaps 'rk-web-css-mode-map
    "." '(stylefmt-format-buffer :wk "format")))

(use-package fnm
  :after rk-web-modes
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
