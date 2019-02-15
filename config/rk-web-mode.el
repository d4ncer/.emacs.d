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

(autoload 'flow-minor-tag-present-p "flow-minor-mode")
(autoload 'flow-minor-configured-p "flow-minor-mode")
(autoload 'flow-minor-binary "flow-minor-mode")

(autoload 'projectile-project-p "projectile")

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

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)))

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
  :preface
  (progn
    (defun rk-web--maybe-setup-flow-lsp ()
      (when (rk-web--flow-lsp-p)
        (progn
          (setq-local lsp-clients-flow-server (flow-minor-binary))
          (lsp))))

    (defun rk-web--add-custom-eslint-rules-dir ()
      (-when-let* ((root (projectile-project-p))
                   (rules-dir (f-join root "rules"))
                   (rules-dir-p (f-exists-p rules-dir)))
        (setq-local flycheck-eslint-rules-directories (-list rules-dir)))))

  :config
  (progn
    ;; Add all possible JS runtimes

    (dolist (name (list "node" "nodejs" "gjs" "rhino"))
      (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'rk-web-js-mode)))

    ;; Use custom ESLint rules if a "rules" dir exists in project root

    (add-hook 'rk-web-js-mode-hook #'rk-web--add-custom-eslint-rules-dir)

    ;; Set up LSP for Flow buffers
    (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-flow-lsp)

    ;; Setup post flycheck load

    (with-eval-after-load 'flycheck
      (setq flycheck-html-tidy-executable (locate-file "tidy" exec-path))
      (flycheck-add-mode 'javascript-eslint 'rk-web-js-mode)
      (flycheck-add-mode 'css-csslint 'rk-web-css-mode)
      (flycheck-add-mode 'json-jsonlint 'rk-web-json-mode)
      (flycheck-add-mode 'html-tidy 'rk-web-html-mode))))

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
  ;; Rely on LSP bindings for moving around / help at point
  ;; TODO: Ensure these are enabled for any buffers that don't have LSP capabilities
  ;; :general
  ;; (:keymaps 'flow-minor-mode-map :states 'normal
  ;;           "gd" #'flow-minor-jump-to-definition
  ;;           "K"  #'flow-minor-type-at-pos)
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

;; Can rely on Flow to do these
;; TODO: Ensure these are enabled for any buffers that don't have LSP capabilities

;; (use-package flycheck-flow
;;   :straight t
;;   :after (flycheck rk-web-modes)
;;   :preface
;;   (defun rk-web--maybe-setup-flycheck-flow ()
;;     "Setup company-flow if buffer if applicable."
;;     (if (rk-in-flow-buffer-p)
;;         (progn
;;           (flycheck-add-mode 'javascript-flow 'rk-web-js-mode)
;;           (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
;;       (setq-local flycheck-disabled-checkers '(javascript-flow))))
;;   :config
;;   (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-flycheck-flow))

;; (use-package company-flow
;;   :straight t
;;   :after rk-web-modes
;;   :preface
;;   (defun rk-web--maybe-setup-company-flow ()
;;     "Setup company-flow if buffer if applicable."
;;     (if (rk-in-flow-buffer-p)
;;         (progn
;;           (setq-local company-flow-modes '(rk-web-js-mode))
;;           (with-eval-after-load 'company
;;             (add-to-list 'company-backends 'company-flow)))
;;       (let ((rk-web--non-flow-backends (-remove (lambda (backend) (eq backend 'company-flow)) company-backends)))
;;         (setq-local company-backends rk-web--non-flow-backends))))
;;   :config
;;   (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-setup-company-flow))

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
          (rk-web--init-prettier-config)
          (prettier-js-mode +1))))

    (defun rk-web--maybe-setup-prettier ()
      (when (and (derived-mode-p 'web-mode)
                 (-contains-p '("javascript" "jsx") web-mode-content-type))
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
    (when (and
           (f-exists-p (locate-dominating-file default-directory ".nvmrc"))
           (locate-file "fnm" exec-path))
      (fnm-use-for-buffer)))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-use-fnm))

(use-package nvm
  :straight t
  :after rk-web-modes
  :preface
  (defun rk-web--maybe-use-nvm ()
    (when (and
           (f-exists-p (locate-dominating-file default-directory ".nvmrc"))
           (locate-file "nvm" exec-path))
      (nvm-use-for-buffer)))
  :config
  (add-hook 'rk-web-js-mode-hook #'rk-web--maybe-use-nvm))

(provide 'rk-web-mode)

;;; rk-web-mode.el ends here
