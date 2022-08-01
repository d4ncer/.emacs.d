;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

;; TS + LSP (WE GONNA TRY IT)

(use-package typescript-mode
  :straight t
  :after lsp
  :preface
  (defun rk-ts--add-eslint-checker ()
    (if (or (eq major-mode 'typescript-mode)
            (eq major-mode 'rk-ts-tsx-mode))
        (progn
          (flycheck-add-mode 'javascript-eslint 'typescript-mode)
          (flycheck-add-mode 'javascript-eslint 'rk-ts-tsx-mode)
          (flycheck-add-next-checker 'lsp 'javascript-eslint))))
  :config
  (add-hook 'rk-ts-tsx-mode-hook #'lsp)
  (add-hook 'lsp-diagnostics-mode-hook #'rk-ts--add-eslint-checker))

(use-package typescript-mode
  :straight t
  :after lsp
  :init
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=verbose"))
  :config
  (add-hook 'typescript-mode-hook #'lsp-deferred))

;; TS + Tide (We like this, but trying LSP)

(use-package typescript-mode
  :straight t
  :mode
  ("\\.tsx\\'" . rk-ts-tsx-mode)
  :init
  (define-derived-mode rk-ts-tsx-mode typescript-mode "TSX"
    "Derived mode for editing TSX files."))

(use-package typescript-mode
  :straight t
  :custom
  (typescript-indent-level 2)
  :mode
  ("\\.ts\\'" . typescript-mode)
  :config
  (dolist (lang '("node" "nodejs" "gjs" "rhino"))
    (setf (alist-get lang interpreter-mode-alist) 'typescript-mode)))

(use-package tide
  :straight t
  :disabled t
  :after typescript-mode
  :commands (tide-setup)
  :custom
  (tide-server-max-response-length 999999999)
  (tide-save-buffer-after-code-edit nil)
  :preface
  (defun rk-ts--setup-tide-local-binds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "gd" #'tide-jump-to-definition
     "r" #'tide-rename-symbol
     "R" #'tide-references
     "K" #'tide-documentation-at-point))
  (defun rk-ts--setup-tide-reference-local-binds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "n" #'tide-find-next-reference
     "p" #'tide-find-previous-reference
     "RET" #'tide-goto-line-reference))
  (defun rk-ts--setup-tide-error-local-binds ()
    (general-define-key
     :states 'normal
     :keymaps 'local
     "n" #'tide-find-next-error
     "p" #'tide-find-previous-error
     "RET" #'tide-goto-error))
  (defun rk-ts--switch-to-ref (&rest _)
    (-when-let* ((buffer (get-buffer "*tide-references*"))
                 (visible (get-buffer-window buffer)))
      (switch-to-buffer-other-window buffer)))
  (defun rk-ts--switch-to-errors (&rest _)
    (-if-let* ((p-buffer-name (tide-project-errors-buffer-name))
               (buffer (get-buffer p-buffer-name))
               (visible (get-buffer-window buffer)))
        (select-window visible)
      (display-buffer buffer)))
  (defun rk-ts--setup-tide ()
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (rk-ts--setup-tide-local-binds))
  :init
  (advice-add 'tide-references :after #'rk-ts--switch-to-ref)
  (advice-add 'tide-project-errors :after #'rk-ts--switch-to-errors)
  (add-hook 'tide-references-mode-hook #'rk-ts--setup-tide-reference-local-binds)
  (add-hook 'tide-project-errors-mode-hook #'rk-ts--setup-tide-error-local-binds)
  (add-hook 'typescript-mode-hook #'rk-ts--setup-tide)
  (add-hook 'rk-ts-tsx-mode-hook #'rk-ts--setup-tide)
  (rk-local-leader-def :keymaps '(typescript-mode-map rk-ts-tsx-mode-map)
    "x" '(tide-restart-server :wk "restart server")
    "f" '(tide-fix :wk "fix")
    "r" '(tide-rename-file :wk "rename file")
    "i" '(tide-organize-imports :wk "organize imports")
    "R" '(tide-refactor :wk "refactor")
    "e" '(tide-project-errors :wk "errors")
    "j" '(tide-jsdoc-template :wk "insert jsdoc")
    "v" '(tide-verify-setup :wk "verify setup")))

(use-package tide
  :straight t
  :disabled t
  :after (flycheck typescript-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'rk-ts-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-mode 'typescript-tide 'rk-ts-tsx-mode))

(use-package prettier
  :straight t
  :after typescript-mode
  :init
  (add-hook 'rk-ts-tsx-mode-hook #'prettier-mode)
  (add-hook 'typescript-mode-hook #'prettier-mode)
  (rk-local-leader-def :keymaps '(typescript-mode-map rk-ts-tsx-mode-map)
    "." '(prettier-prettify :wk "format")))

(use-package tree-sitter-langs
  :straight t
  :after typescript-mode
  :init
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(rk-ts-tsx-mode . tsx)))

(use-package emmet-mode
  :straight t
  :after typescript-mode
  :general
  (:keymaps 'emmet-mode-keymap :states '(normal insert)
            "C-'" #'emmet-expand-line)
  :custom
  (emmet-move-cursor-between-quotes t)
  :init
  (add-hook 'rk-ts-tsx-mode-hook #'emmet-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'rk-ts-tsx-mode))

;; LSP + Tailwind (disabled for now)

(use-package lsp-tailwindcss
  :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :after typescript-mode
  :disabled t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes '(rk-ts-tsx-mode)))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
