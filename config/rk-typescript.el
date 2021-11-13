;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)
(require 'rk-web-modes)
;; (require 'lsp)

(use-package typescript-mode
  :straight t
  :custom
  (typescript-indent-level 2)
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (dolist (lang '("node" "nodejs" "gjs" "rhino"))
    (setf (alist-get lang interpreter-mode-alist) 'typescript-mode)))
;; :hook (typescript-mode . lsp)
;; :init
;; (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=verbose")))

(use-package tide
  :straight t
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
     "K" #'tide-documentation-at-point)
    (general-define-key
     :states 'insert
     :keymaps 'local
     "C-." #'company-complete))
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
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (rk-ts--setup-tide-local-binds))

  :init
  (add-hook 'typescript-mode-hook #'rk-ts--setup-tide)
  (advice-add 'tide-references :after #'rk-ts--switch-to-ref)
  (advice-add 'tide-project-errors :after #'rk-ts--switch-to-errors)
  (add-hook 'tide-references-mode-hook #'rk-ts--setup-tide-reference-local-binds)
  (add-hook 'tide-project-errors-mode-hook #'rk-ts--setup-tide-error-local-binds)
  (with-eval-after-load 'rk-web-modes
    (add-hook 'rk-web-tsx-mode-hook #'rk-ts--setup-tide))

  :config
  (with-eval-after-load 'flycheck
    (with-eval-after-load 'rk-web-modes
      (flycheck-add-mode 'typescript-tide 'rk-web-tsx-mode)))
  (with-eval-after-load 'rk-web-modes
    (rk-local-leader-def :keymaps '(typescript-mode-map rk-web-tsx-mode-map)
      "x" '(tide-restart-server :wk "restart server")
      "f" '(tide-fix :wk "fix")
      "r" '(tide-rename-file :wk "rename file")
      "i" '(tide-organize-imports :wk "organize imports")
      "R" '(tide-refactor :wk "refactor")
      "e" '(tide-project-errors :wk "errors")
      "j" '(tide-jsdoc-template :wk "insert jsdoc")
      "v" '(tide-verify-setup :wk "verify setup"))))

(use-package add-node-modules-path
  :straight t
  :after typescript-mode
  :init
  (add-hook 'typescript-mode #'add-node-modules-path))

(use-package prettier
  :straight t
  :after typescript-mode
  :init
  (add-hook 'typescript-mode-hook #'prettier-mode))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
