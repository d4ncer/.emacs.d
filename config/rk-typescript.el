;;; rk-typescript.el --- TypeScript config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(use-package typescript-mode
  :straight t
  :custom
  (typescript-indent-level 2)
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package tide
  :straight t
  :commands (tide-setup)
  :after typescript-mode
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
  (defun rk-ts--switch-to-ref (&rest _)
    (-when-let* ((buffer (get-buffer "*tide-references*"))
                 (visible (get-buffer-window buffer)))
      (switch-to-buffer-other-window buffer)))
  (defun rk-ts--setup-tide ()
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (rk-ts--setup-tide-local-binds))
  :init
  (add-hook 'typescript-mode-hook #'rk-ts--setup-tide)
  (advice-add 'tide-references :after #'rk-ts--switch-to-ref)
  (add-hook 'tide-references-mode-hook #'rk-ts--setup-tide-reference-local-binds)
  :config
  (rk-local-leader-def :keymaps 'typescript-mode-map
    "x" '(tide-restart-server :wk "restart server")
    "f" '(tide-fix :wk "fix")
    "r" '(tide-rename-file :wk "rename file")
    "i" '(tide-organize-imports :wk "organize imports")
    "e" '(tide-refactor :wk "refactor")
    "j" '(tide-jsdoc-template :wk "insert jsdoc")
    "v" '(tide-verify-setup :wk "verify setup")))

(provide 'rk-typescript)

;;; rk-typescript.el ends here
