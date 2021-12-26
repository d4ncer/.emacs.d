;;; rk-tree-sitter.el --- Tree sitter config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tree-sitter
  :custom
  ;; Favour compilation rather than pulling binaries
  ;; from github until tree-sitter puts up m1 binaries.
  (tsc-dyn-get-from '(:compilation))
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query)
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode))

(use-package expand-region
  :straight t
  :after tree-sitter
  :preface
  (defun rk-er/mark-outer-tree-sitter-node ()
    (interactive)
    (let* ((p (point))
           (m (or (mark) p))
           (beg (min p m))
           (end (max p m))
           (root (tsc-root-node tree-sitter-tree))
           (node (tsc-get-descendant-for-position-range root beg end))
           (node-beg (tsc-node-start-position node))
           (node-end (tsc-node-end-position node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= beg node-beg) (= end node-end))
        (when-let ((node (tsc-get-parent node)))
          (setq node-beg (tsc-node-start-position node)
                node-end (tsc-node-end-position node))))
      (set-mark node-end)
      (goto-char node-beg)))
  (defun rk-tree-sitter--add-tree-sitter-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append er/try-expand-list '(rk-er/mark-outer-tree-sitter-node))))
  :config
  (add-hook 'tree-sitter-mode-hook #'rk-tree-sitter--add-tree-sitter-expansions))

(provide 'rk-tree-sitter)

;;; rk-tree-sitter.el ends here
