;;; rk-tree-sitter.el --- Tree sitter config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; (use-package expand-region
;;   :straight t
;;   :after tree-sitter
;;   :preface
;;   (defun rk-er/mark-outer-tree-sitter-node ()
;;     (interactive)
;;     (let* ((p (point))
;;            (m (or (mark) p))
;;            (beg (min p m))
;;            (end (max p m))
;;            (root (tsc-root-node tree-sitter-tree))
;;            (node (tsc-get-descendant-for-position-range root beg end))
;;            (node-beg (tsc-node-start-position node))
;;            (node-end (tsc-node-end-position node)))
;;       ;; Node fits the region exactly. Try its parent node instead.
;;       (when (and (= beg node-beg) (= end node-end))
;;         (when-let ((node (tsc-get-parent node)))
;;           (setq node-beg (tsc-node-start-position node)
;;                 node-end (tsc-node-end-position node))))
;;       (set-mark node-end)
;;       (goto-char node-beg)))
;;   (defun rk-tree-sitter--add-tree-sitter-expansions ()
;;     (make-variable-buffer-local 'er/try-expand-list)
;;     (setq er/try-expand-list (append er/try-expand-list '(rk-er/mark-outer-tree-sitter-node))))
;;   :config
;;   (add-hook 'tree-sitter-mode-hook #'rk-tree-sitter--add-tree-sitter-expansions))

(provide 'rk-tree-sitter)

;;; rk-tree-sitter.el ends here
