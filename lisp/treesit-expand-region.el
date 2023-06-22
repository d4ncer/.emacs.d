;;; treesit-expand-region.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun er/treesit-er--get-node-between (a b)
  "Find the node that sits above any node in the region (A B)."
  (let* ((start (min a b))
         (end (max a b)))
    (treesit-parent-until
     (treesit-node-at start)
     (lambda (node) (< end (treesit-node-end node))))))

(defun er/treesit-er-parent-node ()
  "Expand to the node above point, or to the node above the active region."
  (interactive)
  (let ((node
         (if (region-active-p)
             (er/treesit-er--get-node-between (mark) (point))
           (treesit-node-at (point)))))
    (goto-char (treesit-node-start node))
    (set-mark (treesit-node-end node))
    (activate-mark)))

(defun rk-er/add-treesit-expander ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append er/try-expand-list '(er/treesit-er-parent-node))))

(provide 'treesit-expand-region)

;;; treesit-expand-region.el ends here
