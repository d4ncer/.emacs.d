;; -*- lexical-binding: t; -*-

(defun +elisp-eval-dwim (&optional beg end)
  "Perform a context-sensitive Elisp eval action.

Evaluate either the current region, from BEG to END, or the defun (or
top-level-form) at point."
  (interactive (when (region-active-p)
                 (list (region-beginning) (region-end))))
  (if (and beg end)
      (message "Eval region => %s" (eval-region beg end))
    (message "Eval defun => %s" (eval-defun nil))))

(provide '+elisp)
