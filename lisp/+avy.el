;;; +avy.el --- Supporting functions for avy config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +avy-action-change-move (pt)
  "Delete the thing at PT and enter insert state."
  (goto-char pt)
  (avy-forward-item)
  (kill-region pt (point))
  (evil-insert-state)
  (point))

(defun +avy-action-evil-lookup (pt)
  "Look up the definition of thing at PT with evil."
  (save-excursion
    (goto-char pt)
    (avy-forward-item)
    (evil-lookup))
  t)


(defmacro +with-clean-up-in-starting-buffer-and-window (form &rest cleanup-forms)
  (declare (indent 1))
  (let ((buf (gensym "buf"))
        (win (gensym "buf")))
    `(let ((,buf (current-buffer))
           (,win (selected-window)))
       ,form
       (with-selected-window ,win
         (with-current-buffer ,buf
           (save-excursion
             ,@cleanup-forms))))))

(provide '+avy)

;;; +avy.el ends here
