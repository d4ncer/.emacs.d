;;; +edit-cmds.el --- General editing commands -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'puni-kill-line "puni")

(defun +format-after-kill ()
  (cond
   ((thing-at-point-looking-at (rx bol (* space) eol))
    (kill-line))
   ((thing-at-point-looking-at (rx bol (* space))))
   (t
    (just-one-space)))
  (indent-for-tab-command))

(defun +forward-kill-sexp ()
  (interactive)
  (kill-sexp)
  (+format-after-kill))

(defun +backward-kill-sexp ()
  (interactive)
  (backward-kill-sexp)
  (+format-after-kill))

(defun +kill-line ()
  (interactive)
  (puni-kill-line)
  (+format-after-kill))

(provide '+edit-cmds)

;;; +edit-cmds.el ends here
