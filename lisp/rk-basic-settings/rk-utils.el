;;; rk-utils.el --- General utility fns  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'evil-join "evil-commands")

(defun rk-utils--chainable-aware-join-line ()
  "Join lines, deleting intermediate spaces for chained function calls."
  (interactive)
  (call-interactively #'evil-join)
  (when (thing-at-point-looking-at (rx (not space) (* space) "."))
    (delete-horizontal-space)))

(provide 'rk-utils)

;;; rk-utils.el ends here
