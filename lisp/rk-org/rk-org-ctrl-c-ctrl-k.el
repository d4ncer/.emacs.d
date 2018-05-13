;;; rk-org-ctrl-c-ctrl-k.el --- Context senstive binding for CTRL-C CTRL-K  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 's)

(autoload 'org-capture-kill "org-capture")
(autoload 'org-cut-subtree "org")
(autoload 'org-kill-note-or-show-branches "org")

(defun rk-org-ctrl-c-ctrl-k (&optional n)
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive "p")
  (cond
   ((and (boundp 'org-capture-mode) org-capture-mode)
    (org-capture-kill))
   ((s-starts-with? "*Org" (buffer-name))
    (org-kill-note-or-show-branches))
   (t
    (org-cut-subtree n))))

(provide 'rk-org-ctrl-c-ctrl-k)

;;; rk-org-ctrl-c-ctrl-k.el ends here
