;;; rk-neotree-cmds.el --- Commands for working with neotree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'neotree)

(autoload 'projectile-project-root "projectile")

(defvar rk-neotree-cmds--auto-indent-point t)

(defun rk-neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when rk-neotree-cmds--auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun rk-neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when rk-neotree-cmds--auto-indent-point
        (neo-point-auto-indent)))))

(defun rk-neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (rk-neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun rk-neotree-find-project-root ()
  "Go to the root of the current project in neotree."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(provide 'rk-neotree-cmds)

;;; rk-neotree-cmds.el ends here
