;;; mod-dired.el --- Dired file manager configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains dired and file management configuration including:
;; - dired (built-in file manager)
;; - dired-aux (auxiliary dired functions)
;; - wdired (editable dired buffers)
;; - nerd-icons and nerd-icons-dired (icons for dired)
;; - dirvish (enhanced dired wrapper)
;; - diredfl (extra font-locking for dired)

;;; Code:

;;; Dired - File manager

(use-package dired
  ;; Emacs' built-in file management interface.
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . hl-line-mode)
  :custom
  (dired-garbage-files-regexp (rx (or ".log" ".toc" ".dvi" ".bak" ".orig" ".rej" ".aux" ".DS_Store")
                                  eos))
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-listing-switches
   "--almost-all --human-readable --group-directories-first --no-group"))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package nerd-icons :ensure t
  ;; Icon set used by various packages.
  :autoload nerd-icons-codicon nerd-icons-faicon)

(use-package nerd-icons-dired :ensure t
  :disabled t
  ;; Show icons in dired.
  :hook dired-mode-hook)

(use-package wdired
  ;; Makes dired buffers directly editable; the changes are interpreted into
  ;; operations on the corresponding file names.
  :general
  (:keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package dirvish :ensure t
  :disabled t
  ;; Wrapper around `dired' that provides better UX.
  :hook (+first-input-hook . dirvish-override-dired-mode)

  :general
  (:keymaps '(dirvish-mode-map dired-mode-map) :states 'normal
            "q" #'dirvish-quit)
  (:keymaps 'dirvish-mode-map :states 'normal
            "<tab>" #'dirvish-layout-toggle)
  :custom
  (dirvish-reuse-session nil)
  (dirvish-attributes '(file-size subtree-state))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-subtree-always-show-state t)
  (dirvish-hide-details '(dirvish dirvish-side))
  (dirvish-hide-cursor '(dirvish dirvish-side))

  :config
  (when (featurep 'nerd-icons)
    (setq dirvish-path-separators (list
                                   (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                   (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                   (format " %s " (nerd-icons-faicon "nf-fa-angle_right")))))


  :config
  (dirvish-peek-mode +1))

(use-package diredfl :ensure t)

;;; Dired modeline

(with-eval-after-load 'doom-modeline
  (defun +modeline-dired-directory ()
    "Return abbreviated directory path for dired buffers."
    (abbreviate-file-name default-directory))

  (defun +modeline-dired-file-count ()
    "Return the number of files in the current dired buffer."
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (dired-next-line 1)
          (cl-incf count)))
      (format "%d items" count)))

  (defun +modeline-dired-free-space ()
    "Return free disk space for the current directory."
    (when-let* ((attr (file-system-info default-directory))
                (free (nth 2 attr))
                (gb (/ free 1073741824.0)))
      (format "%.1f GB free" gb)))

  (doom-modeline-def-segment +dired-info
    "Dired directory, file count, and free space."
    (concat
     (propertize (+modeline-dired-directory)
                 'face (doom-modeline-face 'doom-modeline-buffer-file))
     (doom-modeline-spc)
     (propertize (+modeline-dired-file-count)
                 'face (doom-modeline-face 'doom-modeline-info))
     (when-let* ((space (+modeline-dired-free-space)))
       (concat (doom-modeline-spc)
               (propertize space 'face (doom-modeline-face 'doom-modeline-buffer-minor-mode))))))

  (doom-modeline-def-modeline '+dired
    '(+dired-info)
    '(misc-info))

  (add-to-list 'doom-modeline-mode-alist '(dired-mode . +dired)))

(provide 'mod-dired)
;;; mod-dired.el ends here
