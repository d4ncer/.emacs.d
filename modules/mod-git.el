;;; mod-git.el --- Git and version control tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains git and version control configuration including:
;; - transient (for magit UI)
;; - magit (git interface)
;; - git-timemachine (browse file history)
;; - browse-at-remote (open files on GitHub)
;; - vc (built-in version control)

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; Transient - Magit's UI library

(use-package transient :ensure t
  ;; Lib for showing pop-up menus of key commands, often with switches to modify
  ;; behaviour.
  ;;
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected))
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

;;; Magit - Git interface

(use-package magit :ensure t
  ;; Magit is the definitive UX for working with git.
  :config
  ;; Set initial evil state depending on whether the line is empty or not. Empty
  ;; line = new commit message, whereas non-empty means we're editing an
  ;; existing one.
  (add-hook! 'git-commit-mode-hook
    (when (and (bolp) (eolp))
      (evil-insert-state)))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

;;; Git-timemachine - File history browser

(use-package git-timemachine :ensure t
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" #'git-timemachine-show-previous-revision
   "C-n" #'git-timemachine-show-next-revision
   "gb"  #'git-timemachine-blame
   "gtc" #'git-timemachine-show-commit)

  :config
  ;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.
  (add-hook 'git-timemachine-mode-hook #'font-lock-mode)
  ;; Ensure evil keymaps are applied
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)

  ;; Show information in header-line for better visibility.
  :custom
  (git-timemachine-show-minibuffer-details t)
  :config
  (with-eval-after-load 'nano-modeline
    (defcustom nano-modeline-format-git-timemachine
      (cons '(nano-modeline-element-buffer-status
              nano-modeline-element-space
              nano-modeline-element-git-timemachine-author
              nano-modeline-element-git-timemachine-sha
              nano-modeline-element-space
              nano-modeline-element-git-timemachine-date)
            '(nano-modeline-element-window-status
              nano-modeline-element-space))
      "Modeline format for git-timemachine"
      :type 'nano-modeline-type
      :group 'nano-modeline-modes)

    (defun nano-modeline-element-git-timemachine-author ()
      "Git timemachine author"
      (when (and (boundp 'git-timemachine-show-author)
                 git-timemachine-show-author
                 (boundp 'git-timemachine-revision)
                 git-timemachine-revision)
        (propertize (concat (nth 6 git-timemachine-revision) ": ")
                    'face 'git-timemachine-minibuffer-author-face)))

    (defun nano-modeline-element-git-timemachine-sha ()
      "Git timemachine SHA or subject"
      (when (and (boundp 'git-timemachine-revision)
                 git-timemachine-revision)
        (let ((sha-or-subject (if (and (boundp 'git-timemachine-minibuffer-detail)
                                       (eq git-timemachine-minibuffer-detail 'commit))
                                  (car git-timemachine-revision)
                                (nth 5 git-timemachine-revision))))
          (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face))))

    (defun nano-modeline-element-git-timemachine-date ()
      "Git timemachine date"
      (when (and (boundp 'git-timemachine-revision)
                 git-timemachine-revision)
        (let ((date-full (nth 4 git-timemachine-revision))
              (date-relative (nth 3 git-timemachine-revision)))
          (propertize (format "[%s (%s)]" date-full date-relative)
                      'face 'nano-modeline-face-default))))

    (define-advice git-timemachine--show-minibuffer-details (:override (revision) use-nano-modeline)
      "Show revision details using nano-modeline instead of the minibuffer."
      (nano-modeline nano-modeline-format-git-timemachine))))

;;; Browse-at-remote - Open files on GitHub

(use-package browse-at-remote :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)

  :config
  (define-advice browse-at-remote--get-local-branch (:after-until () const-main)
    "Return 'main' in detached state."
    "main")

  ;; Integrate browse-at-remote with git-timemachine
  :config
  (define-advice browse-at-remote-get-url (:around (fn &rest args) git-timemachine-integration)
    "Allow `browse-at-remote' commands in git-timemachine buffers to open that
file in your browser at the visited revision."
    (if (bound-and-true-p git-timemachine-mode)
        (let* ((start-line (line-number-at-pos (min (region-beginning) (region-end))))
               (end-line (line-number-at-pos (max (region-beginning) (region-end))))
               (remote-ref (browse-at-remote--remote-ref buffer-file-name))
               (remote (car remote-ref))
               (ref (car git-timemachine-revision))
               (relname
                (file-relative-name
                 buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
               (target-repo (browse-at-remote--get-url-from-remote remote))
               (remote-type (browse-at-remote--get-remote-type target-repo))
               (repo-url (cdr target-repo))
               (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
          (unless url-formatter
            (error (format "Origin repo parsing failed: %s" repo-url)))
          (funcall url-formatter repo-url ref relname
                   (if start-line start-line)
                   (if (and end-line (not (equal start-line end-line))) end-line)))
      (apply fn args))))

;;; VC - Built-in version control

(use-package vc
  :custom
  ;; Don't prompt when following links to files that are under version control.
  (vc-follow-symlinks t)
  ;; I literally only ever use Git these days.
  (vc-handled-backends '(Git))
  :config
  (pushnew! vc-directory-exclusion-list
            "node_modules"
            "cdk.out"
            "target"
            ".direnv"))

(provide 'mod-git)
;;; mod-git.el ends here
