;;; rk-completions.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'definers)
  (require 'general))

(use-package selectrum
  :straight t
  :preface
  ;; https://github.com/raxod502/selectrum/issues/498#issuecomment-876424791
  (defun rk-comp--backward-kill-sexp (&optional arg)
    "Selectrum wrapper for `backward-kill-sexp' without saving to kill-ring.
ARG is the same as for `backward-kill-sexp'."
    (interactive "p")
    ;; For Selectrum file prompts `backward-kill-sexp' would wrongly
    ;; include trailing (read only) spaces as part of the input (see
    ;; `selectrum--minibuffer-local-filename-syntax').
    (save-restriction
      (narrow-to-region (minibuffer-prompt-end) (point-max))
      (let ((opoint (point)))
        (forward-sexp (- (or arg 1)))
        (delete-region opoint (point)))))
  :general (:keymaps 'selectrum-minibuffer-map
                     "C-j" #'next-line-or-history-element
                     "C-k" #'previous-line-or-history-element
                     "C-<return>" #'selectrum-submit-exact-input
                     "C-h" #'rk-comp--backward-kill-sexp)
  :init
  (rk-leader-def
    "r" '(selectrum-repeat :wk "resume")))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :straight t
  :general
  (:states '(motion normal)
           "/" #'consult-line)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key (kbd ">"))
  :preface
  (defun rk-comp--consult-line-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun rk-comp--consult-rg-at-point ()
    (interactive)
    (consult-ripgrep (or (projectile-project-root) default-directory) (thing-at-point 'symbol)))
  (defun rk-comp--consult-rg-at-point-custom-dir (dir input)
    (interactive (list (read-directory-name "Start from directory: ")
                       (or (thing-at-point 'symbol) "")))
    (consult-ripgrep dir input))
  :init
  (rk-leader-def
    "/"   '(consult-ripgrep :wk "search (project)")

    "b s" '(consult-buffer :wk "switch to buffer")

    "f r" '(consult-recent-file :wk "recent files")

    "k r" '(consult-yank-from-kill-ring :wk "kill ring")
    "k R" '(consult-yank-replace :wk "replace from kr")

    "s S" '(rk-comp--consult-line-at-point :wk "search in buffer")
    "s P" '(rk-comp--consult-rg-at-point :wk "search in project")
    "s F" '(rk-comp--consult-rg-at-point-custom-dir :wk "search from dir"))
  :config
  (set-face-attribute 'consult-file nil :inherit nil :weight 'semi-bold)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-.")))

(use-package marginalia
  :straight t
  :general (:keymaps 'minibuffer-local-map
                     "C-0" #'marginalia-cycle)
  :preface
  (defun rk-comp--flush-selectrum ()
    (when (bound-and-true-p selectrum-mode)
      (selectrum-exhibit)))
  :init
  (marginalia-mode)
  :config
  (dolist (face '(marginalia-key
                  marginalia-number
                  marginalia-file-owner
                  marginalia-file-priv-read
                  marginalia-file-priv-write
                  marginalia-file-priv-exec
                  marginalia-file-priv-link
                  marginalia-file-priv-dir
                  marginalia-file-priv-no
                  marginalia-file-priv-other
                  marginalia-file-priv-rare))
    (set-face-attribute face nil :inherit nil :weight 'light))
  (advice-add #'marginalia-cycle :after #'rk-comp--flush-selectrum))

(use-package embark
  :straight t
  :general
  ("C-." #'embark-act
   "C-;" #'embark-dwim)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :straight t
  :after org
  :general
  (:keymaps 'org-mode-map :states '(normal visual motion)
            "?" #'consult-org-heading))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'rk-completions)

;;; rk-completions.el ends here
