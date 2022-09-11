;;; rk-completions.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'definers)
(require 'general)

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
  :straight (:files (:defaults))
  :general
  (:states '(motion normal)
           "/" #'consult-line)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key (kbd ">"))
  :hook (completion-list-mode . consult-preview-at-point-mode)
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
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
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
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
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
  (advice-add #'marginalia-cycle :after #'rk-comp--flush-selectrum))

(use-package embark
  :straight t
  :general
  ("C-." #'embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :straight t
  :after (org vulpea)
  :preface
  (defun rk-org-roam--search-projects ()
    (interactive)
    (let ((org-agenda-files (vulpea-project-files)))
      (call-interactively 'consult-org-agenda)))
  :general
  (:keymaps 'org-mode-map :states '(normal visual motion)
            "?" #'consult-org-heading))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Company / Corfu

(use-package company
  :straight t
  :hook (after-init . global-company-mode)

  :general
  (:keymaps 'company-mode-map :states '(insert)
            "C-;" #'company-complete)
  (:keymaps 'company-active-map
            "C-w" nil
            "C-j" #'company-select-next-or-abort
            "C-k" #'company-select-previous-or-abort
            "<tab>" #'company-complete-selection)
  (:keymaps 'company-search-map
            "C-w" nil
            "C-j" #'company-select-next-or-abort
            "C-k" #'company-select-previous-or-abort
            "<tab>" #'company-complete-selection)

  :commands (company-select-next-or-abort
             company-select-previous-or-abort
             company-show-doc-buffer)
  :custom
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.2)
  (company-require-match nil))

(use-package company-dabbrev
  :after company
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))

;; (use-package company-box
;;   :straight t
;;   :custom
;;   (company-box-scrollbar nil)
;;   :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :straight t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-quit-no-match t)
;;   (corfu-quit-at-boundary t)
;;   :preface
;;   (autoload 'company-show-doc-buffer "company")
;;   :general
;;   (:keymaps 'corfu-map
;;             "C-<return>" #'corfu-insert
;;             "C-j" #'corfu-next
;;             "C-k" #'corfu-previous
;;             "C-h" #'corfu-show-documentation)
;;   :init
;;   (global-corfu-mode))

;; (use-package corfu-doc
;;   :straight '(corfu-doc :type git :host github :repo "galeo/corfu-doc")
;;   :after (corfu)
;;   :config
;;   (add-hook 'corfu-mode-hook #'corfu-doc-mode))

(use-package emacs
  :general
  (:keymaps 'minibuffer-mode-map
            "S-<backspace>" #'backward-kill-sentence)
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package bookmark-view
  :straight t
  :init
  (rk-leader-def
    "w s" '(bookmark-view :wk "save view")))

(provide 'rk-completions)

;;; rk-completions.el ends here
