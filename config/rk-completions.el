;;; rk-completions.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'definers)
(require 'general)

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :general
  (:states '(insert normal motion)
           "C-;" #'completion-at-point)
  :init
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :general (:keymaps 'vertico-map
                     "RET" #'vertico-directory-enter
                     "DEL" #'vertico-directory-delete-char
                     "C-h" #'vertico-directory-delete-word)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :defines (vertico-repeat-save)
  :init
  (rk-leader-def
    "r" '(vertico-repeat-last :wk "resume"))
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
  :init
  (marginalia-mode))

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

;; Corfu

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  :general
  (:keymaps 'corfu-map
            "C-<return>" #'corfu-insert
            "C-j" #'corfu-next
            "C-k" #'corfu-previous
            "C-h" #'corfu-show-documentation)
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :straight '(corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after (corfu)
  :config
  (add-hook 'corfu-mode-hook #'corfu-doc-mode))

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
