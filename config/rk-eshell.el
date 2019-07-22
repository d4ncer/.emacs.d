;;; rk-eshell.el --- <enter description here>  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'f)
(require 'general)
(require 'fnm)
(require 'paths)

(autoload 'evil-local-set-key "evil-core")
(autoload 'evil-set-initial-state "evil-core")
(autoload 'page-break-lines-mode "page-break-lines")

;; NOTE: Load just this feature, instead of all of magit.
(autoload 'magit-get-current-branch "magit-git")
(autoload 'magit-process-file "magit-process")

(autoload 'display-buffer-fullframe "display-buffer-fullframe")
(autoload 'page-break-lines--update-display-table "page-break-lines")

(defconst rk-eshell--etc-dir (f-join paths-etc-directory "eshell"))

(use-package eshell
  :commands (eshell)

  :preface
  (progn
    ;; HACK eshell mode map is set as a local variable in its mode function.
    ;; deep cry. ( -̩̩̩͡˛ -̩̩̩͡ )
    (defun rk-eshell--setup-keybindings ()
      (evil-local-set-key 'insert (kbd "C-e") 'end-of-line)
      (evil-local-set-key 'insert (kbd "C-a") 'eshell-bol)))

  :init
  (evil-set-initial-state 'eshell-mode 'emacs)
  :config
  (progn
    (require 'eshell-hacks)
    (general-setq eshell-modules-list
                  '(eshell-tramp
                    eshell-alias
                    eshell-banner
                    eshell-basic
                    eshell-cmpl
                    eshell-dirs
                    eshell-glob
                    eshell-hist
                    eshell-ls
                    eshell-pred
                    eshell-prompt
                    eshell-script
                    eshell-term
                    eshell-unix))

    (add-hook 'eshell-mode-hook #'rk-eshell--setup-keybindings)

    ;; keep aliases under etc directory, which is tracked by git.

    (f-mkdir rk-eshell--etc-dir)
    (general-setq eshell-aliases-file (f-join rk-eshell--etc-dir  "aliases"))))

(use-package fasd
  :straight
  (:type git :repo "https://framagit.org/steckerhalter/emacs-fasd.git")
  :when (executable-find "fasd")
  :config
  (progn
    (apply #'start-process "*fasd*" nil "fasd" "--add" (seq-map #'shell-quote-argument recentf-list))
    (global-fasd-mode +1)))

(use-package pretty-eshell
  :after eshell
  :preface
  (defface eshell-dimmed
    '((t :inherit default))
    "Face for dimmed text in eshell."
    :group 'rk-eshell)

  (defface eshell-timestamp
    '((t :inherit eshell-dimmed :height 0.7))
    "Face for timestamps in eshell."
    :group 'rk-eshell)
  (defun config-eshell--dim-commands-on-submission ()
    (let ((start eshell-last-output-start)
          (end (line-end-position)))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char start)
          (search-forward "\u000c")
          (put-text-property (point) end 'face 'eshell-dimmed)
          (put-text-property (point) end 'read-only t)
          (put-text-property (point) end 'rear-nonsticky t)))))

  (defun config-eshell--inhibit-submission-on-empty (f &rest args)
    ;; Always run command if this is a dir change from hyrda.
    (let* ((start eshell-last-output-end)
           (end (line-end-position))
           (input (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim input))
          (delete-region start end)
        (config-eshell--dim-commands-on-submission)
        (apply f args))))

  :config
  (progn
    ;; Show a horizontal rule and timestamp between commands.

    (defvar-local config-eshell--previous-time nil)

    (setq pretty-eshell-header-fun
          (let ((page-break "\u000c"))
            (lambda ()
              (let* ((time (format-time-string "%H:%M" (current-time)))
                     (timestamp (propertize time 'face 'eshell-timestamp))
                     (str (propertize (concat timestamp "\n" page-break "\n")
                                      'read-only t)))
                (prog1 str
                  (setq config-eshell--previous-time time))))))

    ;; Prevent command submission if there's no text to submit.

    (advice-add 'eshell-send-input :around #'config-eshell--inhibit-submission-on-empty)

    ;; Change the prompt, depending on the previous command's exit code.

    (setq eshell-prompt-function 'pretty-eshell-prompt-func)
    (setq pretty-eshell-prompt-string-fun (lambda ()
                                            (concat " " (if (eshell-exit-success-p)
                                                            ">"
                                                          (propertize "✘" 'face 'error))
                                                    " ")))
    (setq eshell-prompt-regexp (rx bol (* space) (or ">" "✘") space))

    ;; Directory
    (pretty-eshell-define-section rk-eshell--dir-sect
      ""
      (abbreviate-file-name (eshell/pwd))
      '(:inherit eshell-ls-directory :weight light))

    ;; Git Branch
    (pretty-eshell-define-section rk-eshell--git-sect
      ""
      (when-let* ((branch (magit-get-current-branch)))
        (s-truncate 20 (s-chop-prefixes '("feature/" "release/") branch)))
      '(:foreground "#cb4b16" :weight light))

    ;; Node version
    (pretty-eshell-define-section rk-eshell--node-sect
      ""
      (when (and (bound-and-true-p fnm-current-version)
                 (locate-dominating-file default-directory ".nvmrc"))
        (car fnm-current-version))
      '(:foreground "#d33682" :weight light))

    (setq pretty-eshell-funcs (list rk-eshell--dir-sect rk-eshell--git-sect rk-eshell--node-sect))))

(defun rk-eshell--align-timestamp ()
  (let* ((space-char 32)
         (timestamp-width 5) ; HH:MM
         (spaces-count (- (1+ (window-width)) timestamp-width))
         (width (* (char-width space-char) spaces-count))
         (new-display-entry (vconcat (make-list width space-char))))
    (unless (equal new-display-entry (elt buffer-display-table ?\^I))
      (aset buffer-display-table ?\^I new-display-entry))))

(defun eshell-timestamp--update-display-table (window)
  (with-current-buffer (window-buffer window)
    (with-selected-window window
      (let ((inhibit-read-only t))
        (page-break-lines--update-display-table window)
        (unless buffer-display-table
          (setq buffer-display-table (make-display-table)))
        (rk-eshell--align-timestamp)))))

(defun eshell-timestamp--update-display-tables  (&optional frame)
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'eshell-mode)
        (eshell-timestamp--update-display-table window)))))

(defun eshell-timestamp--configure-hooks ()
  (dolist (hook '(window-configuration-change-hook
                  window-size-change-functions
                  after-setting-font-hook))
    (add-hook hook 'eshell-timestamp--update-display-tables t t)))

(add-hook 'eshell-mode-hook #'eshell-timestamp--configure-hooks)

(defun config-eshell--inhibit-read-only (f &rest args)
  (let ((inhibit-read-only t))
    (apply f args)))

(advice-add 'eshell-output-filter :around #'config-eshell--inhibit-read-only)

(use-package eshell-fns
  :after eshell)

(provide 'rk-eshell)

;;; rk-eshell.el ends here
