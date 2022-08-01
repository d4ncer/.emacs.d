;;; rk-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

(defvar rk-flycheck--redundant-checkers
  '(javascript-jshint)
  "Checkers that serve no purpose.")

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  (prog-mode . flycheck-mode-on-safe)

  :general
  (:keymaps
   'flycheck-mode-map
   "M-n" #'flycheck-next-error
   "M-p" #'flycheck-previous-error
   "M-j" #'flycheck-next-error
   "M-k" #'flycheck-previous-error)
  (:keymaps 'flycheck-error-list-mode-map :states 'normal
            "n" #'flycheck-error-list-next-error
            "j" #'flycheck-error-list-next-error
            "p" #'flycheck-error-list-previous-error
            "k" #'flycheck-error-list-previous-error
            "q" #'quit-window)

  (:states 'motion
           :keymaps 'flycheck-error-list-mode-map
           "j" #'flycheck-error-list-next-error
           "k" #'flycheck-error-list-previous-error
           "RET" #'flycheck-error-list-goto-error
           "n" #'flycheck-error-list-next-error
           "p" #'flycheck-error-list-previous-error
           "q" #'quit-window
           "C-g" #'flycheck-toggle-error-list)

  :custom
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-python-pycompile-executable "python")
  (flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch mode-enabled))
  (flycheck-global-modes '(not text-mode
                               org-mode
                               org-agenda-mode))

  :preface
  (autoload 'flycheck-list-errors "flycheck")
  (defun flycheck-toggle-error-list ()
    "Show or hide the error list."
    (interactive)
    (if-let* ((window (seq-find (lambda (it)
                                  (equal flycheck-error-list-buffer
                                         (buffer-name (window-buffer it))))
                                (window-list))))
        (delete-window window)
      (flycheck-list-errors)
      (pop-to-buffer flycheck-error-list-buffer)))

  :init
  (rk-leader-def
    "ec" '(flycheck-clear :wk "clear errors")
    "eh" '(flycheck-describe-checker :wk "describe checker")
    "el" '(flycheck-toggle-error-list :wk "list errors")
    "ee" '(flycheck-buffer :wk "check buffer")
    "es" '(flycheck-select-checker :wk "select checker")
    "eS" '(flycheck-set-checker-executable :wk "set checker binary")
    "ev" '(flycheck-verify-setup :wk "verify setup"))

  :config
  (dolist (checker rk-flycheck--redundant-checkers)
    (add-to-list 'flycheck-disabled-checkers checker))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (slot . 1)
                 (reusable-frames . visible)
                 (side . bottom)
                 (window-height . 0.2))))

(use-package flycheck
  :straight t
  :after projectile
  :config
  (defun rk-flycheck--check-all-project-buffers ()
    (when (and (bound-and-true-p projectile-mode) (projectile-project-p))
      (projectile-process-current-project-buffers
       (lambda (buf)
         (with-current-buffer buf
           (when (bound-and-true-p flycheck-mode)
             ;; HACK: Inhibit checks for elisp, otherwise flycheck will
             ;; spawn a bunch of thrashing Emacs processes.
             (unless (derived-mode-p 'emacs-lisp-mode)
               (flycheck-buffer))))))))

  (add-hook 'after-save-hook #'rk-flycheck--check-all-project-buffers))

(use-package flycheck
  :straight t
  :after evil
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'motion))

(use-package flycheck
  :straight t
  :after nano-modeline
  :config
  (defun rk-flycheck--custom-mode-line-status-text (&optional status)
    (pcase (or status flycheck-last-status-change)
      (`no-checker "Checks[-]")
      (`errored "Checks[ERROR]")
      (`finished
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (cond
          ((and .error .warning)
           (format "✖ (%s error%s, %s warn%s)"
                   .error
                   (if (equal .error 1) "" "s")
                   .warning
                   (if (equal .warning 1) "" "s")))
          (.error
           (format "✖ (%s error%s)" .error (if (equal .error 1) "" "s")))

          (.warning
           (format "! (%s warning%s)" .warning (if (equal .warning 1) "" "s")))
          (t
           "✔"))))
      (`interrupted "? (interrupted)")
      (`suspicious "? (suspicious)")
      (`running "···")
      (_
       "")))

  (defun rk-nano-modeline--default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                                        (propertize branch 'face 'italic)))
                                     ") " (rk-flycheck--custom-mode-line-status-text))
                             position)))
  (advice-add 'nano-modeline-default-mode :override #'rk-nano-modeline--default-mode)
  :custom
  (flycheck-mode-line '(:eval (rk-flycheck--custom-mode-line-status-text))))

(use-package flycheck
  :straight t
  :config
  (defun rk-flycheck--maybe-inhibit (result)
    (unless (or (equal (buffer-name) "*ediff-merge*")
                (string-suffix-p ".dir-locals.el" (buffer-file-name))
                (string-match-p (rx bol "*Pp ") (buffer-name))
                (string-match-p (rx "/node_modules/") default-directory))
      result))

  (advice-add 'flycheck-may-enable-mode :filter-return #'rk-flycheck--maybe-inhibit))

(provide 'rk-flycheck)

;;; rk-flycheck.el ends here
