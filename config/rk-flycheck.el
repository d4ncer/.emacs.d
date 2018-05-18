;;; rk-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(autoload 'evil-define-key "evil-core")
(autoload 'evil-set-initial-state "evil-core")

(require 'spacemacs-keys)

(use-package flycheck
  :straight t
  :defer 3
  :commands (global-flycheck-mode
             flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)
  :init
  (spacemacs-keys-declare-prefix "e" "errors")


  :preface
  (progn
    (autoload 'flycheck-buffer "flycheck")
    (autoload 'flycheck-error-format-message-and-id "flycheck")
    (autoload 'flycheck-get-error-list-window "flycheck")
    (autoload 'flycheck-may-use-echo-area-p "flycheck")
    (autoload 'projectile-project-p "projectile")
    (autoload 'projectile-process-current-project-buffers "projectile")

    (defun rk-flycheck-display-error-messages (errors)
      (unless (flycheck-get-error-list-window 'current-frame)
        (when (and errors (flycheck-may-use-echo-area-p))
          (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
            (display-message-or-buffer (string-join messages "\n\n")
                                       flycheck-error-message-buffer
                                       'display-buffer-popup-window))))))

  :config
  (progn
    (global-flycheck-mode +1)

    (setq flycheck-display-errors-function 'rk-flycheck-display-error-messages)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-check-syntax-automatically '(save))

    (spacemacs-keys-set-leader-keys
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "ee" 'flycheck-explain-error-at-point
      "en" 'flycheck-next-error
      "eN" 'flycheck-next-error
      "ep" 'flycheck-previous-error
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup)

    (with-eval-after-load 'evil
      (evil-set-initial-state 'flycheck-error-list-mode 'motion)
      (evil-define-key 'normal flycheck-error-list-mode-map
        (kbd "n") #'flycheck-error-list-next-error
        (kbd "p") #'flycheck-error-list-previous-error
        (kbd "q") #'quit-window))

    (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
    (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map)
    (bind-key "M-j" 'flycheck-next-error flycheck-mode-map)
    (bind-key "M-k" 'flycheck-previous-error flycheck-mode-map)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))

(use-package flycheck-transient-state
  :config
  (spacemacs-keys-set-leader-keys "el" #'rk-flycheck-ts-transient-state/body))

(provide 'rk-flycheck)

;;; rk-flycheck.el ends here
