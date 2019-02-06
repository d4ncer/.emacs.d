;;; rk-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'definers)

(autoload 'evil-set-initial-state "evil-core")

(defvar rk-flycheck--redundant-checkers
  '(javascript-jshint))

(use-package flycheck
  :straight t
  :defer t
  :commands (global-flycheck-mode
             flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)
  :general
  (:keymaps 'flycheck-mode-map
            "M-n" #'flycheck-next-error
            "M-p" #'flycheck-previous-error
            "M-j" #'flycheck-next-error
            "M-k" #'flycheck-previous-error)
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

    ;; Remove redundant checkers from list

    (dolist (checker rk-flycheck--redundant-checkers)
      (add-to-list 'flycheck-disabled-checkers checker))

    ;; Config

    (setq flycheck-display-errors-function 'rk-flycheck-display-error-messages)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-check-syntax-automatically '(save))

    (rk-leader-def
      "ec" '(flycheck-clear :wk "clear errors")
      "eh" '(flycheck-describe-checker :wk "describe checker")
      "ee" '(flycheck-buffer :wk "check buffer")
      "en" '(flycheck-next-error :wk "next error")
      "ep" '(flycheck-previous-error :wk "prev error")
      "es" '(flycheck-select-checker :wk "select checker")
      "eS" '(flycheck-set-checker-executable :wk "set checker binary")
      "ev" '(flycheck-verify-setup :wk "verify setup"))

    (with-eval-after-load 'evil
      (evil-set-initial-state 'flycheck-error-list-mode 'motion)
      (general-def 'normal flycheck-error-list-mode-map
        "n" #'flycheck-error-list-next-error
        "p" #'flycheck-error-list-previous-error
        "q" #'quit-window))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))

(use-package flycheck-transient-state
  :init
  (rk-leader-def
    "el" '(rk-flycheck-ts-transient-state/body :wk "error hydra")))

(provide 'rk-flycheck)

;;; rk-flycheck.el ends here
