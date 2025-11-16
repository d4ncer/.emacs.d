;;; mod-ai.el --- AI integration (gptel) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains AI integration configuration including:
;; - gptel for LLM interactions (Claude integration)
;; - Custom helper functions for code explanation and error analysis

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; GPtel - LLM integration

(use-package gptel :ensure t
  ;; Provides LLM integrations.
  :demand t
  :hook (gptel-mode-hook . visual-line-mode)
  :general (:states 'visual "RET" #'gptel-rewrite)
  :init
  (defun +gptel-send ()
    (interactive)
    (unless (region-active-p)
      (goto-char (line-end-position)))
    (gptel-send)
    (evil-normal-state))
  (defun +gptel-go-to-insert ()
    "Go to end of buffer and enter evil insert state."
    (interactive)
    (goto-char (point-max))
    (backward-char)
    (evil-insert-state))

  (defmacro +gptel-request-with-buffer (prompt response-buffer-name &rest body)
    "Make a gptel request with PROMPT, displaying response in RESPONSE-BUFFER-NAME.
BODY is executed to prepare the response buffer."
    `(let ((response-buffer (get-buffer-create ,response-buffer-name)))
       (with-current-buffer response-buffer
         (let ((inhibit-read-only t))
           (visual-line-mode 1)
           (markdown-ts-mode)
           (goto-char (point-max))
           (unless (bobp)
             (insert "\n\n" (make-string 40 ?-) "\n\n"))
           ,@body)
         (read-only-mode 1))
       (display-buffer response-buffer)
       (gptel-request ,prompt
         :callback (lambda (response _info)
                     (when (stringp response)
                       (with-current-buffer response-buffer
                         (let ((inhibit-read-only t))
                           (goto-char (point-max))
                           (insert response)))))
         :buffer response-buffer
         :stream t)))

  (defun +gptel-explain-code ()
    "Explain the selected code using gptel, with language inference from major mode."
    (interactive)
    (unless (region-active-p)
      (user-error "No region selected. Please select code to explain"))
    (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
           (language (or (when (symbolp major-mode)
                           (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
                         "text"))
           (prompt (format "Please explain this %s code:\n\n```%s\n%s\n```"
                           language language code)))
      (+gptel-request-with-buffer prompt "*gptel-response*")))

  (defun +gptel-explain-emacs-error ()
    "Explain the selected emacs error message and offer suggestions to fix."
    (interactive)
    (unless (region-active-p)
      (user-error "No region selected. Please select an error message to explain"))
    (let* ((error-text (buffer-substring-no-properties (region-beginning) (region-end)))
           (prompt (format "Please explain this Emacs error message and provide suggestions to fix it:\n\n```\n%s\n```\n\nPlease include:\n1. What this error means\n2. Common causes\n3. Step-by-step solutions\n4. Prevention tips"
                           error-text)))
      (+gptel-request-with-buffer prompt "*gptel-response*")))

  :general
  (:keymaps 'gptel-mode-map :states '(normal insert)
            "C-c C-s" #'+gptel-send
            "A" #'+gptel-go-to-insert)
  :custom
  (gptel-model 'claude-sonnet-4-20250514)
  (gptel-default-mode 'org-mode)
  ;; Use nano-modeline's special format
  (gptel-use-header-line nil)
  :config
  (alist-set! gptel-prompt-prefix-alist 'org-mode "* ")
  (setq-hook! 'gptel-mode-hook
    org-pretty-entities-include-sub-superscripts nil)

  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (lambda ()
                 (auth-source-pick-first-password :host "api.anthropic.com"))))

  (add-hook 'gptel-mode-hook 'evil-insert-state)

  ;; Prevent transient from creating extra windows due to conflicts with custom
  ;; display-buffer rules. See:
  ;; https://github.com/magit/transient/discussions/358
  (setq transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t)))

  ;; Pulse the part of the buffer being sent to the LLM.

  (define-advice gptel-send (:after (&optional show-transient) pulse)
    (when (bound-and-true-p pulsar-mode)
      (unless show-transient
        (let ((pulsar-region-face 'pulsar-green))
          (cond ((region-active-p)
                 (pulsar-pulse-region))
                ((and gptel-mode (org-at-heading-p))
                 (pulsar-pulse-line-green))
                (t
                 (pulsar--create-pulse (cons (point-min) (point)) 'pulsar-green))))))))

(provide 'mod-ai)
;;; mod-ai.el ends here
