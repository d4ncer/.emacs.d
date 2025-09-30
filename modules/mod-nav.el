;;; mod-nav.el --- Navigation tools (avy, ace-window) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains navigation tool configuration including:
;; - avy (jump navigation with custom actions and pulsar integration)
;; - ace-window (window jumping)
;; - Custom avy actions from +avy.el

;;; Code:

(require '+avy)

;;; Avy - Jump navigation

(use-package avy :ensure t
  :config
  (require '+avy)
  :custom
  (avy-dispatch-alist '((?x . avy-action-kill-stay)
                        (?d . avy-action-kill-move)
                        (?c . +avy-action-change-move)
                        (?t . avy-action-teleport)
                        (?v . avy-action-mark)
                        (?y . avy-action-copy)
                        (?p . avy-action-yank)
                        (?P . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?K . +avy-action-evil-lookup)
                        (? . avy-action-zap-to-char)))

  ;; Integrate avy with pulsar for better visual feedback

  :config
  (with-eval-after-load 'pulsar
    (define-advice avy-process (:filter-return (result) pulse-red-on-no-matches)
      (when (eq t result)
        (when pulsar-mode
          (pulsar-pulse-line-red)))
      result)

    (defun +avy-pulse-for-move (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line)))

    (advice-add #'avy-action-goto :after #'+avy-pulse-for-move)

    (defun +avy-pulse-for-change (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line-magenta)))

    (advice-add #'avy-action-kill-move :after #'+avy-pulse-for-change)
    (advice-add #'+avy-action-change-move :after #'+avy-pulse-for-change)

    (defun +avy-pulse-for-change-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-magenta))))

    (advice-add #'avy-action-kill-stay :around #'+avy-pulse-for-change-elsewhere)

    (defun +avy-pulse-for-action-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-green))))

    (advice-add #'+avy-action-evil-lookup :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-copy :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-ispell :around #'+avy-pulse-for-action-elsewhere))

  ;; KLUDGE: Pre-configure indentation for dynamically-loaded macro. Ensures
  ;; Apheleia applies correct indentation if I touch this file without avy being
  ;; loaded in the editing session.
  :init
  (function-put '+with-clean-up-in-starting-buffer-and-window 'lisp-indent-function 1))

;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
;; a selection, make - call avy.
;; (general-define-key :states '(normal motion)
;;                     "-" (general-predicate-dispatch #'avy-goto-char-timer
;;                           (region-active-p) #'expreg-contract)
;;                     "+" #'+expreg-expand-dwim)

;;; Ace-window - Window navigation

(use-package ace-window :ensure t
  ;; Jump to specific windows
  :general ("M-o" #'ace-window))

(provide 'mod-nav)
;;; mod-nav.el ends here
