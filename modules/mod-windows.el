;;; mod-windows.el --- Window management and display-buffer configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains window management configuration including:
;; - Custom window split functions (from +window.el)
;; - display-buffer-alist configuration ("The Dark Pit of Display Buffer")
;; - Side window rules (top, left, right, bottom)
;; - Window dedication utilities
;; - Display buffer fallback actions

;;; Code:

(require '+window)

;;; The Dark Pit of Display Buffer and Despair

;; Emacs' window management system is designed around the 'principle of most
;; surprise'. As you edit, Emacs wants to show you buffers, but how it choses
;; which window to display that buffer feels totally unpredictable.

;; Sometimes Emacs pops up a new one. Sometimes it re-uses an existing one. Why
;; did it pick that one? I dunno, it depends on each individual command how it
;; implemented buffer display. Sometimes you can quit the window with 'q';
;; sometimes you can't. Sometimes it will save & restore your window state on
;; quit, sometimes it scrambles it. Occasionally it will even use a *new frame*
;; (shudder).

;; You can improve the situation with `display-buffer-alist' and a lot of elbow
;; grease, but Emacs has the spirit of a wild stallion that can never be truly
;; tamed.

;; When I'm editing, I generally want a single main buffer to focus on, or two
;; displayed side-by-side. Sometimes it makes sense to pop up another window for
;; a short time, e.g. when looking up docs, running a couple of shell commands,
;; or doing a compilation. I teach `display-buffer' to use side windows for
;; these buffers.

(setq display-buffer-alist
      (cl-labels ((mode-active-p (mode)
                    (cl-assert (symbolp mode))
                    (lambda (buf _action)
                      (with-current-buffer buf
                        (and (boundp mode) (eval mode))))))
        (append

         ;; Top side - wide-screen, modal contexts

         (cl-labels ((top (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (window-height . 0.2)
                                    (side . top)
                                    (slot . 0)))))
           (list
            (top '(derived-mode . debugger-mode))
            (top (rx bos "CAPTURE-") '(window-height . 0.6))))

         ;; Left side - Search results, debugger ancillary buffers. Generally,
         ;; things that define a temporary context change.

         (cl-labels ((left (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (side . left)
                                    (slot . 0)))))
           (list
            (left (rx bos "*Debugger-record*" eos)
                  '(slot . 1)
                  '(window-height . 0.3))

            ;; Search results
            (left `(or
                    (derived-mode . grep-mode)
                    (derived-mode . embark-collect-mode)
                    ,(rx bos "*Embark Export: ")
                    ,(rx bos "*org-node-grep"))
                  '(window-width 80))))

         ;; Right side - documentation, reference buffers & command outputs.

         (cl-labels ((right (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (side . right)
                                    (slot . 0)))))
           (list
            (right `(or
                     ;; shell output
                     ,(rx bos "*shell command output*" eos)
                     ,(rx bos "*Org babel results*" eos)
                     ,(rx bos "*async shell command*" eos)))

            (right `(or
                     ;; Help buffers
                     ,(mode-active-p 'gptel-mode)
                     (derived-mode . rfc-mode)
                     (derived-mode . help-mode)
                     (derived-mode . helpful-mode)
                     (derived-mode . Man-mode)
                     (derived-mode . woman-mode)

                     ;; org-node buffers
                     ,(rx bos "*org-node*" eos)

                     ;; eldoc
                     ,(rx bos "*eldoc*" eos))
                   '(window-width . 80))))


         ;; Bottom - Prompts, warnings, errors, compilation buffers.

         (cl-labels ((bottom (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (side . bottom)
                                    (dedicated . t)
                                    (slot . 0)))))
           (list
            (bottom `(or
                      ;; REPLs
                      (derived-mode . inferior-emacs-lisp-mode)
                      (derived-mode . inf-elixir-mode)

                      ;; shells
                      (derived-mode . mistty-mode)
                      (derived-mode . eshell-mode)

                      ;; compilation
                      (derived-mode . compilation-mode)

                      ;; org-mode popups
                      ,(rx bos "*calendar*" eos)
                      ,(rx bos " *Agenda Commands*" eos)
                      ,(rx bos "*Org Select*" eos)
                      ,(rx bos "*Org Note*" eos)
                      ,(rx bos "*Org-Babel Error Output*" eos)

                      ;; misc
                      (derived-mode . ert-simple-view-mode)))))

         ;; Buffers that should never pop up.

         (cl-labels ((suppress (pred)
                       (cons pred
                             `((display-buffer-no-window)
                               (allow-no-window . t)))))
           (list
            (suppress `(or ,(rx bos "*warnings*" eos)
                           ,(rx bos "*async-native-compile-Log*" eos))))))))


;; Then, customise what display-buffer will do for all buffers not matching the
;; above rules.

;; In particular, prevent display-buffer from ever popping open another frame.

(setq display-buffer-fallback-action
      `((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         ,(defun +display-buffer-fallback (buffer &rest _)
            (when-let* ((win (split-window-sensibly)))
              (with-selected-window win
                (switch-to-buffer buffer)
                (help-window-setup (selected-window))))
            t))))

;; Apply a few more editor settings that are tightly coupled to display-buffer.

(setq window-combination-resize t)
(setq switch-to-buffer-in-dedicated-window 'pop)

(provide 'mod-windows)
;;; mod-windows.el ends here
