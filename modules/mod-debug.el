;;; mod-debug.el --- Debugging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; This module contains debugging configuration including:
;; - debug (built-in Emacs Lisp debugger)

;;; Code:

(eval-and-compile
  (require '+corelib))

;;; Debug - Emacs Lisp debugger

(use-package debug
  ;; The built-in debugger for the Emacs Lisp runtime.
  :init
  (defun +debugger-toggle-on-exit-frame ()
    (interactive)
    (let ((enabled-for-line (save-excursion
                              (goto-char (line-beginning-position))
                              (looking-at (rx (* space) "*" (+ space))))))
      (cond
       (enabled-for-line
        (debugger-frame-clear)
        (message "debug on exit for frame disabled"))
       (t
        (debugger-frame)
        (message "debug on exit for frame enabled")))))

  :general
  (:keymaps 'debugger-mode-map :states 'normal "t" #'+debugger-toggle-on-exit-frame)

  :config
  (define-advice debugger-record-expression (:after (&rest _) display-buffer)
    (display-buffer debugger-record-buffer))

  ;; Show keybindings in the header line for Backtrace buffers.

  (defconst +debugger-mode-line-format
    (cl-labels ((low-emphasis (str)
                  (propertize str 'face 'parenthesis))
                (key (key desc)
                  (concat (propertize key 'face 'which-key-key-face) (low-emphasis ":") " " desc))
                (group (&rest children)
                  (concat (low-emphasis "|") "  " (apply #'distribute children)))
                (distribute (&rest strs)
                  (string-join strs "  ")))
      (distribute
       (propertize "  " 'face 'font-lock-builtin-face)
       (group
        (key "d" "step")
        (key "c" "continue")
        (key "r" "return"))
       (group
        (key "t" "toggle debug on exit frame")
        (key "J" "jump")
        (key "L" "locals"))
       (group
        (key "E" "eval")
        (key "R" "eval & record")))))

  (setq-hook! 'debugger-mode-hook
    header-line-format +debugger-mode-line-format))

(provide 'mod-debug)
;;; mod-debug.el ends here
