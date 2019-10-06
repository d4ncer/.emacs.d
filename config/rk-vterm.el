;;; rk-vterm.el --- Vvvvvvvvvvvvvvvvvterm!  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package vterm
  :straight t
  :general
  (:keymaps 'vterm-mode-map :states 'normal "P" 'vterm-yank)
  (:keymaps 'vterm-mode-map :states '(normal insert) "s-v" 'vterm-yank)
  :preface
  (progn
    (defun config-terminal--build-vterm (package &rest _)
      (when (member package '("vterm"))
        (let* ((root (straight--build-dir "vterm"))
               (module-build-dir (f-join root "build")))
          (mkdir module-build-dir t)
          (let ((default-directory module-build-dir))
            (compilation-start "cmake .. && make")))))
    (add-hook 'straight-use-package-pre-build-functions #'config-terminal--build-vterm))
  :config
  ;; Prevent vterm from handling function keys.
  (dotimes (n 12)
    (define-key vterm-mode-map (kbd (format "<f%s>" (1+ n))) nil)))

(use-package vterm-toggle
  :straight (:host github :repo "jixiuf/vterm-toggle")
  :general ("<f10>" #'vterm-toggle
            "<S-f10>" #'vterm-toggle-cd))

(provide 'rk-vterm)

;;; rk-vterm.el ends here