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


    (defun config-eshell--inhibit-read-only (f &rest args)
      (let ((inhibit-read-only t))
        (apply f args)))

    (advice-add 'eshell-output-filter :around #'config-eshell--inhibit-read-only)

    (add-hook 'eshell-mode-hook #'rk-eshell--setup-keybindings)

    ;; keep aliases under etc directory, which is tracked by git.

    (f-mkdir rk-eshell--etc-dir)
    (general-setq eshell-aliases-file (f-join rk-eshell--etc-dir  "aliases"))))

(use-package eshell-fns
  :after eshell)

(use-package aweshell
  :straight (:host github :repo "manateelazycat/aweshell" :branch "master"))

(provide 'rk-eshell)

;;; rk-eshell.el ends here
