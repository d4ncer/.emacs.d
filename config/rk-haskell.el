;;; rk-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(autoload 'evil-define-key "evil-core")

(use-package haskell-mode
  :straight t
  :mode
  (("\\.[gh]s\\'" . haskell-mode)
   ("\\.l[gh]s\\'" . literate-haskell-mode)
   ("\\.hsc\\'" . haskell-mode))

  :interpreter
  (("runghc" . haskell-mode)
   ("runhaskell" . haskell-mode))

  :init
  (progn
    (add-to-list 'completion-ignored-extensions ".hi")
    (add-to-list 'completion-ignored-extensions ".gm"))

  :preface
  (defun rk-haskell--set-indentation-step ()
    (with-no-warnings (setq evil-shift-width 4))
    (setq tab-width 4))

  :config
  (progn
    ;; Use 4 space indentation style.

    (setq haskell-indentation-layout-offset 4)
    (setq haskell-indentation-starter-offset 2)
    (setq haskell-indentation-where-pre-offset 2)
    (setq haskell-indentation-where-post-offset 2)
    (setq haskell-indentation-left-offset 4)
    (setq haskell-indent-spaces 4)

    (add-hook 'haskell-mode-hook #'rk-haskell--set-indentation-step)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*intero:")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*stack hoogle*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.5)))))


(use-package haskell-customize
  :after haskell-mode
  :config
  (progn
    (setq haskell-stylish-on-save t)
    (setq haskell-completing-read-function #'completing-read)
    (setq haskell-interactive-popup-errors nil)))


(use-package haskell-compile
  :after haskell-mode
  :commands (haskell-compile)
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'haskell-mode
      "c" #'haskell-compile)

    (setq haskell-compile-command "stack exec -- ghc -Wall -ferror-spans -fforce-recomp -c %s")
    (setq haskell-compile-cabal-build-command "stack build --ghc-options -ferror-spans")))


(use-package haskell-cabal
  :after haskell-mode
  :config
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))


(use-package haskell-doc
  :after haskell-mode)


(use-package haskell-interactive-mode
  :after haskell-mode
  :commands (interactive-haskell-mode)
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))


(use-package haskell-debug
  :after haskell-mode
  :config
  (with-no-warnings
    (evilified-state-evilify-map haskell-debug-mode-map
      :mode haskell-debug-mode
      :bindings
      (kbd "n") #'haskell-debug/next
      (kbd "N") #'haskell-debug/previous
      (kbd "p") #'haskell-debug/previous
      (kbd "q") #'quit-window)))


(use-package haskell-presentation-mode
  :after haskell-mode
  :config
  (evil-define-key 'normal haskell-presentation-mode-map (kbd "q") #'quit-window))

(use-package intero
  :straight t
  :after haskell-mode

  :commands (intero-mode
             intero-targets
             intero-goto-definition
             intero-repl
             intero-repl-load
             intero-repl-switch-back
             intero-type-at)

  :preface
  (progn
    (autoload 'intero-mode-map "intero")
    (autoload 'f-filename "f")
    (autoload 'f-base "f")
    (autoload 's-lex-format "s")
    (autoload 's-titleize "s")

    (defun rk-haskell--maybe-intero-mode ()
      (unless (or (derived-mode-p 'ghc-core-mode)
                  (equal (get major-mode 'mode-class) 'special))
        (intero-mode +1)))

    (defun rk-haskell--insert-intero-type ()
      "Insert Intero type above decl."
      (interactive)
      (intero-type-at t)))

  :init
  (add-hook 'haskell-mode-hook #'rk-haskell--maybe-intero-mode)

  :bind
  (:map
   intero-mode-map
   ("M-." . intero-goto-definition)
   ("M-," . pop-tag-mark)
   :map
   intero-repl-mode-map
   ("C-." . intero-repl-switch-back)
   ("C-d" . delete-window))

  :config
  (progn
    (evil-define-key 'normal haskell-mode-map (kbd "gd") #'intero-goto-definition)
    (spacemacs-keys-declare-prefix-for-mode 'haskell-mode "m r" "repl")
    (spacemacs-keys-declare-prefix-for-mode 'haskell-mode "m i" "intero")
    (spacemacs-keys-declare-prefix-for-mode 'haskell-mode "m g" "goto")

    (spacemacs-keys-set-leader-keys-for-major-mode 'haskell-mode
      "rr" #'intero-repl
      "rl" #'intero-repl-load
      "it" #'intero-targets
      "gg" #'intero-goto-definition
      "ii" #'rk-haskell--insert-intero-type)

    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero 'haskell-hlint))

    (with-no-warnings
      (evil-define-key 'normal intero-mode-map (kbd "C-.") #'intero-repl)
      (evil-define-key 'insert intero-mode-map (kbd "C-.") #'intero-repl)
      (evil-define-key 'normal intero-mode-map (kbd "M-.") #'intero-goto-definition)
      (evil-define-key 'normal intero-mode-map (kbd "M-,") #'pop-tag-mark))))

(use-package hindent
  :straight t
  :after haskell-mode
  :commands (hindent-mode hindent-reformat-decl-or-fill hindent-reformat-buffer)
  :config
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'haskell-mode "m f" "format")
    (spacemacs-keys-set-leader-keys-for-major-mode 'haskell-mode
      "f." #'hindent-reformat-buffer
      "ff" #'hindent-reformat-decl-or-fill)
    (setq hindent-reformat-buffer-on-save t)
    (add-hook 'haskell-mode-hook #'hindent-mode)))

(provide 'rk-haskell)

;;; rk-haskell.el ends here
