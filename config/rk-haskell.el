;;; rk-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'definers)

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
    ;; Use 2 space indentation style.

    (setq haskell-indentation-layout-offset 2)
    (setq haskell-indentation-starter-offset 2)
    (setq haskell-indentation-where-pre-offset 2)
    (setq haskell-indentation-where-post-offset 2)
    (setq haskell-indentation-left-offset 2)
    (setq haskell-indent-spaces 2)

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

  :defines (haskell-stylish-on-save
            haskell-completing-read-function
            haskell-interactive-popup-errors)
  :config
  (progn
    (setq haskell-stylish-on-save t)
    (setq haskell-completing-read-function #'completing-read)
    (setq haskell-interactive-popup-errors nil)))


(use-package haskell-compile
  :after haskell-mode
  :commands (haskell-compile)
  :defines (haskell-compile-command
            haskell-compile-cabal-build-command)
  :config
  (progn
    (rk-local-leader-def :keymaps 'haskell-mode-map
      "c" '(haskell-compile :wk "compile"))

    (setq haskell-compile-command "stack exec -- ghc -Wall -ferror-spans -fforce-recomp -c %s")
    (setq haskell-compile-cabal-build-command "stack build --ghc-options -ferror-spans")))


(use-package haskell-cabal
  :after haskell-mode
  :general
  (:keymaps 'haskell-cabal-mode-map
            "C-c C-c" #'haskell-compile))


(use-package haskell-doc
  :after haskell-mode)

(use-package haskell-interactive-mode
  :after haskell-mode
  :commands (interactive-haskell-mode)
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))


(use-package haskell-debug
  :after haskell-mode
  :general
  (:keymaps 'haskell-debug-mode-map :states '(normal motion visual emacs)
            "n" #'haskell-debug/next
            "p" #'haskell-debug/previous
            "q" #'quit-window))


(use-package haskell-presentation-mode
  :after haskell-mode
  :general
  (:keymaps 'haskell-presentation-mode-map :states '(normal motion)
            "q" #'quit-window))

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

  :general
  (:keymaps 'intero-mode-map :states '(normal insert)
            "C-." #'intero-repl)
  (:keymaps 'intero-repl-mode-map
            "C-." #'intero-goto-defintion
            "C-d" #'kill-buffer-and-window)
  (:keymaps 'haskell-mode-map :states '(normal motion)
            "gd" #'intero-goto-definition)

  :config
  (progn
    (rk-local-leader-def :keymaps 'haskell-mode-map
      "i"   '(:ignore t :wk "intero")
      "i i" '(rk-haskell--insert-intero-type :wk "insert type")
      "i t" '(intero-targets :wk "targets")

      "r"   '(:ignore t :wk "repl")
      "r r" '(intero-repl :wk "repl")
      "r l" '(intero-repl-load :wk "load repl"))

    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero 'haskell-hlint))))

(use-package hindent
  :straight t
  :after haskell-mode
  :config
  (progn
    (rk-local-leader-def :keymaps 'haskell-mode-map
      "f"   '(:ignore t :wk "format")
      "f ." '(intero-repl :wk "format buffer")
      "f f" '(intero-repl-load :wk "format declaration"))
    (setq hindent-reformat-buffer-on-save t)
    (add-hook 'haskell-mode-hook #'hindent-mode)))

(provide 'rk-haskell)

;;; rk-haskell.el ends here
