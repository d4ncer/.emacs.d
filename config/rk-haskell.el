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
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*stack hoogle*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.5))))

(use-package lsp-haskell
  :straight t
  :after (haskell-mode lsp)
  ;; :custom
  ;; (lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  ;; (lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hls.log"))
  :hook
  (haskell-mode . lsp))

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

(use-package haskell-interactive-mode
  :after haskell-mode
  :commands (interactive-haskell-mode)
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(provide 'rk-haskell)

;;; rk-haskell.el ends here
