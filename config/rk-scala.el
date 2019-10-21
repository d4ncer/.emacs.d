;;; rk-scala.el --- Configuration for Scala packages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'lsp)
(require 'rk-utils)

(autoload 'projectile-project-name "projectile")

;; Load modes.

(use-package scala-mode
  :straight t
  :defer t
  :mode (("\\.scala\\'" . scala-mode))
  :interpreter
  ("scala" . scala-mode)
  :hook
  (scala-mode . lsp)
  :config
  (progn
    (general-def :keymaps 'scala-mode-map :states 'normal
      "J" #'rk-utils--chainable-aware-join-line)

    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)
    (setq scala-indent:indent-value-expression t)))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :defines (sbt:buffer-project-root)
  :preface
  (autoload 'sbt:find-root-impl "sbt-mode-project")
  (defun rk-set-sbt-root ()
    "Sets sbt root for the given project."
    (-when-let* ((root (locate-dominating-file (buffer-file-name) "build.sbt"))
                 (sbt-root (and root
                                (sbt:find-root-impl "build.sbt" root))))
      (setq-local sbt:buffer-project-root sbt-root)))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (progn
    (add-hook 'sbt-mode-hook #'rk-set-sbt-root)
    (substitute-key-definition 'minibuffer-complete-word
                               'self-insert-command
                               minibuffer-local-completion-map)))

(provide 'rk-scala)

;;; rk-scala.el ends here
