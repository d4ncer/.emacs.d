;;; rk-darwin.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-n") 'new-frame)
(global-set-key (kbd "s-w") 'delete-frame)

(use-package exec-path-from-shell
  :straight t
  :if window-system
  :config
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    (exec-path-from-shell-initialize)

    ;;; Use gls instead of ls on OS X (if available)
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))
  :functions
  (exec-path-from-shell-initialize))

(use-package osx-trash
  :straight t
  :config
  (osx-trash-setup)
  :functions
  (osx-trash-setup))

(provide 'rk-darwin)

;;; rk-darwin.el ends here
