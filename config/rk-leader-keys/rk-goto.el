;;; rk-goto.el --- Shortcut commands to go to particular locations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defun rk-goto-init-file ()
  "Open the Emacs init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun rk-goto-personal-config ()
  "Open the personal configuration file."
  (interactive)
  (find-file "~/Sync/emacs/personal-config.el"))

(defun rk-goto-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))

(provide 'rk-goto)

;;; rk-goto.el ends here
