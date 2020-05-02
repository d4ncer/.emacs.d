;;; definers.el --- Main keybind definers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'general)

(general-create-definer rk-leader-def
  :states '(normal visual motion insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(general-create-definer rk-local-leader-def
  :states '(normal visual motion insert emacs)
  :keymaps 'override
  :prefix ","
  :non-normal-prefix "C-,")

(provide 'definers)

;;; definers.el ends here
