;;; +keybindings.el --- Shared binding declarations -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the local leader macro available without installing global bindings.

;;; Code:

(defmacro +local-leader-set-key (keymaps &rest general-args)
  "Define local leader bindings in KEYMAPS using GENERAL-ARGS."
  (declare (indent 1))
  `(general-define-key :prefix "," :states '(normal motion)
                       :keymaps ,keymaps ,@general-args))

(provide '+keybindings)
;;; +keybindings.el ends here
