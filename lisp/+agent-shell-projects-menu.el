;;; +agent-shell-projects-menu.el --- Relocated package entry -*- lexical-binding: t; -*-

;;; Commentary:
;; Preserve the old entry point for callers and running Emacs sessions.

;;; Code:

(let ((directory (expand-file-name "agent-shell-projects/"
                                   (file-name-directory load-file-name))))
  (add-to-list 'load-path directory)
  (load (expand-file-name "+agent-shell-projects-menu.el" directory) nil t))

(provide '+agent-shell-projects-menu)
;;; +agent-shell-projects-menu.el ends here
