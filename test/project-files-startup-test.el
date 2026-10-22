;;; project-files-startup-test.el --- Finder startup gate -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l test/project-files-startup-test.el

;;; Code:

(let* ((root (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (user-emacs-directory (make-temp-file "project-files-startup-" t)))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (add-to-list 'load-path (expand-file-name "lisp/project-files/" root))
  (dolist (package '("projectile" "compat"))
    (add-to-list 'load-path
                 (expand-file-name (concat "elpaca/builds/" package) root)))
  (require 'use-package)
  (require 'use-package-ensure)
  (unwind-protect
      (let ((use-package-ensure-function #'ignore))
        (load (expand-file-name "modules/mod-project.el" root) nil t)
        (unless (featurep '+project-files)
          (error "Finder setup did not load"))
        (dolist (feature '(consult vertico orderless))
          (when (featurep feature) (error "Eagerly loaded %s" feature)))
        (unless (commandp '+project-find-file)
          (error "Finder command is unavailable"))
        (unless (advice-member-p #'+project-files--watch-directories
                                 'projectile--watch-directories)
          (error "Bounded watch counting is inactive"))
        (message "Startup setup and deferred completion packages: PASS"))
    (when (fboundp 'projectile-mode) (projectile-mode -1))
    (delete-directory user-emacs-directory t)))

(provide 'project-files-startup-test)
;;; project-files-startup-test.el ends here
