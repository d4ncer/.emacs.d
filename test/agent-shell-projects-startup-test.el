;;; agent-shell-projects-startup-test.el --- Cold startup gate -*- lexical-binding: t; -*-

;;; Commentary:
;; Evaluate the actual use-package declaration in a fresh Emacs process.

;;; Code:

(let ((root (file-name-directory
             (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp/" root))
  (add-to-list 'load-path (expand-file-name "lisp/agent-shell-projects/" root))
  (dolist (directory (directory-files (expand-file-name "elpaca/builds/" root)
                                     t "^[^. ]"))
    (when (file-directory-p directory) (add-to-list 'load-path directory)))
  (require 'use-package)
  (require 'general)
  (require 'evil)
  (general-auto-unbind-keys)
  (general-unbind :states '(normal motion) "SPC")
  (let (declaration)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "modules/mod-ai.el" root))
      (emacs-lisp-mode)
      (check-parens)
      (goto-char (point-min))
      (condition-case nil
          (while (not declaration)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) '+agent-shell-projects))
                (setq declaration form))))
        (end-of-file (error "Project declaration not found"))))
    (eval declaration t))
  (dolist (feature '(+agent-shell-projects agent-shell magit-section embark))
    (when (featurep feature) (error "Eagerly loaded %s" feature)))
  (unless (autoloadp (symbol-function '+agent-shell-projects))
    (error "Project entry command is not autoloaded"))
  (require '+agent-shell-projects)
  (when (or (featurep '+agent-shell-projects-code)
            (featurep '+agent-shell-projects-review))
    (error "Document entry eagerly loaded code review"))
  (unless (eq (lookup-key (evil-get-auxiliary-keymap
                           +agent-shell-projects-home-mode-map 'normal)
                          (kbd "RET")) '+agent-shell-projects-visit-at-point)
    (error "Home RET does not activate its target"))
  (message "Deferred activation and native home keys: PASS"))

(provide 'agent-shell-projects-startup-test)
;;; agent-shell-projects-startup-test.el ends here
