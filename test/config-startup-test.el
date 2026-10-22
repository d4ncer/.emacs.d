;;; config-startup-test.el --- Fresh process activation gate -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -L test -l test/config-startup-test.el.

;;; Code:

(require 'config-test-support)
(dolist (symbol '(+ligatures-dir +templates-dir +first-input-hook +first-file-hook
                  +first-buffer-hook +switch-buffer-hook +switch-window-hook
                  +switch-frame-hook savehist-file save-place-file recentf-save-file))
  (eval (list 'defvar symbol nil)))
(let ((temporary (make-temp-file "config-startup-" t)))
  (unwind-protect
      (let ((user-emacs-directory (file-name-as-directory temporary))
            (savehist-file (expand-file-name "history" temporary))
            (save-place-file (expand-file-name "places" temporary))
            (recentf-save-file (expand-file-name "recentf" temporary))
            (+ligatures-dir (expand-file-name "ligatures" +config-test-root))
            (+templates-dir (expand-file-name "templates" +config-test-root))
            (+first-input-hook nil) (+first-file-hook nil) (+first-buffer-hook nil)
            (+switch-buffer-hook nil) (+switch-window-hook nil)
            (+switch-frame-hook nil))
        (dolist (file '("mod-keybindings" "mod-core" "mod-evil" "mod-completion"
                        "mod-editor" "mod-org"))
          (+config-test-load-module (concat "modules/" file ".el")))
        (dolist (feature '(org org-agenda vulpea vertico winner elec-pair))
          (when (featurep feature) (error "Eagerly loaded %s" feature)))
        (run-hooks 'pre-command-hook)
        (unless (and vertico-mode marginalia-mode global-corfu-mode)
          (error "First input did not enable completion"))
        (unless (featurep 'vertico-directory)
          (error "Vertico directory support missing"))
        (let ((file (expand-file-name "source.el" temporary)))
          (write-region ";;; fixture\n(message \"ok\")\n" nil file nil 'silent)
          (with-current-buffer (find-file-noselect file)
            (unless (and electric-pair-mode winner-mode)
              (error "First file did not activate editor services"))
            (kill-buffer)))
        (dolist (feature '(org org-agenda vulpea))
          (when (featurep feature) (error "Source file loaded %s" feature)))
        (let ((file (expand-file-name "note.org" temporary)))
          (write-region "* Fixture\n" nil file nil 'silent)
          (with-current-buffer (find-file-noselect file)
            (unless (and (derived-mode-p 'org-mode)
                         (bound-and-true-p +org-format-on-save-mode))
              (error "Org file configuration did not activate"))
            (kill-buffer)))
        (savehist-mode -1)
        (save-place-mode -1)
        (recentf-mode -1)
        (message "First input, source file, and Org file: PASS"))
    (delete-directory temporary t)))

(provide 'config-startup-test)
;;; config-startup-test.el ends here
