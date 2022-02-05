;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'recentf)
  (require 'use-package))

(require 'f)
(require 'gnus)

(defconst paths-tree-sitter-src-dir (f-join gnus-home-directory "code/elisp-tree-sitter")
  "Directory for elisp-tree-sitter source code.")

(defconst paths-tree-sitter-load-paths (seq-map (lambda (dir) (f-join paths-tree-sitter-src-dir dir)) (list "core" "lisp" "langs"))
  "Load paths for tree-sitter")

(defconst paths-evil-textobj-src-dir (f-join gnus-home-directory "code/evil-textobj-tree-sitter")
  "Directory for evil-textobj-tree-sitter source code.")

(defconst paths-cache-directory
  (concat user-emacs-directory "var"))

(defconst paths-etc-directory
  (concat user-emacs-directory "etc"))

(defconst paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst paths-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst paths-config-directory
  (concat user-emacs-directory "config"))

(defconst paths-hacks-directory
  (concat user-emacs-directory "hacks"))

(defconst paths-themes-directory
  (concat user-emacs-directory "themes"))

(defun paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.
If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-directory
                paths-config-directory
                paths-hacks-directory
                paths-themes-directory))
         (extra-dirs (append paths-tree-sitter-load-paths `(,paths-evil-textobj-src-dir)))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (seq-filter #'file-directory-p (seq-uniq (append main-dirs subdirs extra-dirs load-path)))))

    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

;; Org stuff

(defconst paths--dropbox-dir "~/Dropbox")
(defconst paths--org-dir (f-join paths--dropbox-dir "org"))

(defconst paths--gtd-dir (f-join paths--org-dir "gtd"))
(defconst paths--life-dir (f-join paths--org-dir "life"))
(defconst paths--accounts-dir (f-join paths--org-dir "accounts"))

(defconst rk-org--data-dir (f-join paths--org-dir "data"))

(defconst rk-org--roam-dir (f-join paths--org-dir "roam"))
(defconst rk-org--roam-dailies-dir (f-join rk-org--roam-dir "daily"))
(defconst rk-org--roam-temporal-prefix "%<%Y%m%d%H%M%S>")
(defconst rk-org--roam-inbox (f-join rk-org--roam-dir "20220128063937-inbox.org"))
(defconst rk-org--roam-refs-dir (f-join rk-org--roam-dir "references/"))

(defconst rk-org--roam-review-cache-file (f-join paths--org-dir ".org-roam-review"))

(defconst rk-bib--lib-dir (f-join paths--dropbox-dir "bib_files"))
(defconst rk-bib--refs-file (f-join paths--org-dir "bib/references.bib"))

(defconst rk-accounts--ledger-file (f-join paths--accounts-dir "base.ledger"))

(provide 'paths)

;;; paths.el ends here
