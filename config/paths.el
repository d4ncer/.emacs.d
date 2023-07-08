;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'recentf)
  (require 'use-package))

(require 'f)
(require 'gnus)

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

(defconst paths-custom-pkgs
  (f-join gnus-home-directory "code" "emacs-pkgs"))

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
         (extra-dirs (list))
         (custom-pkgs-dirs (f-directories paths-custom-pkgs))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (seq-filter #'file-directory-p (seq-uniq (append main-dirs subdirs custom-pkgs-dirs extra-dirs load-path)))))

    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

;; Org stuff

(defconst paths--cloud-dir (format "/Users/%s/Library/Mobile Documents/com~apple~CloudDocs" user-login-name))
(defconst paths--org-dir (f-join paths--cloud-dir "rkdev" "org"))

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

(defconst rk-bib--lib-dir (f-join paths--cloud-dir "rkdev" "bib_files"))
(defconst rk-bib--refs-file (f-join paths--org-dir "bib/zotero-lib.bib"))

(defconst rk-accounts--ledger-file (f-join paths--accounts-dir "base.ledger"))

(provide 'paths)

;;; paths.el ends here
