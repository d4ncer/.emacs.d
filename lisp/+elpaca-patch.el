;;; +elpaca-patch.el --- el-patch fixes to Elpaca -*- lexical-binding: t; -*-

;;; Commentary:
;; Required once `elpaca-menu-melpa' loads.  Kept out of +bootstrap.el,
;; which loads before el-patch: eager macroexpansion there would expand
;; inside the patch and break `el-patch-validate'.

;;; Code:

(require 'el-patch)
(require 'elpaca-menu-melpa)

(el-patch-defun elpaca-menu-melpa--clone (path)
  (el-patch-concat
    "Clone MELPA recipes repo to PATH."
    (el-patch-add
      "\nPulls `main', which MELPA renamed from `master'.  Remove this
patch once https://github.com/progfolio/elpaca/issues/585 is fixed."))
  (let ((default-directory path))
    (make-directory path t)
    (make-directory (expand-file-name ".git/info/" path) t)
    (message "Downloading MELPA recipes...")
    (let* ((process-environment (append '("GIT_CONFIG_SYSTEM=/dev/null"
                                          "GIT_CONFIG_GLOBAL=/dev/null")
                                        process-environment))
           (processes
            (list
             (elpaca-process-call "git" "init")
             (elpaca-process-call "git" "config" "core.sparseCheckout" "true")
             (with-temp-buffer
               (insert "recipes")
               (append-to-file (point-min) (point-max)
                               (expand-file-name ".git/info/sparse-checkout" path)))
             (elpaca-process-call "git" "remote" "add" "origin" "https://github.com/melpa/melpa.git")
             (elpaca-process-call "git" "pull" "--depth=1" "origin"
                                  (el-patch-swap "master" "main"))
             (elpaca-process-call "git" "checkout" (el-patch-swap "master" "main"))
             (elpaca-process-call "git" "branch" "--set-upstream-to"
                                  (el-patch-swap "origin/master" "origin/main")
                                  (el-patch-swap "master" "main"))))
           (err (car (cl-remove-if #'zerop (delq nil processes) :key #'car))))
      (when err (error "Unable to clone MELPA: %S" err))
      (message "Downloading MELPA recipes...100%%"))))

(provide '+elpaca-patch)
;;; +elpaca-patch.el ends here
