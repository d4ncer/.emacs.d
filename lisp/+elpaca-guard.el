;;; +elpaca-guard.el --- Keep Elpaca from building in shared checkouts -*- lexical-binding: t; -*-

;;; Commentary:
;; Required once Elpaca loads.  In a checkout that shares packages it does
;; not own, a linked git worktree or one whose elpaca directory links
;; elsewhere, a build records that checkout's paths, which break once it
;; goes; see `+bootstrap-shared-packages'.  There existing builds still
;; load, but an order that would clone, build, rebuild or update a package
;; fails saying why, and deleting one refuses.

;;; Code:

(require 'cl-lib)
(require 'elpaca)

(declare-function +bootstrap-shared-packages "+bootstrap")

(defun +elpaca-guard-message (reason)
  "Return why Elpaca changes no package here, given REASON."
  (format "Elpaca builds, updates and deletes no package here: %s.  \
Do that from the checkout that owns the packages" reason))

(defun +elpaca-guard--refuse (e)
  "Fail order E, which would change packages this checkout shares."
  (let ((text (+elpaca-guard-message (+bootstrap-shared-packages))))
    (message "%s: %s" (elpaca<-id e) text)
    (elpaca--fail e text)))

(cl-defmethod elpaca-build-steps :around (_e &optional _context)
  "Replace steps beyond loading a build with a refusal in a shared checkout."
  (let ((steps (cl-call-next-method)))
    (if (and (cl-set-difference steps elpaca--pre-built-steps)
             (+bootstrap-shared-packages))
        (list #'+elpaca-guard--refuse)
      steps)))

(cl-defmethod elpaca--delete :around (_e)
  "Refuse to delete a package in a shared checkout."
  (if-let* ((reason (+bootstrap-shared-packages)))
      (user-error "%s" (+elpaca-guard-message reason))
    (cl-call-next-method)))

(provide '+elpaca-guard)
;;; +elpaca-guard.el ends here
