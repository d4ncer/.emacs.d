;;; +use-package.el --- Deferred activation triggers -*- lexical-binding: t; -*-

;;; Commentary:
;; Add :after-call to use-package. Timed loading uses its standard :defer.

;;; Code:

(require 'cl-lib)
(require 'use-package-core)

(cl-defstruct (+after-call--state (:constructor +after-call--make))
  package triggers callback running)

(defvar +after-call--registrations (make-hash-table :test #'eq))

(defun +after-call--hook-p (symbol)
  "Return non-nil if SYMBOL names a hook variable."
  (string-match-p "-\\(?:functions\\|hook\\)\\'" (symbol-name symbol)))

(defun +after-call--remove (state)
  "Remove every trigger belonging to STATE."
  (dolist (trigger (+after-call--state-triggers state))
    (if (+after-call--hook-p trigger)
        (remove-hook trigger (+after-call--state-callback state))
      (advice-remove trigger (+after-call--state-callback state))))
  (remhash (+after-call--state-package state) +after-call--registrations))

(defun +after-call--load (state &rest _)
  "Load STATE's package, retaining its triggers if loading fails."
  (unless (+after-call--state-running state)
    (setf (+after-call--state-running state) t)
    (unwind-protect
        (let ((default-directory user-emacs-directory))
          (require (+after-call--state-package state))
          (+after-call--remove state))
      (setf (+after-call--state-running state) nil))))

(defun +after-call-register (package triggers)
  "Load PACKAGE on the first successful invocation of any TRIGGERS.
Repeated registration replaces the old triggers. Errors remain visible
and can be retried by invoking a trigger again."
  (when-let* ((previous (gethash package +after-call--registrations)))
    (+after-call--remove previous))
  (unless (featurep package)
    (let ((state (+after-call--make :package package
                                    :triggers (delete-dups (copy-sequence triggers)))))
      (setf (+after-call--state-callback state)
            (apply-partially #'+after-call--load state))
      (puthash package state +after-call--registrations)
      (dolist (trigger (+after-call--state-triggers state))
        (if (+after-call--hook-p trigger)
            (add-hook trigger (+after-call--state-callback state))
          (advice-add trigger :before (+after-call--state-callback state)))))))

(defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)

(defun use-package-handler/:after-call (name _keyword triggers rest state)
  "Arrange deferred activation of NAME through TRIGGERS."
  (use-package-concat
   (unless (plist-get state :demand)
     `((+after-call-register ',name ',triggers)))
   (use-package-process-keywords name rest state)))

(defun +use-package-setup ()
  "Register the :after-call keyword once."
  (cl-pushnew :after-call use-package-deferring-keywords)
  (unless (memq :after-call use-package-keywords)
    (setq use-package-keywords
          (use-package-list-insert :after-call use-package-keywords :after))))

(provide '+use-package)
;;; +use-package.el ends here
