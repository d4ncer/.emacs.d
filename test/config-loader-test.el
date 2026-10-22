;;; config-loader-test.el --- Deferred loading regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise retries and one-shot dispatch rather than macro expansion shape.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+corelib)
(require '+use-package)
(+use-package-setup)
(defvar +config-loader-first-hook nil)
(defvar +config-loader-second-hook nil)
(defvar +config-loader-once-hook nil)
(defun +config-loader-trigger () "Test function trigger." nil)

(ert-deftest +config-loader-retry-and-all-trigger-cleanup ()
  (let ((+after-call--registrations (make-hash-table :test #'eq))
        +config-loader-first-hook +config-loader-second-hook
        (attempts 0) (real-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (eq feature '+config-fake-package)
                     (when (= (cl-incf attempts) 1) (error "First load failed"))
                   (apply real-require feature args)))))
      (+after-call-register '+config-fake-package
                            '(+config-loader-first-hook +config-loader-trigger))
      (should-error (run-hooks '+config-loader-first-hook))
      (should (= (hash-table-count +after-call--registrations) 1))
      (+config-loader-trigger)
      (should (= attempts 2))
      (should-not +config-loader-first-hook)
      (should (zerop (hash-table-count +after-call--registrations))))))

(ert-deftest +config-loader-reregistration-replaces-triggers ()
  (let ((+after-call--registrations (make-hash-table :test #'eq))
        +config-loader-first-hook +config-loader-second-hook)
    (+after-call-register '+config-fake-package '(+config-loader-first-hook))
    (+after-call-register '+config-fake-package '(+config-loader-second-hook))
    (should-not +config-loader-first-hook)
    (should (= (length +config-loader-second-hook) 1))
    (+after-call--remove (gethash '+config-fake-package +after-call--registrations))))

(ert-deftest +config-loader-once-retries-after-error ()
  (let ((after-init-time t) +config-loader-first-hook +config-loader-second-hook
        +config-loader-once-hook (attempts 0))
    (add-hook '+config-loader-once-hook
              (lambda () (when (= (cl-incf attempts) 1) (error "Retry me"))))
    (+run-hook-once '+config-loader-once-hook
                   '(+config-loader-first-hook +config-loader-second-hook))
    (should-error (run-hooks '+config-loader-first-hook))
    (run-hooks '+config-loader-first-hook)
    (run-hooks '+config-loader-second-hook)
    (should (= attempts 2))
    (should-not +config-loader-first-hook)
    (should-not +config-loader-second-hook)))

(ert-deftest +config-loader-once-guards-initialization-and-recursion ()
  (let (after-init-time +config-loader-first-hook +config-loader-once-hook
        (calls 0))
    (add-hook '+config-loader-once-hook
              (lambda () (cl-incf calls) (run-hooks '+config-loader-first-hook)))
    (+run-hook-once '+config-loader-once-hook '(+config-loader-first-hook))
    (run-hooks '+config-loader-first-hook)
    (should (zerop calls))
    (setq after-init-time t)
    (run-hooks '+config-loader-first-hook)
    (should (= calls 1))
    (should-not +config-loader-first-hook)))

(provide 'config-loader-test)
;;; config-loader-test.el ends here
