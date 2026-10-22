;;; config-compat-test.el --- Upstream workaround regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Protocol fixtures and actual snippet insertion define adapter removal gates.

;;; Code:

(require 'config-test-support)
(require '+expert-compat)
(require '+eglot-tempel-compat)

(ert-deftest +config-compat-expert-only-protocol-changes ()
  (cl-letf (((symbol-function 'object-of-class-p) (lambda (&rest _) t))
            ((symbol-function 'eglot--server-info)
             (lambda (server) (list :name (if (eq server 'expert) "Expert" "Other")))))
    (should (equal (+expert-omit-exit-params '(expert :exit nil))
                   '(expert :exit :jsonrpc-omit)))
    (should (equal (+expert-omit-exit-params '(other :exit nil)) '(other :exit nil)))
    (should (equal (+expert-omit-exit-params '(expert :initialized nil))
                   '(expert :initialized nil)))
    (with-temp-buffer
      (let ((eglot--recent-changes '((1 1 "d"))))
        (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'other)))
          (+expert-force-full-sync)
          (should (listp eglot--recent-changes)))
        (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'expert)))
          (+expert-force-full-sync)
          (should (eq eglot--recent-changes :emacs-messup)))))))

(ert-deftest +config-compat-snippet-follows-mode-indentation ()
  (let* ((snippet "(when t\n        (message \"ok\")\n        )")
         (prefix "(progn\n  ")
         (expected (with-temp-buffer
                     (emacs-lisp-mode)
                     (insert prefix snippet)
                     (indent-region (point-min) (point-max))
                     (buffer-string))))
    (unwind-protect
        (progn
          (+eglot-tempel-compat-mode 1)
          (+eglot-tempel-compat-mode 1)
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert prefix)
            (tempel-insert (eglot-tempel--convert snippet))
            (should (equal (buffer-string) expected))))
      (+eglot-tempel-compat-mode -1))
    (should-not (advice-member-p #'+eglot-tempel-reindent 'eglot-tempel--convert))))

(ert-deftest +config-compat-expert-lifecycle ()
  (unwind-protect
      (progn
        (+expert-compat-mode 1)
        (+expert-compat-mode 1)
        (should (= 1 (cl-count #'+expert-pin-toolchain eglot-managed-mode-hook))))
    (+expert-compat-mode -1))
  (should-not (advice-member-p #'+expert-force-full-sync
                               'eglot--signal-textDocument/didChange)))

(provide 'config-compat-test)
;;; config-compat-test.el ends here
