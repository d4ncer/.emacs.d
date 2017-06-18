;;; rk-go-run.el --- Run commands for go  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(defgroup rk-go-run nil
  "Commands for interacting with tests in go."
  :group 'languages
  :prefix "rk-go-run-")

(defcustom rk-go-run-use-gocheck? t
  "Whether to use gocheck. If nil, fall back to `-run`."
  :group 'rk-go-run
  :type 'boolean)

(defconst rk-go-run-main-buffer "*go run*")
(defconst rk-go-run-test-buffer "*go test*")

(defun rk-go-run--run-test (str)
  (let ((compilation-buffer-name-function (lambda (_) rk-go-run-test-buffer)))
    (compile (concat "go test " str))))

;;;###autoload
(defun rk-go-run-tests (names)
  "Run all unit tests with NAMES."
  (interactive "sNames: ")
  (rk-go-run--run-test (shell-quote-argument names)))

;;;###autoload
(defun rk-go-run-package-tests ()
  "Run all tests in the package."
  (interactive)
  (rk-go-run--run-test ""))

;;;###autoload
(defun rk-go-run-package-tests-nested ()
  "Run all tests in this package and its enclosing packages."
  (interactive)
  (rk-go-run--run-test "./..."))

;;;###autoload
(defun rk-go-run-test-current-function ()
  "Run tests for the current function."
  (interactive)
  (unless (string-match "_test\\.go" buffer-file-name)
    (user-error "Must be in a _test.go file"))
  (save-excursion
    (let ((test-method (if rk-go-run-use-gocheck? "-check.f" "-run")))
      (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
      (rk-go-run--run-test (format "%s='%s'" test-method (match-string 2))))))

;;;###autoload
(defun rk-go-run-test-current-suite ()
  "Run current test suite."
  (interactive)
  (unless (string-match "_test\.go" buffer-file-name)
    (user-error "Must be in a _test.go file to run go-test-current-suite"))
  (unless rk-go-run-use-gocheck?
    (user-error "Gocheck is needed to test the current suite"))
  (save-excursion
    (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
    (rk-go-run--run-test (format "-check.f='%s'" (match-string 2)))))

;;;###autoload
(defun rk-go-run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (save-buffer)
  (let ((compilation-buffer-name-function (lambda (_) rk-go-run-main-buffer)))
    (compile (concat "go run " (shell-quote-argument (buffer-file-name))) t)))

(provide 'rk-go-run)

;;; rk-go-run.el ends here
