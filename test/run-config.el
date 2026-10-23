;;; run-config.el --- Complete configuration verification -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l test/run-config.el
;; Each integration gate runs in a fresh process with disposable state.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(defconst +config-check-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar +config-check-tests 0)
(defconst +config-check-emacs (expand-file-name invocation-name invocation-directory))

(defun +config-check-process (name &rest arguments)
  "Run isolated Emacs ARGUMENTS for gate NAME."
  (with-temp-buffer
    (let ((status (apply #'call-process +config-check-emacs nil t nil
                         "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                         "-L" (expand-file-name "test" +config-check-root)
                         arguments)))
      (unless (and (integerp status) (zerop status))
        (princ (buffer-string))
        (error "%s failed (%s)" name status))
      (goto-char (point-min))
      (while (re-search-forward "^Ran \\([0-9]+\\) tests" nil t)
        (cl-incf +config-check-tests (string-to-number (match-string 1))))
      (princ (format "%s: PASS\n" name)))))

(let ((default-directory +config-check-root))
  (dolist (file (append (directory-files +config-check-root t "\\.el\\'")
                        (directory-files-recursively "modules" "\\.el\\'")
                        (directory-files-recursively "lisp" "\\.el\\'")
                        (directory-files-recursively "test" "\\.el\\'")))
    (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens)))
  (princ "Elisp syntax: PASS\n")
  (+config-check-process
   "Core, activation, caching, compatibility, locks and lifecycle"
   "-l" "config-test-support" "-l" "config-core-test"
   "-l" "config-profile-test" "-l" "config-loader-test"
   "-l" "config-modeline-test" "-l" "config-activation-test"
   "-l" "config-compat-test" "-l" "config-lock-test"
   "-l" "config-grep-test" "-l" "agent-shell-projects-lifecycle-test"
   "-l" "project-files-lifecycle-test"
   "-l" "agent-shell-notify-test" "-l" "agent-shell-mermaid-test"
   "-l" "agent-shell-pstack-test" "-l" "agent-shell-drive-test"
   "-l" "agent-shell-pstack-panel-test" "-l" "config-compile-local-test"
   "-f" "ert-run-tests-batch-and-exit")
  (+config-check-process "Native syntax configuration" "-l" "config-treesit-test"
                         "-f" "ert-run-tests-batch-and-exit")
  (+config-check-process "First input and file activation" "-l" "config-startup-test")
  (+config-check-process "Headless capture" "-l" "config-headless-test")
  (+config-check-process "File finder loading" "-l" "project-files-startup-test")
  (+config-check-process "Agent projects loading" "-l" "agent-shell-projects-startup-test")
  (+config-check-process "File finder tests and compilation" "-l" "run-project-files")
  (+config-check-process "Legacy worker paths" "-l" "config-relocation-test")
  (+config-check-process "Agent project tests and compilation" "-l" "run-agent-shell-projects")
  (+config-check-process "pstack panels over ACP" "-l" "run-agent-shell-pstack")
  (+config-check-process "agent-shell workarounds over ACP" "-l" "run-agent-shell-perf")
  (+config-check-process "Helper compilation" "-l" "config-compile-test")
  (princ (format "All configuration gates passed; %d ERT tests.\n" +config-check-tests)))

(provide 'run-config)
;;; run-config.el ends here
