;;; run-ert.el --- Run chosen test files as the gate does -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l test/run-ert.el FILE... [--select REGEXP]
;; FILE is a test file's path or its name under test/, such as
;; elisp-tools-test.  Sources win over older bytecode, stale local bytecode
;; is recompiled first, and Emacs exits 0 only when no selected test fails.

;;; Code:

(setq load-prefer-newer t)
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'config-test-support)
(+config-test-recompile-stale)

(let ((selector t) files)
  (while command-line-args-left
    (let ((argument (pop command-line-args-left)))
      (if (equal argument "--select")
          (setq selector (or (pop command-line-args-left)
                             (error "--select needs a regexp")))
        (push argument files))))
  (unless files
    (error "Usage: emacs -Q --batch -l test/run-ert.el FILE... \
[--select REGEXP]"))
  (dolist (file (nreverse files))
    (unless (featurep (intern (file-name-base file)))
      (load (if (file-name-directory file) (expand-file-name file) file)
            nil t)))
  (ert-run-tests-batch-and-exit selector))

(provide 'run-ert)
;;; run-ert.el ends here
