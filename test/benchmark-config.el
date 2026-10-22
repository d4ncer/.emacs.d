;;; benchmark-config.el --- Native syntax activation measurements -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l test/benchmark-config.el
;; This isolates syntax activation from LSP, filesystem and personal hooks.

;;; Code:

(require 'benchmark)
(require 'treesit)
(unless (treesit-language-available-p 'python)
  (error "Install the configured Python grammar before benchmarking"))
(setq treesit-font-lock-level 3)
(let ((source (mapconcat
               (lambda (i) (format "def function_%d(value):\n    return value + %d\n" i i))
               (number-sequence 1 300) "\n")))
  (princ
   (format "%S\n"
           (list :emacs emacs-version :build system-configuration
                 :fixture-functions 300
                 :first (benchmark-run 1
                          (with-temp-buffer
                            (insert source) (python-ts-mode) (font-lock-ensure)))
                 :subsequent-20
                 (benchmark-run 20
                   (with-temp-buffer
                     (insert source) (python-ts-mode) (font-lock-ensure)))))))

(provide 'benchmark-config)
;;; benchmark-config.el ends here
