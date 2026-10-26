;;; config-treesit-test.el --- Native syntax configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Test source pins, remapping and grammar availability without downloading.

;;; Code:

(require 'config-test-support)
(+config-test-load-module "modules/mod-lsp.el")
(+config-test-load-module "modules/mod-languages.el")

(ert-deftest +config-treesit-native-mappings-and-sources ()
  (should-not (featurep 'treesit-auto))
  (should (eq (alist-get 'python-mode major-mode-remap-alist) 'python-ts-mode))
  (should (eq treesit-auto-install-grammar 'ask))
  (dolist (language '(bash c cpp css elixir heex html java javascript jsdoc json
                          markdown markdown-inline nix python rust tsx typescript yaml))
    (let ((source (alist-get language treesit-language-source-alist)))
      (should (string-prefix-p "https://" (car source)))
      (should-not (plist-member (cdr source) :commit)))))

(ert-deftest +config-treesit-nix-declined-and-available ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'treesit-ensure-installed) (lambda (_) nil)))
      (+nix-mode)
      (should (eq major-mode 'conf-unix-mode))))
  (with-temp-buffer
    (let (activated)
      (cl-letf (((symbol-function 'treesit-ensure-installed) (lambda (_) t))
                ((symbol-function 'nix-ts-mode) (lambda () (setq activated t))))
        (+nix-mode)
        (should activated)))))

(provide 'config-treesit-test)
;;; config-treesit-test.el ends here
