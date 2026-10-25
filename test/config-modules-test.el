;;; config-modules-test.el --- Whole-configuration load checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs -Q --batch -L test -l test/config-modules-test.el
;; -f ert-run-tests-batch-and-exit.  Loads early-init.el and every module in
;; init.el order against the installed package builds.

;;; Code:

(require 'config-test-support)

(defun +config-modules-test--init-modules ()
  "Return the module names init.el requires, in order."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" +config-test-root))
    (let (modules)
      (while (re-search-forward "^(require '\\(mod-[a-z-]+\\))" nil t)
        (push (match-string 1) modules))
      (nreverse modules))))

(defun +config-modules-test--load-early-init ()
  "Load early-init.el as Emacs would, from this checkout."
  (let ((user-emacs-directory +config-test-root))
    (load (expand-file-name "early-init.el" +config-test-root) nil t)))

(defun +config-modules-test--load-all ()
  "Load early-init.el and every module; return warnings and errors raised."
  (let ((problems nil)
        (temporary (make-temp-file "config-modules-" t)))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _)
                 (push (format "%s: %s" type message) problems))))
      (unwind-protect
          (let ((user-emacs-directory (file-name-as-directory temporary)))
            (dolist (file (file-expand-wildcards
                           (expand-file-name "elpaca/builds/*/*-autoloads.el"
                                             +config-test-root)))
              (load file nil t))
            (unless (fboundp 'elpaca) (defmacro elpaca (&rest _) nil))
            (+config-modules-test--load-early-init)
            (dolist (module (+config-modules-test--init-modules))
              (condition-case err
                  (+config-test-load-module (concat "modules/" module ".el"))
                (error (push (format "%s: %S" module err) problems))))
            (dolist (hook '(after-init-hook emacs-startup-hook pre-command-hook))
              (condition-case err (run-hooks hook)
                (error (push (format "%s: %S" hook err) problems)))))
        (delete-directory temporary t)))
    (nreverse problems)))

(defvar +config-modules-test--problems 'unloaded
  "Problems from loading every module once, or `unloaded'.")

(defun +config-modules-test--ensure-loaded ()
  "Load every module once per process; return the problems raised."
  (when (eq +config-modules-test--problems 'unloaded)
    (setq +config-modules-test--problems (+config-modules-test--load-all)))
  +config-modules-test--problems)

(defun +config-modules-test--definitions ()
  "Return a table of function names defined by any library on `load-path'."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (directory load-path table)
      (when (file-directory-p directory)
        (dolist (file (directory-files directory t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents file)
            (while (re-search-forward
                    "^\\s-*(\\(?:[^ ()\n]*def[^ ()\n]*\\) '?\\([^ ()\n]+\\)" nil t)
              (puthash (intern (match-string 1)) t table))))))))

(defun +config-modules-test--leader-commands ()
  "Return commands bound under the leader map or sharp-quoted in its module."
  (let (commands)
    (cl-labels ((walk (map)
                  (map-keymap (lambda (_event binding)
                                (cond ((keymapp binding) (walk binding))
                                      ((and binding (symbolp binding))
                                       (push binding commands))))
                              map)))
      (walk (symbol-function '+leader-key)))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/mod-keybindings.el" +config-test-root))
      (while (re-search-forward "#'\\([^ ()\n]+\\)" nil t)
        (push (intern (match-string 1)) commands)))
    (delete-dups commands)))

(ert-deftest +config-modules-test-every-module-loads-cleanly ()
  (should (>= (length (+config-modules-test--init-modules)) 20))
  (should-not (+config-modules-test--ensure-loaded)))

(ert-deftest +config-modules-test-leader-commands-exist ()
  (+config-modules-test--ensure-loaded)
  (let ((definitions (+config-modules-test--definitions)))
    (should-not (seq-remove (lambda (command)
                              (or (fboundp command) (gethash command definitions)))
                            (+config-modules-test--leader-commands)))))

(ert-deftest +config-modules-test-theme-follows-dark-system ()
  (require '+theme)
  (let ((+theme-light 'batppuccin-latte))
    (+config-modules-test--load-early-init)
    (cl-letf (((symbol-function '+system-theme-query) (lambda (_) "Dark")))
      (+theme-update))
    (should (equal custom-enabled-themes (list +theme-dark)))
    (should +theme-dark)))

(provide 'config-modules-test)
;;; config-modules-test.el ends here
