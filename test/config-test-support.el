;;; config-test-support.el --- Isolated configuration checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Load installed dependencies without running the package manager or init file.

;;; Code:

(require 'cl-lib)
(require 'ert)
(defconst +config-test-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "lisp" +config-test-root))
(dolist (directory (directory-files (expand-file-name "elpaca/builds" +config-test-root)
                                    t "^[^. ]"))
  (when (file-directory-p directory) (add-to-list 'load-path directory)))
(require '+bootstrap)
(+bootstrap-setup +config-test-root)
(require 'use-package-ensure)
(require 'general)
(setq use-package-ensure-function #'ignore)

(defun +config-test-declaration (file package)
  "Read PACKAGE's top-level use-package declaration from FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file +config-test-root))
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) package))
                (when-let* ((ensure (memq :ensure form)))
                  (setcar (cdr ensure) nil))
                (throw 'found form))))
        (end-of-file (error "Declaration not found: %s" package))))))

(defun +config-test-load-module (file)
  "Evaluate FILE with installed dependencies and package installation disabled."
  (require 'exec-path-from-shell)
  (require 'keychain-environment)
  (cl-letf (((symbol-function 'use-package-normalize/:ensure)
             (lambda (&rest _) '(nil)))
            ((symbol-function 'exec-path-from-shell-initialize) #'ignore)
            ((symbol-function 'keychain-refresh-environment) #'ignore))
    (load (expand-file-name file +config-test-root) nil t)))

(defun +config-test-view-keeps-other-buffers (open shown render &optional
                                                   refresh)
  "Check that a view never draws into a buffer it does not own.
OPEN shows the view in the selected window, SHOWN is its
`window-buffer-change-functions' entry and RENDER its renderer.
REFRESH, when given, is its redraw-if-open command.  The window then
shows another buffer, and each must leave that buffer untouched."
  (save-window-excursion
    (funcall open)
    (let ((window (selected-window))
          (other (generate-new-buffer " *view-other*")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (insert "a transcript")
              (set-buffer-modified-p nil))
            (set-window-buffer window other)
            (funcall shown window)
            (when refresh (funcall refresh))
            (with-current-buffer other
              (should-error (funcall render))
              (should (equal "a transcript" (buffer-string)))
              (should-not (buffer-modified-p))))
        (kill-buffer other)))))

(defun +config-test--probe-tmp ()
  "Return whether this process may make a folder in /tmp."
  (condition-case nil
      (progn (delete-directory (make-temp-file "/tmp/config-test-" t)) t)
    (file-error nil)))

(defun +config-test--probe-sandbox-exec ()
  "Return whether sandbox-exec, where installed, can confine a process."
  (or (not (file-executable-p "/usr/bin/sandbox-exec"))
      (eql 0 (ignore-errors
               (call-process "/usr/bin/sandbox-exec" nil nil nil
                             "-p" "(version 1)(allow default)"
                             "/usr/bin/true")))))

(defun +config-test--probe-ps ()
  "Return whether /bin/ps can list this process."
  (eql 0 (ignore-errors
           (call-process "/bin/ps" nil nil nil
                         "-o" "pid=" "-p" (number-to-string (emacs-pid))))))

(defun +config-test--probe-mmdc ()
  "Return t if the Mermaid CLI renders a trial diagram.
Return nil when it fails inside a sandbox, which forbids its browser,
and otherwise a message saying why it is missing or broken."
  (if-let* ((program (executable-find "mmdc")))
      (let* ((directory (make-temp-file "config-test-mmdc-" t))
             (input (expand-file-name "probe.mmd" directory))
             (output (expand-file-name "probe.png" directory)))
        (unwind-protect
            (with-temp-buffer
              (with-temp-file input (insert "flowchart TD\nA --> B\n"))
              (let ((status (ignore-errors
                              (call-process program nil t nil "-i" input
                                            "-o" output "--quiet"))))
                (cond ((and (eql 0 status) (file-exists-p output)) t)
                      ((not (+config-test-capable-p 'sandbox-exec)) nil)
                      (t (format "%s exited %s: %s" program status
                                 (string-trim (buffer-string)))))))
          (delete-directory directory t)))
    "mmdc is not on `exec-path'; install @mermaid-js/mermaid-cli"))

(defconst +config-test--capabilities
  '((tmp "making folders in /tmp" +config-test--probe-tmp)
    (sandbox-exec "starting sandbox-exec" +config-test--probe-sandbox-exec)
    (ps "running /bin/ps" +config-test--probe-ps)
    (mmdc "rendering with the Mermaid CLI" +config-test--probe-mmdc))
  "Each capability a test may need: (NAME DESCRIPTION PROBE).")

(defvar +config-test--probed nil
  "Alist of each probed capability and its probe's outcome.")

(defun +config-test--outcome (capability)
  "Return CAPABILITY's probe outcome, probing it once.
The outcome is t when it is there, nil when this process may not use it,
or a message saying why it is broken.  CAPABILITY names an entry of
`+config-test--capabilities'."
  (let ((cached (assq capability +config-test--probed)))
    (if cached
        (cdr cached)
      (let ((value (funcall (nth 2 (or (assq capability
                                             +config-test--capabilities)
                                       (error "Unknown capability %s"
                                              capability))))))
        (push (cons capability value) +config-test--probed)
        value))))

(defun +config-test-capable-p (capability)
  "Return whether this process has CAPABILITY; see `+config-test--outcome'."
  (eq t (+config-test--outcome capability)))

(defun +config-test-require (&rest capabilities)
  "Skip the current test unless this process may use every CAPABILITIES.
A capability that is broken rather than forbidden fails the test."
  (dolist (capability capabilities)
    (let ((outcome (+config-test--outcome capability)))
      (cond ((stringp outcome) (ert-fail outcome))
            ((not outcome)
             (ert-skip (format "Needs %s, which this process cannot do"
                               (nth 1 (assq capability
                                            +config-test--capabilities)))))))))

(defvar +agent-shell-sandbox-tmp-parent)

(with-eval-after-load '+agent-shell-sandbox
  (unless (+config-test-capable-p 'tmp)
    (setq +agent-shell-sandbox-tmp-parent
          (directory-file-name temporary-file-directory))))

(provide 'config-test-support)
;;; config-test-support.el ends here
