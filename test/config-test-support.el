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
(require '+compile-local)
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

(defun +config-test-recompile-stale ()
  "Recompile local libraries whose existing bytecode is stale.
Bytecode inlines other libraries' struct accessors, which
`load-prefer-newer' cannot see change; see `+compile-local-stale-files'.
A library without bytecode loads from source, so it is left alone.
Signal an error, with the compiler's output, if one fails."
  (when-let* ((process
               (+compile-local-compile
                (seq-filter (lambda (file) (file-exists-p (concat file "c")))
                            (+compile-local-stale-files +lisp-dir)))))
    (while (process-live-p process)
      (accept-process-output process 0.1))
    (unless (zerop (process-exit-status process))
      (error "Recompiling stale libraries failed:\n%s"
             (with-current-buffer (process-buffer process)
               (buffer-string))))))

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

(defun +config-test-async-state ()
  "Return the live processes and pending timers, for a test's teardown."
  (list (process-list) (append timer-list timer-idle-list nil)))

(defun +config-test--async-new (state)
  "Return the processes and timers started since STATE, as (PROCESSES TIMERS)."
  (list (seq-remove (lambda (process) (memq process (car state)))
                    (process-list))
        (seq-remove (lambda (timer)
                      (or (memq timer (cadr state))
                          (eq (timer--function timer)
                              'undo-auto--boundary-timer)))
                    (append timer-list timer-idle-list nil))))

(defun +config-test-async-quiet-p (state)
  "Return whether no process or timer started since STATE is pending."
  (pcase-let ((`(,processes ,timers) (+config-test--async-new state)))
    (not (or (seq-some #'process-live-p processes) timers))))

(defun +config-test-stop-async (state)
  "Cancel each timer and kill each process started since STATE.
No sentinel runs, so nothing a test started acts after it; wait up to
ten seconds for the killed processes to exit."
  (pcase-let* ((`(,processes ,timers) (+config-test--async-new state))
               (pids (delq nil (mapcar #'process-id processes)))
               (deadline (+ (float-time) 10)))
    (mapc #'cancel-timer timers)
    (dolist (process processes)
      (set-process-sentinel process #'ignore)
      (set-process-filter process #'ignore)
      (delete-process process)
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))
    (cl-flet ((running ()
                (seq-filter (lambda (pid)
                              (when-let* ((attributes (process-attributes pid)))
                                (not (equal "Z" (alist-get 'state attributes)))))
                            pids)))
      (while (and (running) (< (float-time) deadline))
        (accept-process-output nil 0.02))
      (when-let* ((left (running)))
        (error "Processes %S outlived their test" left)))))

(defvar +agent-shell-sandbox-tmp-parent)

(with-eval-after-load '+agent-shell-sandbox
  (unless (+config-test-capable-p 'tmp)
    (setq +agent-shell-sandbox-tmp-parent
          (directory-file-name temporary-file-directory))))

(provide 'config-test-support)
;;; config-test-support.el ends here
