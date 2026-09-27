;;; agent-shell-review-load-test.el --- Review source loading tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Check that evaluating the sidebar source does not recursively load the
;; coordinator, which in turn loads the sidebar.

;;; Code:

(require 'ert)

(defconst agent-shell-review-load-test--ui-file
  (expand-file-name "../lisp/agent-shell-review-ui.el"
                    (file-name-directory load-file-name))
  "Sidebar source file in this checkout.")

(ert-deftest agent-shell-review-load-test-ui-eval ()
  "Evaluating the UI source must not load its coordinator recursively."
  (let* ((output (generate-new-buffer " *review-ui-load*"))
         (expression
          (format "(with-temp-buffer (insert-file-contents %S) (eval-buffer))"
                  agent-shell-review-load-test--ui-file))
         (process
          (make-process
           :name "review-ui-load" :buffer output :noquery t
           :command (list (expand-file-name invocation-name invocation-directory)
                          "-Q" "--batch" "--eval" expression)))
         (deadline (+ (float-time) 3)))
    (unwind-protect
        (progn
          (while (and (process-live-p process)
                      (< (float-time) deadline))
            (accept-process-output process 0.1))
          (ert-info ((with-current-buffer output (buffer-string)))
            (should-not (process-live-p process))
            (should (eq (process-status process) 'exit))
            (should (zerop (process-exit-status process)))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer output))))

(provide 'agent-shell-review-load-test)
;;; agent-shell-review-load-test.el ends here
