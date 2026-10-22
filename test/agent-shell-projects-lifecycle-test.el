;;; agent-shell-projects-lifecycle-test.el --- Lifecycle acceptance -*- lexical-binding: t; -*-

;;; Commentary:
;; Check real registrations, queued work and process teardown.

;;; Code:

(require 'config-test-support)
(require '+agent-shell-projects)
(require '+agent-shell-projects-jobs)
(require 'embark)

(ert-deftest +agent-shell-projects-test-setup-and-teardown ()
  (unwind-protect
      (progn
        (+agent-shell-projects-teardown)
        (should-not (memq #'+agent-shell-projects--window-changed
                          window-buffer-change-functions))
        (+agent-shell-projects-setup)
        (+agent-shell-projects-setup)
        (should (= 1 (cl-count #'+agent-shell-projects--window-changed
                              window-buffer-change-functions)))
        (should (= 1 (cl-count #'+agent-shell-projects-embark-target
                              embark-target-finders)))
        (+agent-shell-projects-mode 1)
        (let ((poll +agent-shell-projects--timer))
          (+agent-shell-projects-mode 1)
          (should (eq poll +agent-shell-projects--timer)))
        (+agent-shell-projects-run-at-time 100 nil #'ignore)
        (+agent-shell-projects-teardown)
        (should-not +agent-shell-projects--timer)
        (should (zerop (hash-table-count +agent-shell-projects--timers)))
        (should-not (memq #'+agent-shell-projects-embark-target embark-target-finders))
        (should-not (assq '+agent-shell-projects-passage embark-keymap-alist)))
    (+agent-shell-projects-teardown)))

(ert-deftest +agent-shell-projects-test-teardown-releases-workers ()
  (let* ((file (make-temp-file "project-worker-"))
         (queued-file (make-temp-file "project-queued-"))
         (buffer (generate-new-buffer " *worker-fixture*"))
         (process (make-process :name "project-teardown-fixture" :buffer buffer
                                :command '("sh" "-c" "exec sleep 10")
                                :noquery t :sentinel #'ignore))
         (calls 0))
    (unwind-protect
        (progn
          (puthash process (list :file file :callback (lambda (_) (cl-incf calls)))
                   +agent-shell-projects--workers)
          (setq +agent-shell-projects--worker-queue
                (list (list :file queued-file :callback (lambda (_) (cl-incf calls)))))
          (+agent-shell-projects-teardown)
          (should (= calls 2))
          (should-not (process-live-p process))
          (should-not (buffer-live-p buffer))
          (should-not (file-exists-p file))
          (should-not (file-exists-p queued-file))
          (should-not +agent-shell-projects--worker-queue)
          (should (zerop (hash-table-count +agent-shell-projects--workers))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (dolist (path (list file queued-file))
        (when (file-exists-p path) (delete-file path))))))

(provide 'agent-shell-projects-lifecycle-test)
;;; agent-shell-projects-lifecycle-test.el ends here
