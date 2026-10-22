;;; project-files-lifecycle-test.el --- Finder lifecycle checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify idempotent registration and cleanup of actual worker processes.

;;; Code:

(require 'config-test-support)
(require '+project-files)

(ert-deftest +project-files-test-lifecycle-registration ()
  (let ((+project-files--states (make-hash-table :test #'equal))
        (+project-files--searches (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (+project-files-setup)
          (+project-files-setup)
          (should (= 1 (cl-count #'+project-files--after-save after-save-hook)))
          (should (advice-member-p #'+project-files--watch-directories
                                   'projectile--watch-directories)))
      (+project-files-teardown))
    (should-not (memq #'+project-files--after-save after-save-hook))
    (should-not (advice-member-p #'+project-files--watch-directories
                                 'projectile--watch-directories))))

(ert-deftest +project-files-test-lifecycle-cancels-index-and-search ()
  (let* ((+project-files--states (make-hash-table :test #'equal))
         (+project-files--searches (make-hash-table :test #'eq))
         (request (make-temp-file "finder-request-"))
         (pending (concat request ".pending"))
         (buffer (generate-new-buffer " *index-lifecycle*"))
         (process (make-process :name "finder-lifecycle" :buffer buffer
                                :command '("sh" "-c" "exec sleep 10")
                                :noquery t :sentinel #'ignore))
         (search (make-process :name "search-lifecycle"
                               :command '("sh" "-c" "exec sleep 10")
                               :noquery t :sentinel #'ignore))
         (timer (run-at-time 100 nil #'ignore))
         (state (+project-files--make-state :root "/fixture/" :process process
                                            :request request :timeout timer)))
    (unwind-protect
        (progn
          (with-temp-file pending (insert "partial index"))
          (puthash "/fixture/" state +project-files--states)
          (puthash search t +project-files--searches)
          (+project-files-teardown)
          (should-not (process-live-p process))
          (should-not (process-live-p search))
          (should-not (buffer-live-p buffer))
          (should-not (file-exists-p request))
          (should-not (file-exists-p pending))
          (should-not (memq timer timer-list))
          (should-not (+project-files--state-process state)))
      (cancel-timer timer)
      (dolist (proc (list process search))
        (when (process-live-p proc) (delete-process proc)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (dolist (file (list request pending))
        (when (file-exists-p file) (delete-file file))))))

(provide 'project-files-lifecycle-test)
;;; project-files-lifecycle-test.el ends here
