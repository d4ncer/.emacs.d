;;; config-profile-test.el --- Measurement regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Check hook cleanup and measurement boundaries with deterministic counters.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+profile)

(ert-deftest +config-profile-next-command-lifecycle ()
  (let (pre-command-hook post-command-hook +profile--owned started stopped report)
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil))
              ((symbol-function 'profiler-start) (lambda (_) (setq started t)))
              ((symbol-function 'profiler-stop) (lambda () (setq stopped t)))
              ((symbol-function 'profiler-report) (lambda () (setq report t))))
      (+profile-next-command)
      (+profile-next-command)
      (let ((this-command '+profile-next-command))
        (run-hooks 'post-command-hook))
      (should-not started)
      (let ((this-command 'forward-char)) (run-hooks 'pre-command-hook))
      (should started)
      (run-hooks 'post-command-hook)
      (should stopped)
      (should report)
      (should-not pre-command-hook)
      (should-not post-command-hook))))

(ert-deftest +config-profile-cancel-cleans-armed-hooks ()
  (let (pre-command-hook post-command-hook +profile--owned)
    (cl-letf (((symbol-function 'profiler-running-p) (lambda () nil)))
      (+profile-next-command)
      (+profile-cancel)
      (should-not pre-command-hook)
      (should-not post-command-hook))))

(ert-deftest +config-benchmark-separates-wall-and-gc ()
  (let ((file (make-temp-file "config-benchmark-"))
        (gcs-done 10) (gc-elapsed 2.0))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _)
                     (cl-incf gcs-done 2)
                     (cl-incf gc-elapsed 0.25)
                     (generate-new-buffer " *benchmark*"))))
          (let ((samples (plist-get (+benchmark-find-file file '(4)) :samples)))
            (should (= (length samples) 4))
            (dolist (sample samples)
              (should (= (plist-get sample :gc-count) 2))
              (should (= (plist-get sample :gc-seconds) 0.25)))))
      (delete-file file))))

(ert-deftest +config-benchmark-preserves-existing-buffer ()
  (let* ((file (make-temp-file "config-benchmark-"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "unsaved")
          (should-error (+benchmark-find-file file) :type 'user-error)
          (should (buffer-live-p buffer))
          (should (equal (buffer-string) "unsaved")))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest +config-benchmark-retains-changed-fixture ()
  (let ((file (make-temp-file "config-benchmark-")) buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _)
                     (setq buffer (generate-new-buffer " *changed-fixture*"))
                     (with-current-buffer buffer (insert "keep this edit"))
                     buffer)))
          (should-error (+benchmark-find-file file 1) :type 'user-error)
          (should (buffer-live-p buffer))
          (should (equal (with-current-buffer buffer (buffer-string)) "keep this edit")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(provide 'config-profile-test)
;;; config-profile-test.el ends here
