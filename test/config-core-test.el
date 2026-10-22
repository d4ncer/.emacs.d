;;; config-core-test.el --- Core configuration regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Consequential editing and hook behavior, independent of package startup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+corelib)
(require '+edit-cmds)

(ert-deftest +config-delete-respects-kill-query ()
  (let* ((file (make-temp-file "config-delete-"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions (list (lambda () nil))))
            (+delete-file-and-buffer)
            (should (buffer-live-p buffer))
            (should (file-exists-p file))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +config-delete-after-successful-kill ()
  (let* ((file (make-temp-file "config-delete-"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (+delete-file-and-buffer))
          (should-not (buffer-live-p buffer))
          (should-not (file-exists-p file)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +config-face-multiple-and-repeated-ancestors ()
  (dolist (face '(+config-face-a +config-face-b +config-face-c))
    (make-face face))
  (set-face-attribute '+config-face-a nil
                      :inherit '(+config-face-b +config-face-c))
  (set-face-attribute '+config-face-b nil :inherit '+config-face-c)
  (should (equal (+face-ancestors '+config-face-a)
                 '(+config-face-a +config-face-b +config-face-c)))
  (should-not (+face-ancestors 'unspecified)))

(ert-deftest +config-face-cycle-terminates ()
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face &rest _)
               (if (eq face 'default) 'shadow 'default))))
    (should (equal (+face-ancestors 'default) '(default shadow)))))

(provide 'config-core-test)
;;; config-core-test.el ends here
