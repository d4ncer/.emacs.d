;;; config-grep-test.el --- Native editable export regression -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise actual Embark export, live source changes, and save on finish.

;;; Code:

(require 'config-test-support)
(require 'embark-consult)
(require '+grep-edit)

(ert-deftest +config-grep-embark-export-saves-only-edited-sources ()
  (let* ((directory (make-temp-file "config-grep-" t))
         (file (expand-file-name "fixture.txt" directory))
         (default-directory directory)
         export source)
    (unwind-protect
        (save-window-excursion
          (write-region "old\n" nil file nil 'silent)
          (+grep-edit-mode 1)
          (embark-consult--export-grep
           :header "Exported results:\n\n" :lines nil
           :insert (lambda (_) (insert "fixture.txt:1:old\n") 1)
           :footer #'ignore)
          (setq export (current-buffer))
          (grep-change-to-grep-edit-mode)
          (goto-char (point-min))
          (search-forward "old")
          (replace-match "new")
          (setq source (find-buffer-visiting file))
          (should (equal (with-current-buffer source (buffer-string)) "new\n"))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "old\n"))
          (grep-edit-save-changes)
          (should (derived-mode-p 'grep-mode))
          (should-not +grep-edit--sources)
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "new\n")))
      (+grep-edit-mode -1)
      (when (buffer-live-p export) (kill-buffer export))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory directory t))))

(provide 'config-grep-test)
;;; config-grep-test.el ends here
