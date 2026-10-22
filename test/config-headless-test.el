;;; config-headless-test.el --- Service dependency boundary -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture a real disposable workspace without loading the review UI.

;;; Code:

(require 'config-test-support)
(require '+agent-shell-projects-capture)

(dolist (feature '(+agent-shell-projects +agent-shell-projects-code
                  +agent-shell-projects-review magit-section agent-shell org))
  (when (featurep feature) (error "Capture service loaded %s" feature)))
(let* ((directory (make-temp-file "headless-project-" t))
       (+agent-shell-projects-directory (expand-file-name "store/" directory))
       (root (expand-file-name "workspace/" directory))
       (default-directory directory))
  (unwind-protect
      (progn
        (unless (zerop (call-process "git" nil nil nil "init" "-q" root))
          (error "Git init failed"))
        (with-temp-file (expand-file-name "file.txt" root) (insert "before\n"))
        (dolist (args `(("-C" ,root "add" ".")
                        ("-C" ,root "-c" "user.name=Fixture"
                         "-c" "user.email=fixture@example.invalid"
                         "-c" "commit.gpgsign=false" "commit" "-qm" "Initial")))
          (unless (zerop (apply #'call-process "git" nil nil nil args))
            (error "Git fixture failed")))
        (with-temp-file (expand-file-name "file.txt" root) (insert "after\n"))
        (let* ((project (+agent-shell-projects-create "Headless" root))
               (artefact (+agent-shell-projects-enrol-code project root))
               (deadline (+ (float-time) 20)))
          (while (and (not (plist-get artefact :current)) (< (float-time) deadline))
            (accept-process-output nil 0.02))
          (unless (plist-get artefact :current)
            (error "Headless capture failed: %s" (plist-get artefact :capture-error))))
        (when (featurep 'magit-section) (error "Capture activated UI"))
        (message "Headless workspace capture: PASS"))
    (+agent-shell-projects-teardown)
    (delete-directory directory t)))

(provide 'config-headless-test)
;;; config-headless-test.el ends here
