;;; agent-shell-review-spec-test.el --- Requirement discovery tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise referenced files, candidate selection, and conversation fallback.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-review-spec nil t)

(ert-deftest agent-shell-review-spec-test-referenced-external-file ()
  "A transcript's external spec wins over project search candidates."
  (let* ((root (make-temp-file "review-project-" t))
         (external-root (make-temp-file "review-external-" t))
         (external-spec (expand-file-name "original-spec.md" external-root))
         (project-spec (expand-file-name "docs/specs/other-spec.md" root))
         (agent-shell-review-spec-search-roots nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory project-spec) t)
          (with-temp-file external-spec (insert "Original criteria"))
          (with-temp-file project-spec (insert "Other criteria"))
          (let ((context (format "## User\nImplement `%s`\n" external-spec)))
            (let ((result (agent-shell-review-spec-resolve root context)))
              (should (eq (plist-get result :kind) 'file))
              (should (equal (plist-get result :source) external-spec))
              (should (equal (plist-get result :text) "Original criteria")))
            (let ((result (agent-shell-review-spec-resolve
                           root context project-spec)))
              (should (equal (plist-get result :source) project-spec)))))
      (delete-directory root t)
      (delete-directory external-root t))))

(ert-deftest agent-shell-review-spec-test-ambiguous-candidates ()
  "Prompt once when equally credible project specs exist."
  (let* ((root (make-temp-file "review-project-" t))
         (spec-dir (expand-file-name "docs/specs" root))
         (agent-shell-review-spec-search-roots nil)
         (selection-count 0))
    (unwind-protect
        (progn
          (make-directory spec-dir t)
          (with-temp-file (expand-file-name "first-spec.md" spec-dir)
            (insert "First"))
          (with-temp-file (expand-file-name "second-spec.md" spec-dir)
            (insert "Second"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _args)
                       (cl-incf selection-count)
                       (car choices))))
            (let ((result (agent-shell-review-spec-resolve root nil)))
              (should (eq (plist-get result :kind) 'file))
              (should (= selection-count 1)))))
      (delete-directory root t))))

(ert-deftest agent-shell-review-spec-test-conversation-fallback ()
  "Use user instructions when no spec exists; reject missing or large input."
  (let* ((root (make-temp-file "review-project-" t))
         (agent-shell-review-spec-search-roots nil)
         (missing (expand-file-name "missing.md" root)))
    (unwind-protect
        (progn
          (let ((fallback (agent-shell-review-spec-resolve
                           root "## User (today)\nBuild the requested behavior.\n\n## Agent (today)\nI will.")))
            (should (eq (plist-get fallback :kind) 'conversation))
            (should (string-match-p "Build the requested behavior"
                                    (plist-get fallback :text))))
          (should-error (agent-shell-review-spec-resolve
                         root "criteria" missing) :type 'user-error)
          (let ((agent-shell-review-spec-max-bytes 8))
            (should-error (agent-shell-review-spec-resolve root "long criteria")
                          :type 'user-error)))
      (delete-directory root t))))

(ert-deftest agent-shell-review-spec-test-reference-quality ()
  "Ignore README references when a spec is named; prompt for two specs."
  (let* ((root (make-temp-file "review-spec-project-" t))
         (external (make-temp-file "review-source-" t))
         (readme (expand-file-name "README.md" root))
         (first (expand-file-name "first-spec.md" external))
         (second (expand-file-name "second-spec.md" external))
         (agent-shell-review-spec-search-roots nil)
         (transcript nil)
         (selection-count 0))
    (unwind-protect
        (progn
          (with-temp-file readme (insert "Project overview"))
          (with-temp-file first (insert "First criteria"))
          (with-temp-file second (insert "Second criteria"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _args)
                       (cl-incf selection-count)
                       (car choices))))
            (setq transcript
                  (format "Follow `README.md` and implement `%s`." first))
            (should (equal (plist-get
                            (agent-shell-review-spec-resolve root transcript)
                            :source)
                           first))
            (setq transcript (format "Compare `%s` and `%s`." first second))
            (should (eq (plist-get
                         (agent-shell-review-spec-resolve root transcript)
                         :kind)
                        'file))
            (should (= selection-count 1))))
      (delete-directory root t)
      (delete-directory external t))))

(ert-deftest agent-shell-review-spec-test-prompted-source ()
  "Ask for criteria when no file or conversation is available."
  (let* ((root (make-temp-file "review-empty-" t))
         (file (expand-file-name "criteria.md" root))
         (agent-shell-review-spec-search-roots nil)
         (choice "Text"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "File criteria"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt _choices &rest _args) choice))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _args) "First line\nSecond line"))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _args) file)))
            (let ((result (agent-shell-review-spec-resolve root nil)))
              (should (eq (plist-get result :kind) 'entered))
              (should (equal (plist-get result :source) "entered requirements"))
              (should (equal (plist-get result :text)
                             "First line\nSecond line")))
            (setq choice "File")
            (should (equal (plist-get (agent-shell-review-spec-resolve root nil)
                                    :text)
                           "File criteria"))
            (setq choice "Text")
            (cl-letf (((symbol-function 'read-from-minibuffer)
                       (lambda (&rest _args) "  ")))
              (should-error (agent-shell-review-spec-resolve root nil)
                            :type 'user-error))))
      (delete-directory root t))))

(provide 'agent-shell-review-spec-test)
;;; agent-shell-review-spec-test.el ends here
