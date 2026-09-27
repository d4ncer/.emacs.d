;;; agent-shell-review-protocol-test.el --- Reviewer protocol tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Validate review-only prompts and strict reviewer results.

;;; Code:

(require 'ert)
(require 'agent-shell-review-protocol nil t)

(ert-deftest agent-shell-review-protocol-test-questions ()
  "A question result carries the ambiguous criterion and reason."
  (let* ((root temporary-file-directory)
         (questions-json
          "{\"kind\":\"questions\",\"items\":[{\"question\":\"Which API?\",\"criterion\":\"API compatibility\",\"why\":\"Two callers differ\"}]}"))
    (let ((result (agent-shell-review-protocol-parse questions-json root)))
      (should (eq (plist-get result :kind) 'questions))
      (should (equal (plist-get (car (plist-get result :items)) :criterion)
                     "API compatibility")))))

(ert-deftest agent-shell-review-protocol-test-priorities ()
  "Findings retain all priorities and exact actionable fields."
  (let* ((root (make-temp-file "review-protocol-" t))
         (json
          "```json\n{\"kind\":\"findings\",\"items\":[{\"priority\":\"P1\",\"file\":\"src/a.el\",\"line\":12,\"title\":\"Wrong value\",\"evidence\":\"Returns nil\",\"requirement\":\"Must return a path\",\"suggestion\":\"Return the resolved path\"}]}\n```"))
    (unwind-protect
        (let* ((findings (agent-shell-review-protocol-parse json root))
               (item (car (plist-get findings :items))))
          (should (eq (plist-get findings :kind) 'findings))
          (should (equal (plist-get item :priority) "P1"))
          (should (equal (plist-get item :file) "src/a.el"))
          (dolist (priority '("P0" "P1" "P2" "P3"))
            (let* ((variant (replace-regexp-in-string
                             "P1" priority json nil t))
                   (parsed (agent-shell-review-protocol-parse variant root)))
              (should (equal (plist-get (car (plist-get parsed :items))
                                      :priority)
                             priority)))))
      (delete-directory root t))))

(ert-deftest agent-shell-review-protocol-test-clear ()
  "A clear result has no items and the prompt includes all inputs."
  (let* ((root temporary-file-directory)
         (snapshot '(:diff "DIFF_SENTINEL" :base "main"))
         (source '(:kind file :source "/tmp/original.md"
                         :text "REQUIREMENT_SENTINEL"))
         (answers '(("Which API?" . "CLARIFICATION_SENTINEL")))
         (prompt (agent-shell-review-protocol-prompt
                  snapshot source answers)))
    (should (eq (plist-get
                 (agent-shell-review-protocol-parse
                  "{\"kind\":\"clear\",\"items\":[]}" root)
                 :kind)
                'clear))
    (dolist (part '("DIFF_SENTINEL" "REQUIREMENT_SENTINEL"
                    "CLARIFICATION_SENTINEL" "ask" "Do not edit"))
      (should (string-match-p part prompt)))))

(ert-deftest agent-shell-review-protocol-test-malformed ()
  "Reject unsafe paths, missing evidence, mixed kinds, and invalid lines."
  (let* ((root (make-temp-file "review-protocol-" t))
         (good
          "{\"kind\":\"findings\",\"items\":[{\"priority\":\"P1\",\"file\":\"src/a.el\",\"line\":12,\"title\":\"Wrong\",\"evidence\":\"Observed nil\",\"requirement\":\"Must return path\",\"suggestion\":\"Return it\"}]}")
         (unsafe-path-json
          (replace-regexp-in-string "src/a.el" "../escape.el" good nil t)))
    (unwind-protect
        (progn
          (should-error (agent-shell-review-protocol-parse
                         unsafe-path-json root))
          (should-error (agent-shell-review-protocol-parse
                         (replace-regexp-in-string
                          "Observed nil" "" good nil t) root))
          (should-error (agent-shell-review-protocol-parse
                         (replace-regexp-in-string
                          "\"line\":12" "\"line\":0" good nil t)
                         root))
          (should-error (agent-shell-review-protocol-parse
                         (replace-regexp-in-string
                          "\"kind\":\"findings\""
                          "\"kind\":\"questions\"" good nil t)
                         root))
          (should-error (agent-shell-review-protocol-parse
                         "{\"kind\":\"clear\",\"items\":null}" root))
          (should-error (agent-shell-review-protocol-parse "not json" root)))
      (delete-directory root t))))

(provide 'agent-shell-review-protocol-test)
;;; agent-shell-review-protocol-test.el ends here
