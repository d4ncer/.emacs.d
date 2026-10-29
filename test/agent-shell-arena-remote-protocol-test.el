;;; agent-shell-arena-remote-protocol-test.el --- The remote protocol spec -*- lexical-binding: t; -*-

;;; Commentary:
;; Check that docs/PSTACK-REMOTE-PROTOCOL.md's generated sections match
;; what scripts/protocol-schemas.el renders from the record kinds and the
;; capability registry now.

;;; Code:

(require 'ert)
(load (expand-file-name "../scripts/protocol-schemas"
                        (file-name-directory (or load-file-name
                                                 buffer-file-name)))
      nil t)

(ert-deftest +agent-shell-arena-remote-protocol-test-spec-current ()
  "Every generated section of the spec is what the generator renders now."
  (when-let* ((drift (+protocol-schemas-drift
                      (+protocol-schemas--contents +protocol-schemas-file))))
    (ert-fail (format "%s drifted from the code: %s; run emacs -Q --batch \
-l scripts/protocol-schemas.el -f +protocol-schemas-batch"
                      (file-relative-name +protocol-schemas-file
                                          +protocol-schemas-root)
                      (string-join drift ", ")))))

(ert-deftest +agent-shell-arena-remote-protocol-test-drift-named ()
  "A changed, missing or unknown section is named by its record."
  (let* ((text (+protocol-schemas--contents +protocol-schemas-file))
         (job (cdr (assoc "record job" (+protocol-schemas-read text)))))
    (should-not (+protocol-schemas-drift text))
    (should (equal '("record job")
                   (+protocol-schemas-drift
                    (string-replace job (string-replace "| `brief` |"
                                                        "| `summary` |" job)
                                    text))))
    (let ((+agent-shell-arena-remote--kinds
           (cons '(extra ignore ignore (name "name" text))
                 +agent-shell-arena-remote--kinds)))
      (should (equal '("record extra") (+protocol-schemas-drift text)))
      (should-error (+protocol-schemas-update text)))
    (should (equal '("record stale")
                   (+protocol-schemas-drift
                    (concat text "<!-- BEGIN GENERATED record stale -->\n"
                            "<!-- END GENERATED record stale -->\n"))))
    (should (equal text (+protocol-schemas-update text)))
    (should (equal text (+protocol-schemas-update
                         (string-replace job "" text))))))

(ert-deftest +agent-shell-arena-remote-protocol-test-render ()
  "A record's section gives each field's key, type and presence."
  (let ((section (+protocol-schemas-render-kind 'request-member)))
    (should (string-prefix-p "#### `request-member`\n\n| Key |" section))
    (should (string-match-p
             "^| `kind` | one of `\"candidate\"`, `\"judge\"`, \
`\"delegate\"` | required |$"
             section))
    (should (string-match-p "^| `focus` | string | optional, absent or null |$"
                            section)))
  (should (string-match-p
           "^| `members` | list of \\[`member-state`\\](#member-state) | \
absent is empty |$"
           (+protocol-schemas-render-kind 'status)))
  (should (string-match-p "`\"version\"` from 1 to 1, and no `\"kind\"`"
                          (+protocol-schemas-render-kind 'ready)))
  (should (string-match-p "^| `egress_tshark` | tshark captures"
                          (+protocol-schemas-render-capabilities))))

(provide 'agent-shell-arena-remote-protocol-test)
;;; agent-shell-arena-remote-protocol-test.el ends here
