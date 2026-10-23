;;; agent-shell-pstack-panel-acp-test.el --- pstack panels over real ACP -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end panels: a pstack lead and its members are real agent shells
;; talking ACP to test/fixtures/agent-shell-projects-agent.py.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require '+agent-shell-pstack-panel)

(defvar +agent-shell-pstack-panel-acp-test--fixture
  (expand-file-name "fixtures/agent-shell-projects-agent.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun +agent-shell-pstack-panel-acp-test--config (identifier &rest params)
  "Return a fixture agent config named IDENTIFIER, passing PARAMS."
  (agent-shell-make-agent-config
   :identifier identifier :mode-line-name (symbol-name identifier)
   :buffer-name (format "Panel %s" identifier) :shell-prompt "Fixture> "
   :shell-prompt-regexp "Fixture> "
   :client-maker
   (lambda (buffer)
     (acp-make-client :context-buffer buffer
                      :command (executable-find "python3")
                      :command-params
                      (cons +agent-shell-pstack-panel-acp-test--fixture
                            params)))))

(defun +agent-shell-pstack-panel-acp-test--wait-for (predicate)
  "Wait up to 15 seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.03)))
  (should (funcall predicate)))

(defmacro +agent-shell-pstack-panel-acp-test--with-lead (&rest body)
  "Run BODY with a ready pstack lead shell bound to `lead'.
Agents `fixture-codex' and `fixture-claude' have read-only modes;
`fixture-stubborn' refuses to change mode.  BODY also sees `cwd' and
`reports', the panel report root."
  (declare (indent 0))
  `(let* ((cwd (file-name-as-directory
                (file-truename (make-temp-file "pstack-lead-" t))))
          (reports (make-temp-file "pstack-reports-" t))
          (agent-shell-show-welcome-message nil)
          (shell-maker-prompt-before-killing-buffer nil)
          (agent-shell-show-config-icons nil)
          (agent-shell-prefer-viewport-interaction nil)
          (agent-shell-mcp-servers nil)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-agent-configs
           (list (+agent-shell-pstack-panel-acp-test--config 'fixture-codex)
                 (+agent-shell-pstack-panel-acp-test--config 'fixture-claude)
                 (+agent-shell-pstack-panel-acp-test--config
                  'fixture-stubborn "--refuse-set-mode")))
          (+agent-shell-drive-read-only-modes
           '((fixture-codex . "read-only") (fixture-claude . "plan")
             (fixture-stubborn . "read-only")))
          (+agent-shell-pstack-invocations '((fixture-codex . "$poteto-mode")))
          (+agent-shell-pstack-panel-directory reports)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-drive--watchers (make-hash-table :test #'equal))
          (+agent-shell-drive--shells (make-hash-table :test #'eq))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (lead nil))
     (unwind-protect
         (save-window-excursion
           (+agent-shell-pstack-panel-mode 1)
           (setq lead (+agent-shell-drive-start :config 'fixture-codex
                                                :directory cwd))
           (+agent-shell-pstack-panel-acp-test--wait-for
            (lambda () (and (agent-shell-session-id :shell-buffer lead)
                            (eq (agent-shell-status :shell-buffer lead)
                                'ready))))
           (with-current-buffer lead (+agent-shell-pstack-mode 1))
           ,@body)
       (+agent-shell-pstack-panel-mode -1)
       (dolist (panel (+agent-shell-pstack-panel-list))
         (when-let* ((timer (+agent-shell-pstack-panel-timer panel)))
           (cancel-timer timer))
         (dolist (member (+agent-shell-pstack-panel-members panel))
           (+agent-shell-drive-kill
            (+agent-shell-pstack-panel-member-shell member))))
       (+agent-shell-drive-kill lead)
       (delete-directory cwd t)
       (delete-directory reports t))))

(defun +agent-shell-pstack-panel-acp-test--request (lead json &optional observe)
  "Prompt LEAD to request the panel JSON and wait for its follow-up.
Call OBSERVE while waiting.  Assert that no member shell was shown on the
way."
  (let ((shown (window-buffer (selected-window))))
    (should (+agent-shell-drive-send lead (concat "PSTACK-LEAD " json)))
    (+agent-shell-pstack-panel-acp-test--wait-for
     (lambda ()
       (when observe (funcall observe))
       (seq-some #'+agent-shell-pstack-panel-delivered
                 (+agent-shell-pstack-panel-list))))
    (should (eq shown (window-buffer (selected-window))))
    (should (= 1 (length (window-list)))))
  (car (+agent-shell-pstack-panel-list)))

(defun +agent-shell-pstack-panel-acp-test--report (member)
  "Return the contents of MEMBER's report file."
  (with-temp-buffer
    (insert-file-contents (+agent-shell-pstack-panel-member-report member))
    (buffer-string)))

(defun +agent-shell-pstack-panel-acp-test--members-gone (panel)
  "Wait until every member shell of PANEL is gone."
  (+agent-shell-pstack-panel-acp-test--wait-for
   (lambda () (not (seq-some (lambda (member)
                               (buffer-live-p
                                (+agent-shell-pstack-panel-member-shell
                                 member)))
                             (+agent-shell-pstack-panel-members panel))))))

(defun +agent-shell-pstack-panel-acp-test--lead-saw (lead text)
  "Wait until LEAD's buffer shows TEXT."
  (+agent-shell-pstack-panel-acp-test--wait-for
   (lambda () (with-current-buffer lead
                (save-excursion
                  (goto-char (point-min))
                  (search-forward text nil t))))))

(ert-deftest +agent-shell-pstack-panel-acp-test-how ()
  "Run a two-agent how panel read-only in the lead's directory.
Members stay out of `agent-shell-buffers' and the lead's mode line shows
their progress."
  (+agent-shell-pstack-panel-acp-test--with-lead
    (let* ((live-members nil) (listed-members nil) (progress nil)
           (observe
            (lambda ()
              (dolist (panel (+agent-shell-pstack-panel-list))
                (dolist (member (+agent-shell-pstack-panel-members panel))
                  (let ((shell (+agent-shell-pstack-panel-member-shell member)))
                    (when (buffer-live-p shell)
                      (cl-pushnew shell live-members)
                      (when (memq shell (agent-shell-buffers))
                        (cl-pushnew shell listed-members))))))
              (when-let* ((item (buffer-local-value
                                 '+agent-shell-pstack-panel--mode-line lead)))
                (cl-pushnew (substring-no-properties item) progress
                            :test #'equal))))
           (panel (+agent-shell-pstack-panel-acp-test--request
                   lead "{\"version\":1,\"shape\":\"how\",\"brief\":\"Explain \
the drive.\",\"members\":[{\"role\":\"data-flow\",\"agent\":\"fixture-codex\"},\
{\"role\":\"risks\",\"agent\":\"fixture-claude\"}]}" observe))
           (members (+agent-shell-pstack-panel-members panel))
           (lead-cwd (directory-file-name
                      (with-current-buffer lead (agent-shell-cwd)))))
      (should (equal '(done done)
                     (mapcar #'+agent-shell-pstack-panel-member-state members)))
      (should (equal (list (format "REPORT cwd=%s mode=read-only" lead-cwd)
                           (format "REPORT cwd=%s mode=plan" lead-cwd))
                     (mapcar #'+agent-shell-pstack-panel-acp-test--report
                             members)))
      (should (equal (list (expand-file-name
                            (format "%s/data-flow.md"
                                    (+agent-shell-pstack-panel-id panel))
                            reports)
                           (expand-file-name
                            (format "%s/risks.md"
                                    (+agent-shell-pstack-panel-id panel))
                            reports))
                     (mapcar #'+agent-shell-pstack-panel-member-report
                             members)))
      (should (= 2 (length live-members)))
      (should-not listed-members)
      (should (memq lead (agent-shell-buffers)))
      (should (member "panel 0/2 " progress))
      (should-not (buffer-local-value '+agent-shell-pstack-panel--mode-line
                                      lead))
      (+agent-shell-pstack-panel-acp-test--lead-saw lead "TRIAGED 2")
      (+agent-shell-pstack-panel-acp-test--members-gone panel))))

(ert-deftest +agent-shell-pstack-panel-acp-test-timeout ()
  "Settle a hung member at the deadline with its partial text."
  (+agent-shell-pstack-panel-acp-test--with-lead
    (let* ((panel (+agent-shell-pstack-panel-acp-test--request
                   lead "{\"version\":1,\"shape\":\"interrogate\",\"brief\":\
\"HANG\",\"timeout\":1,\"members\":[{\"role\":\"hung\",\"agent\":\
\"fixture-claude\"}]}"))
           (member (car (+agent-shell-pstack-panel-members panel))))
      (should (eq 'timeout (+agent-shell-pstack-panel-member-state member)))
      (should (equal "Partial before hang."
                     (+agent-shell-pstack-panel-acp-test--report member)))
      (should (string-match-p
               "## hung (fixture-claude): timeout\nReport: .*\nNo report \
within the 1-second timeout\\.\nPartial text:\n\nPartial before hang\\."
               (+agent-shell-pstack-panel-followup panel)))
      (+agent-shell-pstack-panel-acp-test--lead-saw lead "TRIAGED 1")
      (+agent-shell-pstack-panel-acp-test--members-gone panel))))

(ert-deftest +agent-shell-pstack-panel-acp-test-mode-mismatch ()
  "Refuse a member whose agent stays outside its read-only mode."
  (+agent-shell-pstack-panel-acp-test--with-lead
    (let* ((panel (+agent-shell-pstack-panel-acp-test--request
                   lead "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"stubborn\",\"agent\":\"fixture-stubborn\"}]}"))
           (member (car (+agent-shell-pstack-panel-members panel))))
      (should (eq 'error (+agent-shell-pstack-panel-member-state member)))
      (should (equal "Session mode is default, not the read-only mode \
read-only"
                     (+agent-shell-pstack-panel-member-error member)))
      (should-not (+agent-shell-pstack-panel-member-report member))
      (+agent-shell-pstack-panel-acp-test--lead-saw lead "TRIAGED 1")
      (+agent-shell-pstack-panel-acp-test--members-gone panel))))

(ert-deftest +agent-shell-pstack-panel-acp-test-rejected-plan ()
  "Reject a member's mode switch and report the plan it carried."
  (+agent-shell-pstack-panel-acp-test--with-lead
    (let* ((panel (+agent-shell-pstack-panel-acp-test--request
                   lead "{\"version\":1,\"shape\":\"how\",\"brief\":\
\"ASK-PERMISSION\",\"members\":[{\"role\":\"planner\",\"agent\":\
\"fixture-claude\"}]}"))
           (member (car (+agent-shell-pstack-panel-members panel))))
      (should (eq 'done (+agent-shell-pstack-panel-member-state member)))
      (should (equal "# Plan\nRead the drive library, then report on its \
watchers."
                     (+agent-shell-pstack-panel-acp-test--report member)))
      (+agent-shell-pstack-panel-acp-test--lead-saw lead "TRIAGED 1"))))

(provide 'agent-shell-pstack-panel-acp-test)
;;; agent-shell-pstack-panel-acp-test.el ends here
