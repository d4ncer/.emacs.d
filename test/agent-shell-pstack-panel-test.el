;;; agent-shell-pstack-panel-test.el --- Tests for pstack panels -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests against a stubbed `+agent-shell-drive' surface.  Events reach
;; the panel through the watchers it registers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack-panel nil t)

(defmacro +agent-shell-pstack-panel-test--with (&rest body)
  "Run BODY with a pstack lead, stubbed drive functions and fresh panels.
BODY sees `lead', `directory' (the reports root), `cwd' (the lead's),
`sent' (newest first, each (SHELL . TEXT)), `started' (each (AGENT SHELL
POLICY CWD)), `interrupted' (shells, newest first), `send-ok' (whether
sends succeed), `start-error' (a start failure message), `partials' (alist
of shell to partial text), and the functions `lead-event' and
`member-event'."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "pstack-panels-" t))
          (cwd (file-name-as-directory (make-temp-file "pstack-lead-" t)))
          (+agent-shell-pstack-panel-directory directory)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-reminder-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (handlers (make-hash-table :test #'equal))
          (sent nil) (started nil) (interrupted nil) (send-ok t)
          (partials nil)
          (start-error nil)
          (lead (generate-new-buffer " *lead*")))
     (cl-letf (((symbol-function '+agent-shell-drive-read-only-agents)
                (lambda () '(codex claude-code)))
               ((symbol-function 'agent-shell-cwd) (lambda () cwd))
               ((symbol-function '+agent-shell-drive-watch)
                (lambda (_shell owner id function)
                  (puthash (cons owner id) function handlers)))
               ((symbol-function '+agent-shell-drive-unwatch)
                (lambda (owner &optional id)
                  (dolist (key (hash-table-keys handlers))
                    (when (and (eq (car key) owner)
                               (or (null id) (equal (cdr key) id)))
                      (remhash key handlers)))))
               ((symbol-function '+agent-shell-drive-send)
                (lambda (shell text)
                  (when send-ok (push (cons shell text) sent) t)))
               ((symbol-function '+agent-shell-drive-interrupt)
                (lambda (shell) (push shell interrupted)))
               ((symbol-function '+agent-shell-drive-partial-text)
                (lambda (shell) (alist-get shell partials)))
               ((symbol-function '+agent-shell-drive-kill)
                (lambda (shell &optional _reason)
                  (when (buffer-live-p shell) (kill-buffer shell))))
               ((symbol-function '+agent-shell-drive-start)
                (cl-function
                 (lambda (&key config directory policy)
                   (when start-error (user-error "%s" start-error))
                   (let ((shell (generate-new-buffer " *member*")))
                     (setq started (append started
                                           (list (list config shell policy
                                                       directory))))
                     shell)))))
       (cl-flet* ((lead-event (&rest event)
                    (funcall (gethash (cons 'pstack-lead lead) handlers)
                             (append event (list :shell lead))))
                  (member-event (role &rest event)
                    (let ((id (+agent-shell-pstack-panel-id
                               (car (last (+agent-shell-pstack-panel-list))))))
                      (funcall (gethash (cons 'pstack-panel (cons id role))
                                        handlers)
                               event))))
         (unwind-protect
             (progn
               (with-current-buffer lead
                 (setq-local +agent-shell-pstack-mode t))
               (+agent-shell-pstack-panel-mode 1)
               ,@body)
           (+agent-shell-pstack-panel-mode -1)
           (dolist (panel (+agent-shell-pstack-panel-list))
             (when-let* ((timer (+agent-shell-pstack-panel-timer panel)))
               (cancel-timer timer)))
           (accept-process-output nil 0.01)
           (dolist (entry started)
             (when (buffer-live-p (cadr entry)) (kill-buffer (cadr entry))))
           (kill-buffer lead)
           (delete-directory directory t)
           (delete-directory cwd t))))))

(defun +agent-shell-pstack-panel-test--message (json &optional prose)
  "Return a lead message of PROSE ending with a pstack-panel block of JSON."
  (format "%s\n\n```pstack-panel\n%s\n```\n" (or prose "Requesting a panel.")
          json))

(defconst +agent-shell-pstack-panel-test--how
  "{\"version\":1,\"shape\":\"how\",\"brief\":\"Explain the drive.\",\
\"timeout\":1,\"members\":[{\"role\":\"data-flow\",\"agent\":\"codex\",\
\"focus\":\"Events\"},{\"role\":\"risks\",\"agent\":\"claude-code\"}]}"
  "A valid two-member how request.")

(defun +agent-shell-pstack-panel-test--id (json)
  "Return the id of the panel requested with JSON in session s1."
  (substring (sha1 (concat "s1" "\0" json)) 0 12))

(defun +agent-shell-pstack-panel-test--followups (sent lead)
  "Return the texts in SENT that went to LEAD, oldest first."
  (reverse (mapcar #'cdr (seq-filter (lambda (entry) (eq (car entry) lead))
                                     sent))))

(ert-deftest +agent-shell-pstack-panel-test-block ()
  "Read the first complete pstack-panel block in the message."
  (should (equal "{\"a\":1}"
                 (+agent-shell-pstack-panel--block
                  "Text\n```pstack-panel\n{\"a\":1}\n```\n  \n")))
  (should (equal "{\"a\":1}"
                 (+agent-shell-pstack-panel--block
                  (concat "```pstack-panel\n{\"a\":1}\n```\nmore\n"
                          "```pstack-panel\n{\"b\":2}\n```"))))
  (should (equal "{\"a\":1}\n{\"x\":\"```\"}"
                 (+agent-shell-pstack-panel--block
                  (concat "```pstack-panel\n{\"a\":1}\n{\"x\":\"```\"}\n```\n"
                          "Then I continue."))))
  (should-not (+agent-shell-pstack-panel--block
               "Text ```pstack-panel\n{\"a\":1}\n```"))
  (should-not (+agent-shell-pstack-panel--block "```pstack-panel\n{\"a\":1}\n"))
  (should-not (+agent-shell-pstack-panel--block "```json\n{}\n```"))
  (should-not (+agent-shell-pstack-panel--block nil)))

(ert-deftest +agent-shell-pstack-panel-test-block-before-more-work ()
  "Start a panel whose lead kept working after the block, and interrupt it."
  (+agent-shell-pstack-panel-test--with
    (let ((text (concat (+agent-shell-pstack-panel-test--message
                         +agent-shell-pstack-panel-test--how)
                        "\nMeanwhile, more digging.")))
      (lead-event :kind 'tool-call-update :session "s1" :text "Reading.")
      (should-not started)
      (should-not interrupted)
      (lead-event :kind 'tool-call-update :session "s1" :text text
                  :title "Run git show")
      (should (= 2 (length started)))
      (should (equal (list lead) interrupted))
      (lead-event :kind 'tool-call-update :session "s1" :text text
                  :title "Run git show")
      (lead-event :kind 'turn-complete :session "s1" :text text)
      (should (= 2 (length started)))
      (should (equal (list lead) interrupted))
      (should (= 1 (length (+agent-shell-pstack-panel-list))))
      (member-event "data-flow" :kind 'init-finished)
      (member-event "risks" :kind 'init-finished)
      (member-event "data-flow" :kind 'turn-complete :text "A")
      (member-event "risks" :kind 'turn-complete :text "B")
      (should (string-prefix-p "Emacs interrupted your turn because you \
called Run git show after your pstack-panel block, so that call may not have \
finished.  Rerun it if you still need it.\n\npstack panel "
                               (cdar sent))))
    (lead-event :kind 'tool-call-update :session "s1" :title "Edit"
                :text (+agent-shell-pstack-panel-test--message "{bad"))
    (should (equal (list lead lead) interrupted))
    (should (string-prefix-p "Emacs interrupted your turn because you called \
Edit after your pstack-panel block, so that call may not have finished.  \
Rerun it if you still need it.\n\nEmacs rejected your pstack-panel request: \
The block is not valid JSON" (cdar sent)))
    (lead-event :kind 'turn-complete :session "s1"
                :text (concat (+agent-shell-pstack-panel-test--message
                               "{\"version\":1,\"shape\":\"how\",\
\"brief\":\"c\",\"members\":[{\"role\":\"a\",\"agent\":\"codex\"}]}")
                              "\nOne more thought."))
    (should (= 3 (length started)))
    (should (= 2 (length interrupted)))))

(ert-deftest +agent-shell-pstack-panel-test-validation ()
  "Accept a well-formed request and explain each rejection to the lead."
  (+agent-shell-pstack-panel-test--with
    (let ((panel (+agent-shell-pstack-panel--parse
                  "{\"version\":1,\"shape\":\"interrogate\",\"brief\":\"b\",\
\"members\":[{\"role\":\"r-1\",\"agent\":\"claude-code\"}]}")))
      (should (eq 'interrogate (+agent-shell-pstack-panel-shape panel)))
      (should (= 900 (+agent-shell-pstack-panel-timeout panel)))
      (should (eq 'claude-code
                  (+agent-shell-pstack-panel-member-agent
                   (car (+agent-shell-pstack-panel-members panel))))))
    (let ((member "{\"role\":\"a\",\"agent\":\"codex\"}"))
      (pcase-dolist
          (`(,json ,reason)
           `(("{not json" "not valid JSON")
             ("[1]" "JSON object")
             (,(format "{\"version\":2,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[%s]}" member) "Version must be 1")
             (,(format "{\"version\":1,\"shape\":\"arena\",\"brief\":\"b\",\
\"members\":[%s]}" member) "shapes: how, interrogate")
             (,(format "{\"version\":1,\"shape\":\"how\",\"brief\":\" \",\
\"members\":[%s]}" member) "Brief")
             (,(format "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"timeout\":3601,\"members\":[%s]}" member) "from 1 to 3600")
             ("{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\"members\":[]}"
              "1 to 4 members")
             (,(format "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[%s]}" (string-join (make-list 5 member) ",")) "1 to 4 members")
             ("{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"Bad Role\",\"agent\":\"codex\"}]}" "lowercase")
             ("{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"a\",\"agent\":\"gemini\"}]}"
              "available agents: codex, claude-code")
             (,(format "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[%s,%s]}" member member) "Role a appears more than once")))
        (let ((sent-before (length sent)))
          (lead-event :kind 'turn-complete :session "s1"
                      :text (+agent-shell-pstack-panel-test--message json))
          (should (= (1+ sent-before) (length sent)))
          (should (string-match-p (regexp-quote reason) (cdar sent)))
          (should (string-prefix-p
                   "Emacs rejected your pstack-panel request: "
                   (cdar sent))))))
    (should-not started)))

(ert-deftest +agent-shell-pstack-panel-test-followup-golden ()
  "Hand the lead every report inline with its path and the triage line."
  (+agent-shell-pstack-panel-test--with
    (let* ((json +agent-shell-pstack-panel-test--how)
           (id (+agent-shell-pstack-panel-test--id json))
           (reports (expand-file-name id directory)))
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message json))
      (should (equal (list (list 'codex 'read-only cwd)
                           (list 'claude-code 'read-only cwd))
                     (mapcar (lambda (entry)
                               (list (nth 0 entry) (nth 2 entry) (nth 3 entry)))
                             started)))
      (let ((codex (nth 1 (nth 0 started)))
            (claude (nth 1 (nth 1 started))))
        (member-event "data-flow" :kind 'prompt-ready)
        (should-not sent)
        (member-event "data-flow" :kind 'init-finished)
        (member-event "risks" :kind 'init-finished)
        (should (equal (list codex claude) (reverse (mapcar #'car sent))))
        (should (equal "You are the data-flow member of a pstack how panel \
that Emacs runs for another agent.  You are read-only: Emacs refuses edits and \
commands that change state.

Brief:
Explain the drive.

Your focus:
Events

Investigate the brief from your focus and explain what you find.  Cite \
file:line for every claim, and mark anything you inferred rather than read.  \
End your turn with the report as your final message: findings first, then \
open questions.  Nobody will answer questions during the turn."
                       (cdr (assq codex sent))))
        (member-event "data-flow" :kind 'turn-complete :text "Report A\n")
        (push (cons claude "Half done") partials)
        (should-not (+agent-shell-pstack-panel-test--followups sent lead))
        (accept-process-output nil 1.3)
        (accept-process-output nil 0.05)
        (should-not (buffer-live-p codex))
        (should-not (buffer-live-p claude))
        (should (equal
                 (list (format "pstack panel %s (how) has finished: 1 done, \
0 error, 1 timeout.

## data-flow (codex): done
Report: %s/data-flow.md

Report A

## risks (claude-code): timeout
Report: %s/risks.md
No report within the 1-second timeout.
Partial text:

Half done

Triage these reports as the how skill's synthesizer: reconcile conflicts, \
check each load-bearing claim against the code, then write the explanation."
                               id reports reports))
                 (+agent-shell-pstack-panel-test--followups sent lead)))
        (should (equal "Report A\n"
                       (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "data-flow.md" reports))
                         (buffer-string))))))))

(ert-deftest +agent-shell-pstack-panel-test-settles-once ()
  "Keep the first outcome of a member and deliver the follow-up once."
  (+agent-shell-pstack-panel-test--with
    (let* ((json (replace-regexp-in-string
                  ",{\"role\":\"risks\",\"agent\":\"claude-code\"}" ""
                  +agent-shell-pstack-panel-test--how t t))
           (report (expand-file-name
                    "data-flow.md"
                    (expand-file-name
                     (+agent-shell-pstack-panel-test--id json) directory))))
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message json))
      (member-event "data-flow" :kind 'init-finished)
      (member-event "data-flow" :kind 'turn-complete :text "First")
      (member-event "data-flow" :kind 'turn-complete :text "Second")
      (member-event "data-flow" :kind 'clean-up :reason "gone")
      (lead-event :kind 'turn-complete :session "s1" :text "Thanks.")
      (let ((followups (+agent-shell-pstack-panel-test--followups sent lead)))
        (should (= 1 (length followups)))
        (should (string-match-p "## data-flow (codex): done" (car followups)))
        (should-not (string-match-p "Second\\|gone" (car followups))))
      (should (equal "First" (with-temp-buffer
                               (insert-file-contents report)
                               (buffer-string)))))))

(ert-deftest +agent-shell-pstack-panel-test-member-errors ()
  "Report members whose shell failed to start or ended early."
  (+agent-shell-pstack-panel-test--with
    (let ((json "{\"version\":1,\"shape\":\"interrogate\",\"brief\":\"b\",\
\"members\":[{\"role\":\"a\",\"agent\":\"codex\"},\
{\"role\":\"b\",\"agent\":\"claude-code\"}]}"))
      (setq start-error "No codex here")
      (lead-event :kind 'turn-complete :session "s2"
                  :text (+agent-shell-pstack-panel-test--message json))
      (should (string-match-p "0 done, 2 error.*\n\n## a (codex): error\nNo codex here"
                              (cdar sent)))
      (setq start-error nil sent nil)
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message json))
      (member-event "a" :kind 'clean-up
                    :reason "Session mode is agent, not the read-only mode \
read-only")
      (member-event "b" :kind 'init-finished)
      (member-event "b" :kind 'clean-up)
      (let ((followup (car (+agent-shell-pstack-panel-test--followups
                            sent lead))))
        (should (string-match-p "has finished: 0 done, 2 error, 0 timeout"
                                followup))
        (should (string-match-p "## a (codex): error\nSession mode is agent"
                                followup))
        (should (string-match-p
                 "## b (claude-code): error\nThe member's shell ended"
                 followup))
        (should-not (string-match-p "Report:" followup))
        (should (string-suffix-p "Findings that several agents agree on are \
high signal." followup))))))

(ert-deftest +agent-shell-pstack-panel-test-delivery-retry ()
  "Retry a refused follow-up when the lead is ready, and send it once."
  (+agent-shell-pstack-panel-test--with
    (let ((json "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"a\",\"agent\":\"codex\"}]}"))
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message json))
      (member-event "a" :kind 'init-finished)
      (setq send-ok nil)
      (member-event "a" :kind 'turn-complete :text "Done")
      (should-not (+agent-shell-pstack-panel-test--followups sent lead))
      (setq send-ok t)
      (lead-event :kind 'prompt-ready)
      (should (= 1 (length (+agent-shell-pstack-panel-test--followups
                            sent lead))))
      (lead-event :kind 'turn-complete :session "s1" :text "Next")
      (lead-event :kind 'prompt-ready)
      (should (= 1 (length (+agent-shell-pstack-panel-test--followups
                            sent lead)))))))

(ert-deftest +agent-shell-pstack-panel-test-live-requests-only ()
  "Ignore restored sessions and requests already seen."
  (+agent-shell-pstack-panel-test--with
    (let ((text (+agent-shell-pstack-panel-test--message
                 +agent-shell-pstack-panel-test--how)))
      (lead-event :kind 'session-restored :session "s1" :text text)
      (should-not started)
      (lead-event :kind 'turn-complete :session "s1" :text text)
      (lead-event :kind 'turn-complete :session "s1" :text text)
      (should (= 2 (length started)))
      (should (= 1 (length (+agent-shell-pstack-panel-list)))))))

(ert-deftest +agent-shell-pstack-panel-test-plan-fallback ()
  "Use a rejected plan as the report when it says more than the text."
  (+agent-shell-pstack-panel-test--with
    (let ((json "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"a\",\"agent\":\"claude-code\"},\
{\"role\":\"b\",\"agent\":\"claude-code\"}]}"))
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message json))
      (member-event "a" :kind 'init-finished)
      (member-event "b" :kind 'init-finished)
      (member-event "a" :kind 'turn-complete :text "Plan below."
                    :plan "# Findings\nThe drive owns subscriptions.")
      (member-event "b" :kind 'turn-complete :text "The full report text."
                    :plan "Short")
      (let ((followup (car (+agent-shell-pstack-panel-test--followups
                            sent lead))))
        (should (string-match-p "done\nReport: .*\n\n# Findings\nThe drive"
                                followup))
        (should-not (string-match-p "Plan below" followup))
        (should (string-match-p "The full report text" followup))))))

(ert-deftest +agent-shell-pstack-panel-test-abandon-with-lead ()
  "Stop a panel's members when the lead's shell ends."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       +agent-shell-pstack-panel-test--how))
    (lead-event :kind 'clean-up)
    (accept-process-output nil 0.02)
    (should-not (seq-some #'buffer-live-p (mapcar #'cadr started)))
    (should (seq-every-p
             (lambda (member)
               (eq 'error (+agent-shell-pstack-panel-member-state member)))
             (+agent-shell-pstack-panel-members
              (car (+agent-shell-pstack-panel-list)))))))

(ert-deftest +agent-shell-pstack-panel-test-protocol ()
  "Explain the protocol on the priming prompt only, naming usable agents."
  (+agent-shell-pstack-panel-test--with
    (with-current-buffer lead
      (setq-local agent-shell--state
                  (list (cons :agent-config (list (cons :identifier 'codex)))))
      (let* ((request (lambda (text)
                        `((:method . "session/prompt")
                          (:params . ((sessionId . "s1")
                                      (prompt . [((type . "text")
                                                  (text . ,text))]))))))
             (texts (lambda (request)
                      (mapcar (lambda (block) (map-elt block 'text))
                              (map-nested-elt request '(:params prompt)))))
             (first (funcall texts (+agent-shell-pstack--decorate
                                    lead (funcall request "go"))))
             (second (funcall texts (+agent-shell-pstack--decorate
                                     lead (funcall request "again")))))
        (should (equal "$poteto-mode go" (car first)))
        (should (= 2 (length first)))
        (should (string-match-p "^Agents: codex, claude-code\\.  "
                                (cadr first)))
        (should (string-match-p "two or more members, mix agents" (cadr first)))
        (should (string-match-p "end your turn" (cadr first)))
        (should (string-match-p "Emacs sends you a prompt" (cadr first)))
        (should (equal "again" (car second)))
        (should (equal +agent-shell-pstack-reminder (cadr second)))
        (should (string-match-p
                 (concat "never on your own subagents.*The protocol is in "
                         (regexp-quote
                          (+agent-shell-pstack-panel-protocol-file)))
                 (caddr second)))
        (should (= 3 (length second)))
        (should (equal (cadr first)
                       (with-temp-buffer
                         (insert-file-contents
                          (+agent-shell-pstack-panel-protocol-file))
                         (buffer-string))))
        (should (string-match-p "never run such a panel on your own subagents"
                                (cadr first)))
        (should (+agent-shell-pstack-panel--block
                 (concat (cadr first) "\n```pstack-panel\n{}\n```")))
        (let ((example (with-temp-buffer
                         (insert (cadr first))
                         (goto-char (point-min))
                         (re-search-forward "^```pstack-panel\n\\(.*\\)\n```")
                         (match-string 1))))
          (should (+agent-shell-pstack-panel--parse example)))
        (+agent-shell-pstack-panel-mode -1)
        (setq +agent-shell-pstack--primed nil)
        (should (= 1 (length (funcall texts (+agent-shell-pstack--decorate
                                             lead (funcall request "go"))))))
        (should (equal (list "again" +agent-shell-pstack-reminder)
                       (funcall texts (+agent-shell-pstack--decorate
                                       lead (funcall request "again")))))
        (should-not (gethash (cons 'pstack-lead lead) handlers))))))

(ert-deftest +agent-shell-pstack-panel-test-view ()
  "List panels with member states, refresh on change, and visit targets."
  (+agent-shell-pstack-panel-test--with
    (save-window-excursion
      (+agent-shell-pstack-panels)
      (let ((view (current-buffer)))
        (should (equal +agent-shell-pstack-panels--buffer (buffer-name view)))
        (should buffer-read-only)
        (should (string-prefix-p "No panels yet." (buffer-string)))
        (lead-event :kind 'turn-complete :session "s2"
                    :text (+agent-shell-pstack-panel-test--message "{}"))
        (lead-event :kind 'turn-complete :session "s1"
                    :text (+agent-shell-pstack-panel-test--message
                           +agent-shell-pstack-panel-test--how))
        (let* ((id (+agent-shell-pstack-panel-test--id
                    +agent-shell-pstack-panel-test--how))
               (codex (nth 1 (nth 0 started)))
               (claude (nth 1 (nth 1 started)))
               (rejected (+agent-shell-pstack-panel-id
                          (car (+agent-shell-pstack-panel-list)))))
          (should (equal (format "%s  -  rejected
  Lead:  *lead*
  The block must hold a JSON object

%s  how  running
  Lead:  *lead*
  data-flow        codex        starting  %s
  risks            claude-code  starting  %s

" rejected id (buffer-name codex) (buffer-name claude))
                         (with-current-buffer view (buffer-string))))
          (member-event "data-flow" :kind 'init-finished)
          (member-event "data-flow" :kind 'turn-complete :text "Report A")
          (let ((report (expand-file-name (format "%s/data-flow.md" id)
                                          directory)))
            (with-current-buffer view
              (goto-char (point-min))
              (should (search-forward
                       (format "data-flow        codex        done      %s"
                               (abbreviate-file-name report))))
              (should (search-forward "risks            claude-code  starting"))
              (goto-char (point-min))
              (search-forward "data-flow")
              (+agent-shell-pstack-panels-visit)
              (should (equal report buffer-file-name))
              (kill-buffer))
            (with-current-buffer view
              (goto-char (point-min))
              (search-forward "risks")
              (+agent-shell-pstack-panels-visit)
              (should (eq claude (current-buffer))))
            (with-current-buffer view
              (goto-char (point-min))
              (search-forward "Lead:")
              (+agent-shell-pstack-panels-visit)
              (should (eq lead (current-buffer))))))
        (kill-buffer view)))))

(ert-deftest +agent-shell-pstack-panel-test-mode-line ()
  "Show member progress in the lead's mode line while its panel runs."
  (+agent-shell-pstack-panel-test--with
    (let* ((updates nil)
           (id (+agent-shell-pstack-panel-test--id
                +agent-shell-pstack-panel-test--how))
           (item (lambda () (buffer-local-value
                             '+agent-shell-pstack-panel--mode-line lead)))
           (rendered (lambda ()
                       (format-mode-line mode-line-misc-info nil nil lead))))
      (should (member +agent-shell-pstack-panel--mode-line-construct
                      mode-line-misc-info))
      (should-not (funcall item))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional all)
                   (push (list (current-buffer) all) updates))))
        (lead-event :kind 'turn-complete :session "s1"
                    :text (+agent-shell-pstack-panel-test--message
                           +agent-shell-pstack-panel-test--how))
        (should (equal "panel 0/2 " (substring-no-properties (funcall item))))
        (should (equal (format "pstack panel %s (how)
  data-flow  codex  starting
  risks  claude-code  starting
mouse-1: show panels" id)
                       (get-text-property 0 'help-echo (funcall item))))
        (should (eq #'+agent-shell-pstack-panels
                    (lookup-key (get-text-property 0 'local-map (funcall item))
                                [mode-line mouse-1])))
        ;; `format-mode-line' renders nothing in batch Emacs.
        (unless noninteractive
          (let* ((line (funcall rendered))
                 (start (string-match "panel 0/2 " line)))
            (should start)
            (should (get-text-property start 'help-echo line))))
        (should (equal (list (list lead nil)) updates))
        (setq updates nil)
        (member-event "data-flow" :kind 'prompt-ready)
        (should-not updates)
        (member-event "data-flow" :kind 'init-finished)
        (should (string-match-p "data-flow  codex  running"
                                (get-text-property 0 'help-echo
                                                   (funcall item))))
        (member-event "data-flow" :kind 'turn-complete :text "Report A")
        (should (equal "panel 1/2 " (substring-no-properties (funcall item))))
        (should (equal (list (list lead nil) (list lead nil)) updates))
        (member-event "risks" :kind 'init-finished)
        (member-event "risks" :kind 'turn-complete :text "Report B"))
      (should-not (funcall item))
      (unless noninteractive
        (should-not (string-match-p "panel" (funcall rendered))))
      (+agent-shell-pstack-panel-mode -1)
      (should-not (member +agent-shell-pstack-panel--mode-line-construct
                          mode-line-misc-info)))))

(provide 'agent-shell-pstack-panel-test)
;;; agent-shell-pstack-panel-test.el ends here
