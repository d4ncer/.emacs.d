;;; agent-shell-pstack-panel-test.el --- Tests for pstack panels -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests against a stubbed `+agent-shell-drive' surface.  Events reach
;; the panel through the watchers it registers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+agent-shell-pstack-panel nil t)
(require 'agent-shell-sandbox-test)

(defmacro +agent-shell-pstack-panel-test--with (&rest body)
  "Run BODY with a pstack lead, stubbed drive functions and fresh panels.
BODY sees `lead', `directory' (the reports root), `cwd' (the lead's),
`sent' (newest first, each (SHELL . TEXT)), `started' (each (AGENT SHELL
POLICY CWD)), `interrupted' (shells, newest first), `send-ok' (whether
sends succeed), `start-error' (a start failure message), `partials' (alist
of shell to partial text), and the functions `lead-event' and
`member-event'.

Arena git work is stubbed too: `bases' lists the directories snapshotted,
`base-error' and `provision-error' make those steps fail, `shared' lists
the dependency paths each candidate links, `stopped' lists the shells
stopped (newest first), `swept' the sandboxes swept without a shell, and
`cleaned' the manifests cleaned.  Each worktree gets a directory under
`arenas', the arena root.  `captured' lists the roles captured,
`capture-errors' maps a role to its capture failure, and `unchanged'
lists the roles whose result equals the base."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "pstack-panels-" t))
          (cwd (file-name-as-directory
                (file-truename (make-temp-file "pstack-lead-" t))))
          (arenas (file-name-as-directory
                   (file-truename (make-temp-file "pstack-arenas-" t))))
          (+agent-shell-pstack-panel-directory directory)
          (+agent-shell-pstack-arena-directory arenas)
          (+agent-shell-pstack-panel--panels (make-hash-table :test #'equal))
          (+agent-shell-pstack-prime-texts nil)
          (+agent-shell-pstack-reminder-texts nil)
          (+agent-shell-pstack-mode-hook nil)
          (+agent-shell-pstack-panel-mode nil)
          (handlers (make-hash-table :test #'equal))
          (sent nil) (started nil) (interrupted nil) (send-ok t)
          (partials nil)
          (start-error nil)
          (bases nil) (base-error nil) (provision-error nil) (shared nil)
          (stopped nil) (swept nil) (cleaned nil)
          (captured nil) (capture-errors nil) (unchanged nil)
          (diff-extras nil)
          (+agent-shell-drive-read-only-modes
           '((claude-code . "plan") (codex . "read-only")))
          (+agent-shell-sandbox-agents
           '((claude-code :mode "bypassPermissions")
             (codex :mode "agent-full-access")))
          (lead (generate-new-buffer " *lead*")))
     (cl-letf (((symbol-function '+agent-shell-drive--configs)
                (lambda () '(((:identifier . codex))
                             ((:identifier . claude-code))
                             ((:identifier . gemini)))))
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
                     shell))))
               ((symbol-function '+agent-shell-drive-stop)
                (lambda (shell &optional _reason callback)
                  (push shell stopped)
                  (run-at-time 0 nil (lambda ()
                                       (when (buffer-live-p shell)
                                         (kill-buffer shell))
                                       (funcall callback nil)))))
               ((symbol-function '+agent-shell-sandbox-sweep)
                (lambda (sandbox callback &optional _rounds)
                  (push sandbox swept)
                  (run-at-time 0 nil callback nil)))
               ((symbol-function '+agent-shell-arena-git-capture-base)
                (lambda (directory callback)
                  (push directory bases)
                  (run-at-time
                   0 nil callback
                   (unless base-error
                     (+agent-shell-arena-git--base
                      :git-dir (expand-file-name ".git" cwd) :toplevel cwd
                      :prefix "" :head "head" :commit "base"))
                   base-error)))
               ((symbol-function '+agent-shell-arena-git-provision)
                (lambda (manifest name callback)
                  (let ((worktree
                         (file-name-as-directory
                          (expand-file-name
                           name (+agent-shell-arena-git-manifest-directory
                                 manifest)))))
                    (make-directory worktree t)
                    (setf (+agent-shell-arena-git-manifest-roles manifest)
                          (append (+agent-shell-arena-git-manifest-roles
                                   manifest)
                                  (list (+agent-shell-arena-git--role
                                         :name name :worktree worktree
                                         :shared shared))))
                    (run-at-time 0 nil callback
                                 (unless provision-error worktree)
                                 provision-error))))
               ((symbol-function '+agent-shell-arena-git-common-dir)
                (lambda (_manifest) (expand-file-name ".git" cwd)))
               ((symbol-function '+agent-shell-arena-git-clean)
                (lambda (manifest callback)
                  (push manifest cleaned)
                  (run-at-time 0 nil callback nil nil)))
               ((symbol-function '+agent-shell-arena-git-capture)
                (lambda (manifest name callback)
                  (push name captured)
                  (let ((role (+agent-shell-arena-git-role manifest name))
                        (error (alist-get name capture-errors nil nil #'equal)))
                    (unless error
                      (setf (+agent-shell-arena-git-role-ref role)
                            (concat "refs/pstack-arena/x/" name)
                            (+agent-shell-arena-git-role-commit role)
                            (concat "commit-" name)))
                    (run-at-time 0 nil callback
                                 (unless error (concat "commit-" name))
                                 error))))
               ((symbol-function '+agent-shell-arena-git-diff)
                (lambda (_manifest name callback)
                  (run-at-time 0 nil callback
                               (if (member name unchanged)
                                   '(:changed nil :stat "")
                                 (append
                                  (list :changed t
                                        :stat (format " %s.txt | 1 +" name))
                                  (alist-get name diff-extras nil nil
                                             #'equal)))
                               nil)))
               ((symbol-function '+agent-shell-arena-git-checkout)
                (lambda (manifest name callback)
                  (let ((checkout (expand-file-name
                                   (concat "checkouts/" name "/")
                                   (+agent-shell-arena-git-manifest-directory
                                    manifest))))
                    (setf (+agent-shell-arena-git-role-checkout
                           (+agent-shell-arena-git-role manifest name))
                          checkout)
                    (run-at-time 0 nil callback checkout nil)))))
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
           (dolist (panel (+agent-shell-pstack-panel-list))
             (dolist (member (+agent-shell-pstack-panel-members panel))
               (when-let* ((sandbox (+agent-shell-pstack-panel-member-sandbox
                                     member)))
                 (dolist (folder (list (+agent-shell-sandbox-tmp sandbox)
                                       (+agent-shell-sandbox-config sandbox)))
                   (+agent-shell-sandbox-test-unlock folder)
                   (delete-directory folder t)))))
           (dolist (entry started)
             (when (buffer-live-p (cadr entry)) (kill-buffer (cadr entry))))
           (kill-buffer lead)
           (delete-directory directory t)
           (delete-directory arenas t)
           (delete-directory cwd t))))))

(defun +agent-shell-pstack-panel-test--drain ()
  "Run pending timers and process output for a moment."
  (dotimes (_ 5) (accept-process-output nil 0.01)))

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
             (,(format "{\"version\":1,\"shape\":\"bakeoff\",\"brief\":\"b\",\
\"members\":[%s]}" member) "shapes: how, interrogate, arena")
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

(ert-deftest +agent-shell-pstack-panel-test-cancel-from-view ()
  "Cancelling a running panel from the view settles its members once."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       +agent-shell-pstack-panel-test--how))
    (let ((panel (car (+agent-shell-pstack-panel-list))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (with-temp-buffer
          (+agent-shell-pstack-panels-mode)
          (+agent-shell-pstack-panels--render)
          (goto-char (point-min))
          (+agent-shell-pstack-panels-cancel)
          (should (equal '(error error)
                         (+agent-shell-pstack-panel-test--states panel)))
          (should (string-match-p "## data-flow (codex): error\nYou \
cancelled the panel." (cdar sent)))
          (+agent-shell-pstack-panels--render)
          (goto-char (point-min))
          (should-error (+agent-shell-pstack-panels-cancel)
                        :type 'user-error))))))

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

(defconst +agent-shell-pstack-panel-test--one
  "{\"version\":1,\"shape\":\"how\",\"brief\":\"b\",\
\"members\":[{\"role\":\"a\",\"agent\":\"codex\"}]}"
  "A valid one-member how request.")

(ert-deftest +agent-shell-pstack-panel-test-stop-reasons ()
  "Settle a turn that ended without a report as an error with its reason."
  (pcase-dolist (`(,reason ,state)
                 '(("end_turn" done) (nil done) ("cancelled" error)
                   ("refusal" error) ("max_tokens" error)
                   ("max_turn_requests" error)))
    (+agent-shell-pstack-panel-test--with
      (lead-event :kind 'turn-complete :session "s1"
                  :text (+agent-shell-pstack-panel-test--message
                         +agent-shell-pstack-panel-test--one))
      (member-event "a" :kind 'init-finished)
      (member-event "a" :kind 'turn-complete :text "Half a report"
                    :stop-reason reason)
      (let ((followup (car (+agent-shell-pstack-panel-test--followups
                            sent lead))))
        (should (string-match-p (format "## a (codex): %s\n" state) followup))
        (if (eq state 'done)
            (should-not (string-match-p "stop reason" followup))
          (should (string-match-p
                   (format "The turn ended with stop reason %s\\.\n\
Partial text:\n\nHalf a report" reason)
                   followup)))))))

(ert-deftest +agent-shell-pstack-panel-test-error-event ()
  "Settle a member on an error event only while its brief is running."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       +agent-shell-pstack-panel-test--how))
    (member-event "data-flow" :kind 'error :message "Bootstrapping failed")
    (member-event "data-flow" :kind 'init-finished)
    (member-event "risks" :kind 'init-finished)
    (member-event "risks" :kind 'turn-complete :text "B")
    (let ((member (+agent-shell-pstack-panel--member
                   (car (+agent-shell-pstack-panel-list)) "data-flow")))
      (should (eq 'running (+agent-shell-pstack-panel-member-state member)))
      (should-not (+agent-shell-pstack-panel-test--followups sent lead))
      (push (cons (+agent-shell-pstack-panel-member-shell member) "So far")
            partials)
      (member-event "data-flow" :kind 'error :message "Prompt failed")
      (should (eq 'error (+agent-shell-pstack-panel-member-state member)))
      (member-event "data-flow" :kind 'error :message "Second failure")
      (let ((followups (+agent-shell-pstack-panel-test--followups sent lead)))
        (should (= 1 (length followups)))
        (should (string-match-p "## data-flow (codex): error\nReport: .*\n\
The agent reported an error: Prompt failed\nPartial text:\n\nSo far"
                                (car followups)))
        (should-not (string-match-p "Second failure" (car followups)))))))

(ert-deftest +agent-shell-pstack-panel-test-expiry-in-phase ()
  "Expire only the phase that armed the timer."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       +agent-shell-pstack-panel-test--one))
    (let* ((panel (car (+agent-shell-pstack-panel-list)))
           (id (+agent-shell-pstack-panel-id panel)))
      (+agent-shell-pstack-panel--expire id 'judging)
      (should (eq 'starting (+agent-shell-pstack-panel-member-state
                             (car (+agent-shell-pstack-panel-members panel)))))
      (+agent-shell-pstack-panel--expire id 'running)
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should (eq 'timeout (+agent-shell-pstack-panel-member-state
                            (car (+agent-shell-pstack-panel-members panel))))))))

(ert-deftest +agent-shell-pstack-panel-test-schema-guard ()
  "Drop panels from an older struct layout when the file loads again."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       +agent-shell-pstack-panel-test--one))
    (let ((current (car (+agent-shell-pstack-panel-list)))
          (messages nil))
      (puthash "old" (record '+agent-shell-pstack-panel "old" 'how "b" 900)
               +agent-shell-pstack-panel--panels)
      (puthash "older"
               (let ((panel (copy-sequence current)))
                 (setf (+agent-shell-pstack-panel-schema panel) 1)
                 panel)
               +agent-shell-pstack-panel--panels)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format-message format args) messages))))
        (load (locate-library "+agent-shell-pstack-panel.el") nil t t))
      (should (equal (list current) (+agent-shell-pstack-panel-list)))
      (should (string-prefix-p "Dropped 2 pstack panels" (car messages)))
      (setq messages nil)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format-message format args) messages))))
        (load (locate-library "+agent-shell-pstack-panel.el") nil t t))
      (should-not messages)
      (member-event "a" :kind 'init-finished)
      (member-event "a" :kind 'turn-complete :text "Still works")
      (should (string-match-p "Still works" (cdar sent))))))

(defun +agent-shell-pstack-panel-test--arena (&rest fields)
  "Return an arena request with FIELDS, an alist overriding the defaults."
  (json-serialize
   (append fields
           (seq-remove
            (lambda (entry) (assq (car entry) fields))
            `((version . 1) (shape . "arena") (brief . "Add a cache.")
              (rubric . ["RUBRIC-ONE correctness" "RUBRIC-TWO simplicity"
                         "RUBRIC-THREE tests"])
              (timeout . 60)
              (members . [((role . "opus") (agent . "claude-code"))
                          ((role . "codex") (agent . "codex")
                           (focus . "Prefer an LRU"))]))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-request ()
  "Parse an arena into pending candidates and a pending judge."
  (+agent-shell-pstack-panel-test--with
    (let* ((panel (+agent-shell-pstack-panel--parse
                   (+agent-shell-pstack-panel-test--arena) 'codex))
           (members (+agent-shell-pstack-panel-members panel)))
      (should (eq 'preparing (+agent-shell-pstack-panel-phase panel)))
      (should (equal '("RUBRIC-ONE correctness" "RUBRIC-TWO simplicity"
                       "RUBRIC-THREE tests")
                     (+agent-shell-pstack-panel-rubric panel)))
      (should (= 60 (+agent-shell-pstack-panel-timeout panel)))
      (should (= 900 (+agent-shell-pstack-panel-judge-timeout panel)))
      (should (equal '(("opus" claude-code candidate pending nil)
                       ("codex" codex candidate pending "Prefer an LRU")
                       ("judge" claude-code judge pending nil))
                     (mapcar (lambda (member)
                               (list (+agent-shell-pstack-panel-member-role
                                      member)
                                     (+agent-shell-pstack-panel-member-agent
                                      member)
                                     (+agent-shell-pstack-panel-member-kind
                                      member)
                                     (+agent-shell-pstack-panel-member-state
                                      member)
                                     (+agent-shell-pstack-panel-member-focus
                                      member)))
                             members))))
    (let ((judge (lambda (panel)
                   (+agent-shell-pstack-panel-member-agent
                    (car (last (+agent-shell-pstack-panel-members panel)))))))
      (should (eq 'codex (funcall judge (+agent-shell-pstack-panel--parse
                                         (+agent-shell-pstack-panel-test--arena)
                                         'claude-code))))
      (should (eq 'codex (funcall judge (+agent-shell-pstack-panel--parse
                                         (+agent-shell-pstack-panel-test--arena)
                                         nil))))
      (should (eq 'claude-code
                  (funcall judge (+agent-shell-pstack-panel--parse
                                  (+agent-shell-pstack-panel-test--arena)
                                  'codex))))
      (let ((panel (+agent-shell-pstack-panel--parse
                    (+agent-shell-pstack-panel-test--arena
                     '(judge . ((agent . "codex"))) '(judge_timeout . 120))
                    'codex)))
        (should (eq 'codex (funcall judge panel)))
        (should (= 120 (+agent-shell-pstack-panel-judge-timeout panel)))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-validation ()
  "Explain each arena rejection so the lead can correct its request."
  (+agent-shell-pstack-panel-test--with
    (pcase-dolist
        (`(,field ,reason)
         `(((rubric . ["a" "b"]) "Rubric must list 3 to 6 non-empty strings")
           ((rubric . ["a" "b" "c" "d" "e" "f" "g"]) "Rubric must list 3 to 6")
           ((rubric . ["a" "b" 3]) "Rubric must list 3 to 6")
           ((rubric . ["a" "b" " "]) "Rubric must list 3 to 6")
           ((members . [((role . "a") (agent . "gemini"))])
            "Member a has agent \"gemini\"; available agents: codex, \
claude-code")
           ((members . [((role . "judge") (agent . "codex"))])
            "Role judge is reserved")
           ((members . [((role . "base") (agent . "codex"))])
            "Role base is reserved for the ref holding the base")
           ((members . [((role . "-a") (agent . "codex"))])
            "Member role \"-a\" must be lowercase")
           ((judge . ((agent . "gemini"))) "Judge must be an object whose \
agent can run sandboxed; available agents: codex, claude-code")
           ((judge . "codex") "Judge must be an object")
           ((judge_timeout . 0) "Judge timeout must be a whole number of \
seconds from 1 to 3600")
           ((timeout . 4000) "Timeout must be")))
      (let ((sent-before (length sent)))
        (lead-event :kind 'turn-complete :session "s1"
                    :text (+agent-shell-pstack-panel-test--message
                           (+agent-shell-pstack-panel-test--arena field)))
        (should (= (1+ sent-before) (length sent)))
        (should (string-prefix-p
                 (concat "Emacs rejected your pstack-panel request: " reason)
                 (cdar sent)))))
    (let ((system-type 'windows-nt) (+agent-shell-sandbox-backend nil))
      (lead-event :kind 'turn-complete :session "s2"
                  :text (+agent-shell-pstack-panel-test--message
                         (+agent-shell-pstack-panel-test--arena)))
      (should (string-prefix-p "Emacs rejected your pstack-panel request: \
Arena panels need a sandbox to confine the candidates, macOS's sandbox-exec \
or Linux's bwrap; this Emacs runs on windows-nt"
                               (string-replace "’" "'" (cdar sent)))))
    (let ((system-type 'gnu/linux) (+agent-shell-sandbox-backend 'bwrap))
      (should (eq 'arena (+agent-shell-pstack-panel-shape
                          (+agent-shell-pstack-panel--parse
                           (+agent-shell-pstack-panel-test--arena))))))
    (let ((+agent-shell-sandbox-agents nil))
      (lead-event :kind 'turn-complete :session "s3"
                  :text (+agent-shell-pstack-panel-test--message
                         (+agent-shell-pstack-panel-test--arena)))
      (should (string-match-p "available agents: none" (cdar sent))))
    (should-not started)))

(defun +agent-shell-pstack-panel-test--candidates (panel)
  "Return PANEL's candidate members."
  (seq-filter (lambda (member)
                (eq 'candidate (+agent-shell-pstack-panel-member-kind member)))
              (+agent-shell-pstack-panel-members panel)))

(defun +agent-shell-pstack-panel-test--states (panel)
  "Return the state of each of PANEL's members."
  (mapcar #'+agent-shell-pstack-panel-member-state
          (+agent-shell-pstack-panel-members panel)))

(ert-deftest +agent-shell-pstack-panel-test-arena-phase-barrier ()
  "Start candidates in their sandboxes only once preparing ends.
Their timer starts then too, and the judge stays pending."
  (+agent-shell-pstack-panel-test--with
    (setq shared '("elpaca"))
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (let ((panel (car (+agent-shell-pstack-panel-list))))
      (should (eq 'preparing (+agent-shell-pstack-panel-phase panel)))
      (should (equal (list cwd) bases))
      (should-not started)
      (should-not (+agent-shell-pstack-panel-timer panel))
      (should (string-match-p (format "%s  arena  preparing"
                                      (+agent-shell-pstack-panel-id panel))
                              (with-temp-buffer
                                (+agent-shell-pstack-panels--render)
                                (buffer-string))))
      (+agent-shell-pstack-panel-test--drain)
      (should (eq 'candidates (+agent-shell-pstack-panel-phase panel)))
      (should (+agent-shell-pstack-panel-timer panel))
      (should (equal '(starting starting pending)
                     (+agent-shell-pstack-panel-test--states panel)))
      (let* ((manifest (+agent-shell-pstack-panel-arena panel))
             (worktrees (mapcar #'+agent-shell-arena-git-role-worktree
                                (+agent-shell-arena-git-manifest-roles
                                 manifest))))
        (should (string-prefix-p
                 arenas (+agent-shell-arena-git-manifest-directory manifest)))
        (should (equal '(claude-code codex) (mapcar #'car started)))
        (should (equal worktrees (mapcar (lambda (entry)
                                           (file-name-as-directory
                                            (nth 3 entry)))
                                         started)))
        (cl-loop
         for (_agent _shell sandbox _directory) in started
         for worktree in worktrees
         for other in (reverse worktrees)
         do (should (+agent-shell-sandbox-p sandbox))
         (should (equal (directory-file-name worktree)
                        (+agent-shell-sandbox-root sandbox)))
         (let ((denied (+agent-shell-sandbox-deny-reads sandbox)))
           (dolist (path (list cwd directory "~/.claude" "~/.codex" other))
             (should (member (directory-file-name
                              (file-truename (expand-file-name path)))
                             denied)))
           (should-not (member (directory-file-name worktree) denied)))
         (should (equal (list (expand-file-name ".git" cwd)
                              (expand-file-name "elpaca" cwd))
                        (+agent-shell-sandbox-allow-reads sandbox)))
         (should (equal (list (directory-file-name cwd))
                        (+agent-shell-sandbox-allow-metadata sandbox))))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-candidate-brief ()
  "Brief each candidate with the task, its focus and the contract, never
the rubric."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (+agent-shell-pstack-panel-test--drain)
    (member-event "opus" :kind 'init-finished)
    (member-event "codex" :kind 'init-finished)
    (let ((briefs (mapcar (lambda (entry) (cdr (assq (cadr entry) sent)))
                          started)))
      (should (= 2 (length (delq nil briefs))))
      (should (string-prefix-p "You are the opus candidate of a pstack arena \
that Emacs runs for another agent.  Other candidates work on the same task \
elsewhere.\n\nTask:\nAdd a cache.\n\nImplement the task in the current \
directory" (nth 0 briefs)))
      (should (string-match-p "\n\nYour focus:\nPrefer an LRU\n\n"
                              (nth 1 briefs)))
      (dolist (brief briefs)
        (should (string-match-p "Leave your changes in the tree, uncommitted"
                                brief))
        (should (string-match-p "Do not run git commands that write, such as \
git add, commit, stash or checkout" brief))
        (should (string-match-p "the alternatives you considered, and what \
you rejected and why" brief))
        (should-not (string-match-p "RUBRIC" brief))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-mid-turn ()
  "Snapshot the base only after the interrupted lead's turn ends."
  (+agent-shell-pstack-panel-test--with
    (let ((text (concat (+agent-shell-pstack-panel-test--message
                         (+agent-shell-pstack-panel-test--arena))
                        "\nOne more edit.")))
      (lead-event :kind 'tool-call-update :session "s1" :text text
                  :title "Edit file")
      (let ((panel (car (+agent-shell-pstack-panel-list))))
        (should (equal (list lead) interrupted))
        (should (eq 'interrupting (+agent-shell-pstack-panel-phase panel)))
        (+agent-shell-pstack-panel-test--drain)
        (should-not bases)
        (lead-event :kind 'turn-complete :session "s1" :text text
                    :stop-reason "cancelled")
        (should (equal (list cwd) bases))
        (+agent-shell-pstack-panel-test--drain)
        (should (eq 'candidates (+agent-shell-pstack-panel-phase panel)))
        (should (= 2 (length started)))
        (lead-event :kind 'turn-complete :session "s1" :text "Next")
        (should (= 1 (length bases)))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-preparation-fails ()
  "Reject an arena whose base or worktrees cannot be made."
  (+agent-shell-pstack-panel-test--with
    (setq base-error "/x is not inside a git working tree")
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (+agent-shell-pstack-panel-test--drain)
    (should (string-prefix-p "Emacs rejected your pstack-panel request: /x is \
not inside a git working tree\nNo panel started." (cdar sent)))
    (should-not started)
    (setq base-error nil provision-error "git worktree: no space")
    (lead-event :kind 'turn-complete :session "s2"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (+agent-shell-pstack-panel-test--drain)
    (let ((panel (car (last (+agent-shell-pstack-panel-list)))))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should (equal '(error error error)
                     (+agent-shell-pstack-panel-test--states panel)))
      (should (string-prefix-p "Emacs rejected your pstack-panel request: \
git worktree: no space" (cdar sent)))
      (should (= 1 (length cleaned)))
      (should-not (+agent-shell-pstack-panel-arena panel)))
    (should-not started)))

(ert-deftest +agent-shell-pstack-panel-test-arena-abandon-preparing ()
  "End an arena whose lead died while it was being prepared.
An arena already on disk is discarded once the step in flight returns."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (lead-event :kind 'clean-up)
    (let ((panel (car (+agent-shell-pstack-panel-list))))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should (equal '(error error error)
                     (+agent-shell-pstack-panel-test--states panel)))
      (+agent-shell-pstack-panel-test--drain)
      (should-not (+agent-shell-pstack-panel-arena panel))
      (should-not cleaned))
    (let ((held nil))
      (cl-letf (((symbol-function '+agent-shell-arena-git-provision)
                 (lambda (_manifest _name callback) (setq held callback))))
        (lead-event :kind 'turn-complete :session "s2"
                    :text (+agent-shell-pstack-panel-test--message
                           (+agent-shell-pstack-panel-test--arena)))
        (+agent-shell-pstack-panel-test--drain)
        (should held)
        (lead-event :kind 'clean-up)
        (funcall held "/somewhere" nil)
        (+agent-shell-pstack-panel-test--drain)
        (should (= 1 (length cleaned)))
        (should-not (+agent-shell-pstack-panel-arena
                     (car (last (+agent-shell-pstack-panel-list)))))))
    (should-not started)))

(ert-deftest +agent-shell-pstack-panel-test-arena-stops-candidates ()
  "Stop a settled candidate with every process it started.
Its private folders go even when they hold read-only trees, and a
folder that cannot go is reported without stalling the stop."
  (+agent-shell-pstack-panel-test--with
    (lead-event :kind 'turn-complete :session "s1"
                :text (+agent-shell-pstack-panel-test--message
                       (+agent-shell-pstack-panel-test--arena)))
    (+agent-shell-pstack-panel-test--drain)
    (let* ((panel (car (+agent-shell-pstack-panel-list)))
           (opus (car (+agent-shell-pstack-panel-test--candidates panel)))
           (codex (cadr (+agent-shell-pstack-panel-test--candidates panel)))
           (sandbox (+agent-shell-pstack-panel-member-sandbox opus))
           (stuck (+agent-shell-pstack-panel-member-sandbox codex))
           (messages nil))
      (+agent-shell-sandbox-test-read-only-tree
       (+agent-shell-sandbox-tmp sandbox))
      (+agent-shell-sandbox-test-read-only-tree
       (+agent-shell-sandbox-config sandbox))
      (+agent-shell-sandbox-test-immutable
       (expand-file-name "pinned" (+agent-shell-sandbox-tmp stuck)))
      (member-event "opus" :kind 'init-finished)
      (member-event "opus" :kind 'turn-complete :text "Rationale")
      (should-not stopped)
      (+agent-shell-sandbox-test-wait
       (lambda () (eq 'stopped (+agent-shell-pstack-panel-member-capture
                                opus))))
      (should (equal (list (+agent-shell-pstack-panel-member-shell opus))
                     stopped))
      (+agent-shell-sandbox-test-wait
       (lambda ()
         (not (or (file-exists-p (+agent-shell-sandbox-tmp sandbox))
                  (file-exists-p (+agent-shell-sandbox-config sandbox))))))
      (kill-buffer (+agent-shell-pstack-panel-member-shell codex))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (push (apply #'format-message format arguments)
                         messages))))
        (member-event "codex" :kind 'clean-up :reason "Adapter crashed")
        (+agent-shell-sandbox-test-wait
         (lambda () (not (memq (+agent-shell-pstack-panel-member-capture
                                codex)
                               '(nil stopping)))))
        (+agent-shell-sandbox-test-wait
         (lambda () (seq-some (lambda (message)
                                (and (string-search "codex" message)
                                     (string-search "pinned" message)))
                              messages))))
      (should (equal (list stuck) swept))
      (should (= 1 (length stopped)))
      (should-not (file-exists-p (+agent-shell-sandbox-config stuck))))))

(defmacro +agent-shell-pstack-panel-test--with-arena (&rest body)
  "Run BODY with an arena whose candidates opus and codex are running.
BODY sees `panel', the functions of `+agent-shell-pstack-panel-test--with'
and `finish' (ROLE TEXT), which ends ROLE's turn with TEXT."
  (declare (indent 0))
  `(+agent-shell-pstack-panel-test--with
     (lead-event :kind 'turn-complete :session "s1"
                 :text (+agent-shell-pstack-panel-test--message
                        (+agent-shell-pstack-panel-test--arena)))
     (+agent-shell-pstack-panel-test--drain)
     (let ((panel (car (+agent-shell-pstack-panel-list))))
       (member-event "opus" :kind 'init-finished)
       (member-event "codex" :kind 'init-finished)
       (cl-flet ((finish (role text)
                   (member-event role :kind 'turn-complete :text text
                                 :stop-reason "end_turn")))
         ,@body))))

(defun +agent-shell-pstack-panel-test--judge (panel)
  "Return PANEL's judge."
  (car (last (+agent-shell-pstack-panel-members panel))))

(ert-deftest +agent-shell-pstack-panel-test-arena-judging ()
  "Capture every candidate, then judge the results with the rubric.
The candidates' timeout leaves the pending judge alone, and the judge gets
a timer of its own."
  (+agent-shell-pstack-panel-test--with-arena
    (let ((judge (+agent-shell-pstack-panel-test--judge panel))
          (id (+agent-shell-pstack-panel-id panel))
          (manifest (+agent-shell-pstack-panel-arena panel)))
      (finish "opus" "Rationale A\nRejected a global cache.")
      (push (cons (nth 1 (nth 1 started)) "Half way") partials)
      (+agent-shell-pstack-panel--expire id 'candidates)
      (should (eq 'pending (+agent-shell-pstack-panel-member-state judge)))
      (should (eq 'capturing (+agent-shell-pstack-panel-phase panel)))
      (should (= 2 (length started)))
      (+agent-shell-pstack-panel-test--drain)
      (should (equal '("codex" "opus") (sort (copy-sequence captured)
                                             #'string<)))
      (should (equal '("done" "timeout")
                     (mapcar #'+agent-shell-arena-git-role-state
                             (+agent-shell-arena-git-manifest-roles manifest))))
      (should (eq 'judging (+agent-shell-pstack-panel-phase panel)))
      (let* ((entry (nth 2 started))
             (sandbox (nth 2 entry))
             (checkouts (expand-file-name
                         "checkouts"
                         (+agent-shell-arena-git-manifest-directory manifest))))
        (should (eq 'codex (nth 0 entry)))
        (should (eq sandbox (+agent-shell-pstack-panel-member-sandbox judge)))
        (should (+agent-shell-sandbox-read-only sandbox))
        (should (equal (file-truename checkouts)
                       (+agent-shell-sandbox-root sandbox)))
        (should (equal (+agent-shell-sandbox-root sandbox) (nth 3 entry)))
        (should (+agent-shell-sandbox--under-p
                 (file-truename cwd)
                 (+agent-shell-sandbox-deny-reads sandbox))))
      (should (eq (+agent-shell-pstack-panel-member-shell judge)
                  (nth 1 (nth 2 started))))
      (+agent-shell-pstack-panel--expire id 'candidates)
      (should (eq 'starting (+agent-shell-pstack-panel-member-state judge)))
      (should (+agent-shell-pstack-panel-timer panel))
      (member-event "judge" :kind 'init-finished)
      (let ((brief (cdr (assq (+agent-shell-pstack-panel-member-shell judge)
                              sent)))
            (checkouts (expand-file-name
                        "checkouts/"
                        (+agent-shell-arena-git-manifest-directory manifest))))
        (should (string-prefix-p
                 (format "You are the judge of a pstack arena that \
Emacs runs for another agent.  You run in a read-only sandbox: you can \
read the checkouts below and the repository's git objects, and write only \
your temp folder.  Each candidate below worked on the same task from the \
same base commit, alone in a sandbox.  A checkout holds the files its \
candidate changed, or every file when it changed more than 1000.  Read any \
other file at the base with git -C CHECKOUT show base:PATH.

Task:
Add a cache.

Rubric:
1. RUBRIC-ONE correctness
2. RUBRIC-TWO simplicity
3. RUBRIC-THREE tests

## Candidate A
It finished its turn.
Checkout: %sopus/
Diff against the base:
 opus.txt | 1 +
Rationale:
Rationale A
Rejected a global cache.

## Candidate B
It timed out; its work was captured as it stood.
Checkout: %scodex/
Diff against the base:
 codex.txt | 1 +
Rationale:
Half way

Read every candidate's checkout" checkouts checkouts)
                 brief))
        (should (string-match-p "Score each candidate on each rubric \
criterion from 1 to 5" brief))
        (should (string-match-p "recommend one candidate as the base" brief)))
      (should-not (+agent-shell-pstack-panel-test--followups sent lead))
      (member-event "judge" :kind 'turn-complete :text "A wins.")
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should-not (+agent-shell-pstack-panel-timer panel))
      (should (= 1 (length (+agent-shell-pstack-panel-test--followups
                            sent lead))))
      (+agent-shell-pstack-panel-test--drain)
      (should-not (buffer-live-p (+agent-shell-pstack-panel-member-shell
                                  judge))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-judge-expires ()
  "Time the judge out on its own timeout."
  (+agent-shell-pstack-panel-test--with-arena
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (let ((judge (+agent-shell-pstack-panel-test--judge panel)))
      (should (eq 'judging (+agent-shell-pstack-panel-phase panel)))
      (member-event "judge" :kind 'init-finished)
      (+agent-shell-pstack-panel--expire (+agent-shell-pstack-panel-id panel)
                                         'judging)
      (should (eq 'timeout (+agent-shell-pstack-panel-member-state judge)))
      (should (equal "No report within the 900-second timeout."
                     (+agent-shell-pstack-panel-member-error judge)))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-captures-after-all-stop ()
  "Capture nothing while a candidate runs, so none can read another's result."
  (+agent-shell-pstack-panel-test--with-arena
    (let ((candidates (+agent-shell-pstack-panel-test--candidates panel)))
      (finish "opus" "A")
      (+agent-shell-pstack-panel-test--drain)
      (should (equal '(stopped nil)
                     (mapcar #'+agent-shell-pstack-panel-member-capture
                             candidates)))
      (should (= 1 (length stopped)))
      (should-not captured)
      (should (eq 'candidates (+agent-shell-pstack-panel-phase panel)))
      (finish "codex" "B")
      (+agent-shell-pstack-panel-test--drain)
      (should (equal '("codex" "opus") (sort (copy-sequence captured)
                                             #'string<)))
      (should (equal '(captured captured)
                     (mapcar #'+agent-shell-pstack-panel-member-capture
                             candidates)))
      (should (eq 'judging (+agent-shell-pstack-panel-phase panel))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-captures-once-stopped ()
  "Wait for the last settled candidate to stop before capturing any."
  (+agent-shell-pstack-panel-test--with-arena
    (let* ((candidates (+agent-shell-pstack-panel-test--candidates panel))
           (codex (+agent-shell-pstack-panel-member-shell (cadr candidates)))
           (held nil)
           (stop (symbol-function '+agent-shell-drive-stop)))
      (cl-letf (((symbol-function '+agent-shell-drive-stop)
                 (lambda (shell &optional reason callback)
                   (if (eq shell codex)
                       (setq held (lambda () (funcall callback nil)))
                     (funcall stop shell reason callback)))))
        (finish "opus" "A")
        (finish "codex" "B")
        (+agent-shell-pstack-panel-test--drain)
        (should held)
        (should (equal '(stopped stopping)
                       (mapcar #'+agent-shell-pstack-panel-member-capture
                               candidates)))
        (should (eq 'capturing (+agent-shell-pstack-panel-phase panel)))
        (should-not captured)
        (funcall held)
        (+agent-shell-pstack-panel-test--drain)
        (should (= 2 (length captured)))
        (should (eq 'judging (+agent-shell-pstack-panel-phase panel)))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-capture-failure ()
  "Capture and judge the other candidates when one capture fails."
  (+agent-shell-pstack-panel-test--with-arena
    (setq capture-errors '(("codex" . "git add: boom")))
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (let ((judge (+agent-shell-pstack-panel-test--judge panel)))
      (should (equal '(captured failed)
                     (mapcar #'+agent-shell-pstack-panel-member-capture
                             (+agent-shell-pstack-panel-test--candidates
                              panel))))
      (should (eq 'judging (+agent-shell-pstack-panel-phase panel)))
      (should (eq (+agent-shell-pstack-panel-member-shell judge)
                  (nth 1 (nth 2 started)))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-lead-killed-candidates ()
  "Capture what the candidates made when the lead dies, but never judge."
  (+agent-shell-pstack-panel-test--with-arena
    (let ((judge (+agent-shell-pstack-panel-test--judge panel)))
      (finish "opus" "A")
      (+agent-shell-pstack-panel-test--drain)
      (should-not captured)
      (lead-event :kind 'clean-up)
      (should (equal '(done error error)
                     (+agent-shell-pstack-panel-test--states panel)))
      (should (equal "The lead's shell ended."
                     (+agent-shell-pstack-panel-member-error judge)))
      (+agent-shell-pstack-panel-test--drain)
      (should (= 2 (length captured)))
      (should (equal '(captured captured)
                     (mapcar #'+agent-shell-pstack-panel-member-capture
                             (+agent-shell-pstack-panel-test--candidates
                              panel))))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should (= 2 (length started)))
      (should-not (+agent-shell-pstack-panel-member-shell judge)))))

(ert-deftest +agent-shell-pstack-panel-test-arena-lead-killed-judging ()
  "Stop the judge when the lead dies while it runs."
  (+agent-shell-pstack-panel-test--with-arena
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (let ((judge (+agent-shell-pstack-panel-test--judge panel)))
      (should (eq 'judging (+agent-shell-pstack-panel-phase panel)))
      (lead-event :kind 'clean-up)
      (should (eq 'error (+agent-shell-pstack-panel-member-state judge)))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (+agent-shell-pstack-panel-test--drain)
      (should-not (buffer-live-p (+agent-shell-pstack-panel-member-shell
                                  judge))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-judge-start-fails ()
  "Finish once when the judge cannot start."
  (+agent-shell-pstack-panel-test--with-arena
    (setq start-error "No codex here")
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (let ((judge (+agent-shell-pstack-panel-test--judge panel)))
      (should (eq 'error (+agent-shell-pstack-panel-member-state judge)))
      (should (equal "No codex here"
                     (+agent-shell-pstack-panel-member-error judge)))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should-not (+agent-shell-pstack-panel-timer panel))
      (lead-event :kind 'prompt-ready)
      (+agent-shell-pstack-panel-test--drain)
      (should (= 1 (length (+agent-shell-pstack-panel-test--followups
                            sent lead)))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-nothing-usable ()
  "Finish without a judge when no candidate changed the base."
  (+agent-shell-pstack-panel-test--with-arena
    (setq unchanged '("opus") capture-errors '(("codex" . "git add: boom")))
    (finish "opus" "Nothing to do.")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (let ((judge (+agent-shell-pstack-panel-test--judge panel))
          (candidates (+agent-shell-pstack-panel-test--candidates panel)))
      (should (equal '(captured failed)
                     (mapcar #'+agent-shell-pstack-panel-member-capture
                             candidates)))
      (should (equal '(:commit "commit-opus" :ref "refs/pstack-arena/x/opus"
                               :changed nil :stat "")
                     (+agent-shell-pstack-panel-member-result
                      (car candidates))))
      (should (equal '(:error "git add: boom")
                     (+agent-shell-pstack-panel-member-result
                      (cadr candidates))))
      (should (eq 'finished (+agent-shell-pstack-panel-phase panel)))
      (should (= 2 (length started)))
      (should (eq 'error (+agent-shell-pstack-panel-member-state judge)))
      (should (string-match-p "No candidate changed the base"
                              (+agent-shell-pstack-panel-member-error judge))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-judges-usable-only ()
  "Show the judge only the candidates whose results changed the base."
  (+agent-shell-pstack-panel-test--with-arena
    (setq unchanged '("codex"))
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (member-event "judge" :kind 'init-finished)
    (let ((brief (cdar sent)))
      (should (string-match-p "## Candidate A\n" brief))
      (should-not (string-match-p "## Candidate B" brief))
      (should (string-match-p "\nNot judged, because they left no usable \
change: B\\.\n" brief)))))

(ert-deftest +agent-shell-pstack-panel-test-arena-followup ()
  "Hand the lead each candidate's result, a graft command and the judge's
report, then how to pick, graft, verify and clean up."
  (+agent-shell-pstack-panel-test--with-arena
    (setq unchanged '("codex"))
    (let* ((manifest (+agent-shell-pstack-panel-arena panel))
           (arena (+agent-shell-arena-git-manifest-directory manifest))
           (id (+agent-shell-pstack-panel-id panel))
           (report (expand-file-name (concat id "/judge.md") directory)))
      (finish "opus" "Rationale A")
      (push (cons (nth 1 (nth 1 started)) "Half way") partials)
      (+agent-shell-pstack-panel--expire id 'candidates)
      (+agent-shell-pstack-panel-test--drain)
      (member-event "judge" :kind 'init-finished)
      (member-event "judge" :kind 'turn-complete :text "A wins: 5, 4, 5.")
      (let ((followup (car (+agent-shell-pstack-panel-test--followups
                            sent lead))))
        (should (string-prefix-p
                 (format "pstack panel %s (arena) has finished: 1 of 2 \
candidates changed the base.
Base: base, a snapshot of %s when the arena started.  Arena directory: %s

## A: opus (claude-code): done
Result: commit-opus (refs/pstack-arena/x/opus)
Diff against the base:
 opus.txt | 1 +
Apply: (cd %s && git diff --binary base commit-opus | git apply --3way)
Rationale:
Rationale A

## B: codex (codex): timeout
No report within the 60-second timeout.
Result: commit-codex (refs/pstack-arena/x/codex), the same tree as the base.
Rationale:
Half way

## judge (codex): done
Report: %s

A wins: 5, 4, 5.

Pick, graft and verify as the arena skill's lead."
                         id (directory-file-name cwd)
                         (directory-file-name arena)
                         (shell-quote-argument (directory-file-name cwd))
                         report)
                 followup))
        (should (string-match-p "never by checking their files out over \
your tree" followup))
        (should-not (string-match-p "checkout [^ ]+ --" followup))
        (should (string-suffix-p "press x on its line in the pstack panels \
view, or run M-x +agent-shell-pstack-arena-clean." followup))))))

(defun +agent-shell-pstack-panel-test--candidate-section (followup label)
  "Return candidate LABEL's section of FOLLOWUP."
  (string-match (format "^## %s: .*\n\\(\\(?:[^#\n].*\n\\|\n\\)*\\)" label)
                followup)
  (match-string 1 followup))

(ert-deftest +agent-shell-pstack-panel-test-arena-followup-sparse-warning ()
  "Name the files a candidate changed outside the lead's sparse patterns."
  (+agent-shell-pstack-panel-test--with-arena
    (setq diff-extras '(("opus" :outside ("l10n/m.de.xlf" "l10n/m.fr.xlf"))))
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (member-event "judge" :kind 'init-finished)
    (member-event "judge" :kind 'turn-complete :text "A wins.")
    (let ((followup (car (+agent-shell-pstack-panel-test--followups
                          sent lead))))
      (should (string-match-p
               "^Outside your sparse-checkout patterns: l10n/m.de.xlf, \
l10n/m.fr.xlf\\.  Applying the diff writes them into your tree anyway"
               (+agent-shell-pstack-panel-test--candidate-section
                followup "A")))
      (should-not (string-match-p
                   "sparse" (+agent-shell-pstack-panel-test--candidate-section
                             followup "B"))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-followup-lfs-warning ()
  "Name the Git LFS files a candidate changed and how to graft them."
  (+agent-shell-pstack-panel-test--with-arena
    (setq diff-extras '(("codex" :lfs ("assets/logo.png"))))
    (finish "opus" "A")
    (finish "codex" "B")
    (+agent-shell-pstack-panel-test--drain)
    (member-event "judge" :kind 'init-finished)
    (member-event "judge" :kind 'turn-complete :text "B wins.")
    (let ((followup (car (+agent-shell-pstack-panel-test--followups
                          sent lead))))
      (should (string-match-p
               "^Git LFS files: assets/logo.png\\.  The diff changes their \
pointers.*graft such a file by hand\\.$"
               (+agent-shell-pstack-panel-test--candidate-section
                followup "B")))
      (should-not (string-match-p
                   "LFS" (+agent-shell-pstack-panel-test--candidate-section
                          followup "A"))))))

(ert-deftest +agent-shell-pstack-panel-test-arena-view-and-clean ()
  "Show an arena's phase and directory, and clean it with x once finished."
  (+agent-shell-pstack-panel-test--with-arena
    (save-window-excursion
      (+agent-shell-pstack-panels)
      (let* ((view (current-buffer))
             (id (+agent-shell-pstack-panel-id panel))
             (manifest (+agent-shell-pstack-panel-arena panel))
             (arena (abbreviate-file-name
                     (+agent-shell-arena-git-manifest-directory manifest)))
             (answers nil)
             (text (lambda () (with-current-buffer view (buffer-string)))))
        (should (string-match-p (format "^%s  arena  candidates$" id)
                                (funcall text)))
        (should (string-match-p (format "^  Arena: %s$" (regexp-quote arena))
                                (funcall text)))
        (should (string-match-p (format "pstack panel %s (arena, candidates)"
                                        id)
                                (get-text-property
                                 0 'help-echo
                                 (buffer-local-value
                                  '+agent-shell-pstack-panel--mode-line lead))))
        (with-current-buffer view
          (goto-char (point-min))
          (search-forward "Arena:")
          (should-error (+agent-shell-pstack-panels-clean) :type 'user-error))
        (should (string-match-p "^  judge            codex        pending   \
not started$" (funcall text)))
        (finish "opus" "A")
        (finish "codex" "B")
        (+agent-shell-pstack-panel-test--drain)
        (should (string-match-p "^  opus             claude-code  done      \
captured commit-opus$" (funcall text)))
        (should (string-match-p (format "^%s  arena  judging$" id)
                                (funcall text)))
        (member-event "judge" :kind 'init-finished)
        (member-event "judge" :kind 'turn-complete :text "Scores")
        (should (string-match-p
                 (format "^  Arena: %s  (x cleans it up)$" (regexp-quote arena))
                 (funcall text)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt) (push prompt answers) nil)))
          (with-current-buffer view
            (goto-char (point-min))
            (search-forward "judge ")
            (+agent-shell-pstack-panels-clean)))
        (should (= 1 (length answers)))
        (should-not cleaned)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (with-current-buffer view
            (goto-char (point-min))
            (search-forward "Lead:")
            (+agent-shell-pstack-panels-clean)))
        (should (equal (list manifest) cleaned))
        (should-not (+agent-shell-pstack-panel-arena panel))
        (should-not (string-match-p "Arena:" (funcall text)))
        (kill-buffer view)))))

(ert-deftest +agent-shell-pstack-panel-test-arena-clean-command ()
  "Offer every arena on disk except those whose panel still runs."
  (+agent-shell-pstack-panel-test--with-arena
    (let* ((other (+agent-shell-arena-git-create
                   (+agent-shell-arena-git--base
                    :git-dir (expand-file-name ".git" cwd) :toplevel cwd
                    :prefix "" :head "head" :commit "base")
                   "old"))
           (offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq offered (mapcar #'car choices))
                   (car offered)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (+agent-shell-pstack-arena-clean))
      (should (equal (list (format "%s  %s"
                                   (+agent-shell-arena-git-manifest-id other)
                                   (abbreviate-file-name cwd)))
                     offered))
      (should (equal (list (+agent-shell-arena-git-manifest-id other))
                     (mapcar #'+agent-shell-arena-git-manifest-id cleaned)))
      (should (+agent-shell-pstack-panel-arena panel)))))

(ert-deftest +agent-shell-pstack-panel-test-arena-protocol ()
  "Describe arenas only where candidates can run sandboxed."
  (+agent-shell-pstack-panel-test--with
    (let ((protocol (+agent-shell-pstack-panel--protocol)))
      (should (string-match-p "calls for a how, interrogate or arena panel"
                              protocol))
      (should (string-match-p "^Shapes: how (.*); interrogate (.*); arena \
(candidates each implement" protocol))
      (should (string-match-p "Candidate agents: codex, claude-code\\."
                              protocol))
      (should (string-match-p "\"rubric\", 3 to 6 criteria that only the judge \
sees" protocol))
      (should (string-match-p "\"judge_timeout\", 900 seconds" protocol))
      (should (string-match-p "^Request how, interrogate and arena panels"
                              (+agent-shell-pstack-panel--reminder))))
    (dolist (binding '((+agent-shell-sandbox-backend . nil)
                       (+agent-shell-sandbox-agents . nil)))
      (cl-progv (list (car binding)) (list (cdr binding))
        (let ((protocol (+agent-shell-pstack-panel--protocol)))
          (should (string-match-p "calls for a how or interrogate panel"
                                  protocol))
          (should-not (string-match-p "arena\\|rubric" protocol))
          (should (string-match-p "^Request how and interrogate panels"
                                  (+agent-shell-pstack-panel--reminder))))))))

(provide 'agent-shell-pstack-panel-test)
;;; agent-shell-pstack-panel-test.el ends here
