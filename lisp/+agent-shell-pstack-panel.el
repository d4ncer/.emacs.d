;;; +agent-shell-pstack-panel.el --- Cross-vendor pstack panels -*- lexical-binding: t; -*-

;;; Commentary:
;; A lead agent in `+agent-shell-pstack-mode' requests a panel by ending its
;; final message with a fenced pstack-panel JSON block.  A lead that calls a
;; tool after the block is interrupted once the panel starts.  The priming
;; prompt carries the protocol, and later prompts name the file that holds
;; it, `+agent-shell-pstack-panel-protocol-file'.  Emacs starts one
;; read-only shell per member in the lead's directory, sends each the brief,
;; writes each report once under `+agent-shell-pstack-panel-directory', and
;; prompts the lead with every report and the shape's triage line.
;;
;; Panels live in memory.  Only live `turn-complete' events are read, so a
;; restored session never starts a panel, and a request seen before is
;; ignored.  A lead's mode line shows its running panels.  See
;; docs/AGENT-SHELL-PSTACK.md.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-drive)
(require '+agent-shell-pstack)

(declare-function agent-shell-cwd "agent-shell-project")

(defgroup +agent-shell-pstack-panel nil
  "Cross-vendor pstack panels."
  :group '+agent-shell-pstack)

(defcustom +agent-shell-pstack-panel-directory
  (locate-user-emacs-file "var/pstack-panels/")
  "Directory holding one subdirectory of member reports per panel."
  :type 'directory)

(defcustom +agent-shell-pstack-panel-default-timeout 900
  "Seconds a panel waits for its members when the request names none."
  :type 'natnum)

(defconst +agent-shell-pstack-panel-max-timeout 3600
  "Longest timeout, in seconds, a request may ask for.")

(defconst +agent-shell-pstack-panel-max-members 4
  "Most members a panel may have.")

(defcustom +agent-shell-pstack-panel-shapes
  '((how
     :summary "members investigate the brief from their focus and explain \
it, citing file:line"
     :contract "Investigate the brief from your focus and explain what you \
find.  Cite file:line for every claim, and mark anything you inferred rather \
than read.  End your turn with the report as your final message: findings \
first, then open questions.  Nobody will answer questions during the turn."
     :triage "Triage these reports as the how skill's synthesizer: reconcile \
conflicts, check each load-bearing claim against the code, then write the \
explanation.")
    (interrogate
     :summary "members are adversarial reviewers returning findings with \
severity and file:line evidence"
     :contract "You are an adversarial reviewer.  Try to break the design or \
change in the brief from your focus.  Report each finding with a severity \
(critical, high, medium or low), file:line evidence, and the failure it \
causes.  Say plainly when you find nothing.  End your turn with the findings \
as your final message.  Nobody will answer questions during the turn."
     :triage "Triage each finding as the interrogate skill's lead: verify it \
against the code, then fix it, dismiss it with a concrete reason, or ask.  \
Findings that several agents agree on are high signal."))
  "Panel shapes by name.
Each value is a plist: :summary describes the shape to the lead, :contract
ends each member's prompt, and :triage ends the follow-up to the lead."
  :type '(alist :key-type symbol :value-type plist))

(cl-defstruct (+agent-shell-pstack-panel (:constructor nil)
                                         (:constructor
                                          +agent-shell-pstack-panel--make)
                                         (:copier nil))
  "A panel request and its progress."
  id shape brief timeout lead cwd members timer rejection followup delivered
  interrupted (created (float-time)))

(cl-defstruct (+agent-shell-pstack-panel-member
               (:constructor nil)
               (:constructor +agent-shell-pstack-panel--make-member)
               (:copier nil))
  "One panel member.
STATE moves from `starting' (shell initializing) to `waiting' (initialized,
brief unsent) to `running' (brief sent), then settles once as `done',
`error' or `timeout'."
  role agent focus shell (state 'starting) text error report)

(defvar +agent-shell-pstack-panel--panels (make-hash-table :test #'equal)
  "Map panel id to panel, including rejected requests.")

(defconst +agent-shell-pstack-panel--settled '(done error timeout)
  "Member states that end a member.")

(defvar-local +agent-shell-pstack-panel--mode-line nil
  "Mode-line text for this lead's running panels, or nil.")
(put '+agent-shell-pstack-panel--mode-line 'risky-local-variable t)

(defconst +agent-shell-pstack-panel--mode-line-construct
  '("" +agent-shell-pstack-panel--mode-line)
  "Mode-line construct showing `+agent-shell-pstack-panel--mode-line'.")

;;;; Requests

(defun +agent-shell-pstack-panel--block (text)
  "Return the first complete pstack-panel block in TEXT, or nil."
  (when (and text (string-match (rx bol "```pstack-panel" (* blank) "\n")
                                text))
    (let ((start (match-end 0)))
      (when (string-match (rx bol "```" (* blank) eol) text start)
        (substring-no-properties text start
                                 (max start (1- (match-beginning 0))))))))

(defun +agent-shell-pstack-panel--id (session block)
  "Return the panel id for BLOCK requested in SESSION."
  (substring (sha1 (concat session "\0" block)) 0 12))

(defun +agent-shell-pstack-panel--agents ()
  "Return the agent identifiers a panel member may use."
  (+agent-shell-drive-read-only-agents))

(defun +agent-shell-pstack-panel--names (symbols)
  "Return SYMBOLS as a comma-separated string."
  (mapconcat #'symbol-name symbols ", "))

(defun +agent-shell-pstack-panel--parse-member (value agents)
  "Return a member from request VALUE, one of AGENTS."
  (unless (and (consp value) (consp (car value)))
    (user-error "Each member must be an object"))
  (let ((role (map-elt value 'role))
        (agent (map-elt value 'agent))
        (focus (map-elt value 'focus)))
    (unless (and (stringp role) (string-match-p (rx bos (+ (any "a-z0-9-")) eos)
                                                role))
      (user-error "Member role %S must be lowercase letters, digits and \
hyphens" role))
    (unless (and (stringp agent) (memq (intern agent) agents))
      (user-error "Member %s has agent %S; available agents: %s" role agent
                  (+agent-shell-pstack-panel--names agents)))
    (unless (or (null focus) (stringp focus))
      (user-error "Member %s has a focus that is not a string" role))
    (+agent-shell-pstack-panel--make-member
     :role role :agent (intern agent) :focus focus)))

(defun +agent-shell-pstack-panel--parse (block)
  "Return a panel described by request BLOCK, or signal `user-error' why not."
  (let* ((request (condition-case nil
                      (json-parse-string block :object-type 'alist
                                         :array-type 'list
                                         :null-object nil :false-object nil)
                    (json-parse-error
                     (user-error "The block is not valid JSON"))))
         (_ (unless (and (consp request) (consp (car request)))
              (user-error "The block must hold a JSON object")))
         (shapes (mapcar #'car +agent-shell-pstack-panel-shapes))
         (shape (let ((name (map-elt request 'shape)))
                  (and (stringp name) (car (memq (intern name) shapes)))))
         (brief (map-elt request 'brief))
         (timeout (or (map-elt request 'timeout)
                      +agent-shell-pstack-panel-default-timeout))
         (values (map-elt request 'members))
         (agents (+agent-shell-pstack-panel--agents)))
    (unless (equal (map-elt request 'version) 1)
      (user-error "Version must be 1"))
    (unless shape
      (user-error "Shape %S is unknown; shapes: %s" (map-elt request 'shape)
                  (+agent-shell-pstack-panel--names shapes)))
    (unless (and (stringp brief) (not (string-blank-p brief)))
      (user-error "Brief must be a non-empty string"))
    (unless (and (natnump timeout) (<= 1 timeout
                                       +agent-shell-pstack-panel-max-timeout))
      (user-error "Timeout must be a whole number of seconds from 1 to %d"
                  +agent-shell-pstack-panel-max-timeout))
    (unless (and (proper-list-p values)
                 (<= 1 (length values) +agent-shell-pstack-panel-max-members))
      (user-error "Members must list 1 to %d members"
                  +agent-shell-pstack-panel-max-members))
    (let ((members (mapcar (lambda (value)
                             (+agent-shell-pstack-panel--parse-member
                              value agents))
                           values)))
      (when-let* ((roles (mapcar #'+agent-shell-pstack-panel-member-role
                                 members))
                  (duplicate (seq-find (lambda (role)
                                         (member role
                                                 (cdr (member role roles))))
                                       roles)))
        (user-error "Role %s appears more than once" duplicate))
      (+agent-shell-pstack-panel--make
       :shape shape :brief brief :timeout timeout :members members))))

;;;; Prompts

(defun +agent-shell-pstack-panel--protocol ()
  "Return the text telling a lead how to request a panel, or nil."
  (when-let* ((agents (+agent-shell-pstack-panel--agents)))
    (let ((shapes +agent-shell-pstack-panel-shapes))
      (format "Emacs can run pstack panels on other agents for you.  When a \
skill calls for a %s panel, request it from Emacs: end your final message \
with a fenced block like this one, then end your turn.  This replaces the \
subagent fan-out those skills describe, so never run such a panel on your \
own subagents.

```pstack-panel
{\"version\":1,\"shape\":\"%s\",\"brief\":\"Self-contained question with \
file paths\",\"timeout\":%d,\"members\":[{\"role\":\"data-flow\",\"agent\":\
\"%s\",\"focus\":\"What this member looks at\"}]}
```

Shapes: %s.
Agents: %s.  With two or more members, mix agents so the panel spans \
vendors.
Each member starts fresh, read-only, in your working directory, and sees \
only the brief and its own focus.  Use 1 to %d members with unique roles of \
lowercase letters, digits and hyphens.  The timeout is in seconds, at most \
%d.  End your turn right after the block.  Emacs starts the panel as soon as \
the block is complete, and interrupts your turn if you call a tool after it.
Do not wait or poll after the block.  When every member has finished or \
timed out, Emacs sends you a prompt with each report inline, the path it was \
saved to, and how to triage the reports."
              (mapconcat (lambda (shape) (symbol-name (car shape)))
                         shapes " or ")
              (caar shapes) +agent-shell-pstack-panel-default-timeout
              (car agents)
              (mapconcat (lambda (shape)
                           (format "%s (%s)" (car shape)
                                   (plist-get (cdr shape) :summary)))
                         shapes "; ")
              (+agent-shell-pstack-panel--names agents)
              +agent-shell-pstack-panel-max-members
              +agent-shell-pstack-panel-max-timeout))))

(defun +agent-shell-pstack-panel-protocol-file ()
  "Return the file that holds the panel protocol for leads."
  (expand-file-name "PROTOCOL.md" +agent-shell-pstack-panel-directory))

(defun +agent-shell-pstack-panel--reminder ()
  "Write the protocol file and return the line that points a lead to it.
Return nil when no agent can be a member."
  (when-let* ((protocol (+agent-shell-pstack-panel--protocol)))
    (let ((file (+agent-shell-pstack-panel-protocol-file)))
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region protocol nil file nil 'silent))
      (format "Request how and interrogate panels from Emacs, never on your \
own subagents: end your final message with a pstack-panel block, then end \
your turn.  The protocol is in %s.  Reread it if it is no longer in context."
              file))))

(defun +agent-shell-pstack-panel--shape (panel key)
  "Return PANEL's shape property KEY."
  (plist-get (alist-get (+agent-shell-pstack-panel-shape panel)
                        +agent-shell-pstack-panel-shapes)
             key))

(defun +agent-shell-pstack-panel--brief (panel member)
  "Return the prompt that briefs MEMBER of PANEL."
  (concat
   (format "You are the %s member of a pstack %s panel that Emacs runs for \
another agent.  You are read-only: Emacs refuses edits and commands that \
change state.\n\nBrief:\n%s\n"
           (+agent-shell-pstack-panel-member-role member)
           (+agent-shell-pstack-panel-shape panel)
           (+agent-shell-pstack-panel-brief panel))
   (when-let* ((focus (+agent-shell-pstack-panel-member-focus member)))
     (format "\nYour focus:\n%s\n" focus))
   "\n" (+agent-shell-pstack-panel--shape panel :contract)))

(defun +agent-shell-pstack-panel-followup-text (panel)
  "Return the prompt that hands PANEL's results to its lead."
  (concat
   (when-let* ((tool (+agent-shell-pstack-panel-interrupted panel)))
     (format "Emacs interrupted your turn because you called %s after your \
pstack-panel block, so that call may not have finished.  Rerun it if you \
still need it.\n\n" tool))
   (+agent-shell-pstack-panel--outcome-text panel)))

(defun +agent-shell-pstack-panel--outcome-text (panel)
  "Return the part of PANEL's follow-up that reports its outcome."
  (if-let* ((rejection (+agent-shell-pstack-panel-rejection panel)))
      (format "Emacs rejected your pstack-panel request: %s\nNo panel \
started.  To retry, end a message with a corrected pstack-panel block."
              rejection)
    (let ((members (+agent-shell-pstack-panel-members panel)))
      (concat
       (format "pstack panel %s (%s) has finished: %s.\n"
               (+agent-shell-pstack-panel-id panel)
               (+agent-shell-pstack-panel-shape panel)
               (mapconcat
                (lambda (state)
                  (format "%d %s" (cl-count state members
                                            :key #'+agent-shell-pstack-panel-member-state)
                          state))
                +agent-shell-pstack-panel--settled ", "))
       (mapconcat
        (lambda (member)
          (let ((state (+agent-shell-pstack-panel-member-state member))
                (text (+agent-shell-pstack-panel-member-text member))
                (report (+agent-shell-pstack-panel-member-report member))
                (error (+agent-shell-pstack-panel-member-error member)))
            (concat
             (format "\n## %s (%s): %s\n"
                     (+agent-shell-pstack-panel-member-role member)
                     (+agent-shell-pstack-panel-member-agent member) state)
             (when report (format "Report: %s\n" report))
             (when error (format "%s\n" error))
             (when (and text (not (string-blank-p text)))
               (concat (unless (eq state 'done) "Partial text:\n")
                       "\n" (string-trim text) "\n")))))
        members "")
       "\n" (+agent-shell-pstack-panel--shape panel :triage)))))

;;;; Lifecycle

(defun +agent-shell-pstack-panel--find (id)
  "Return the panel with ID, or nil."
  (gethash id +agent-shell-pstack-panel--panels))

(defun +agent-shell-pstack-panel--member (panel role)
  "Return PANEL's member with ROLE."
  (seq-find (lambda (member)
              (equal role (+agent-shell-pstack-panel-member-role member)))
            (+agent-shell-pstack-panel-members panel)))

(defun +agent-shell-pstack-panel--settled-p (member)
  "Return whether MEMBER has settled."
  (memq (+agent-shell-pstack-panel-member-state member)
        +agent-shell-pstack-panel--settled))

(defun +agent-shell-pstack-panel--deliver (panel)
  "Send PANEL's follow-up to its lead unless it was already delivered."
  (when (and (+agent-shell-pstack-panel-followup panel)
             (not (+agent-shell-pstack-panel-delivered panel))
             (+agent-shell-drive-send (+agent-shell-pstack-panel-lead panel)
                                      (+agent-shell-pstack-panel-followup
                                       panel)))
    (setf (+agent-shell-pstack-panel-delivered panel) t)
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--deliver-to (lead)
  "Deliver the oldest pending follow-up for LEAD."
  (seq-find #'+agent-shell-pstack-panel--deliver
            (seq-filter (lambda (panel)
                          (eq lead (+agent-shell-pstack-panel-lead panel)))
                        (+agent-shell-pstack-panel-list))))

(defun +agent-shell-pstack-panel--finish (panel)
  "Compose PANEL's follow-up once every member has settled, then deliver it."
  (when (and (not (+agent-shell-pstack-panel-followup panel))
             (seq-every-p #'+agent-shell-pstack-panel--settled-p
                          (+agent-shell-pstack-panel-members panel)))
    (when-let* ((timer (+agent-shell-pstack-panel-timer panel)))
      (cancel-timer timer))
    (setf (+agent-shell-pstack-panel-timer panel) nil
          (+agent-shell-pstack-panel-followup panel)
          (+agent-shell-pstack-panel-followup-text panel))
    (+agent-shell-pstack-panel--deliver-to
     (+agent-shell-pstack-panel-lead panel))))

(defun +agent-shell-pstack-panel--write (panel member)
  "Write MEMBER's text as its report under PANEL's directory."
  (let ((text (+agent-shell-pstack-panel-member-text member)))
    (unless (or (null text) (string-blank-p text))
      (let ((file (expand-file-name
                   (concat (+agent-shell-pstack-panel-member-role member) ".md")
                   (expand-file-name (+agent-shell-pstack-panel-id panel)
                                     +agent-shell-pstack-panel-directory))))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil file nil 'silent))
        (setf (+agent-shell-pstack-panel-member-report member) file)))))

(defun +agent-shell-pstack-panel--settle (panel member state text
                                                &optional error)
  "Settle MEMBER of PANEL as STATE with TEXT and ERROR, once."
  (unless (+agent-shell-pstack-panel--settled-p member)
    (setf (+agent-shell-pstack-panel-member-state member) state
          (+agent-shell-pstack-panel-member-text member) text
          (+agent-shell-pstack-panel-member-error member) error)
    (+agent-shell-pstack-panel--write panel member)
    (when-let* ((shell (+agent-shell-pstack-panel-member-shell member)))
      (run-at-time 0 nil #'+agent-shell-drive-kill shell))
    (+agent-shell-pstack-panel--finish panel)))

(defun +agent-shell-pstack-panel--send-brief (panel member)
  "Send MEMBER of PANEL its brief once it is waiting for one."
  (when (and (eq (+agent-shell-pstack-panel-member-state member) 'waiting)
             (+agent-shell-drive-send
              (+agent-shell-pstack-panel-member-shell member)
              (+agent-shell-pstack-panel--brief panel member)))
    (setf (+agent-shell-pstack-panel-member-state member) 'running)))

(defun +agent-shell-pstack-panel--report-text (event)
  "Return the report in a member's turn-complete EVENT.
Prefer a rejected plan when it is longer than the final text."
  (let ((text (or (plist-get event :text) ""))
        (plan (plist-get event :plan)))
    (if (and (stringp plan) (> (length (string-trim plan))
                               (length (string-trim text))))
        plan
      text)))

(defun +agent-shell-pstack-panel--member-event (id role event)
  "Advance member ROLE of panel ID through EVENT."
  (when-let* ((panel (+agent-shell-pstack-panel--find id))
              (member (+agent-shell-pstack-panel--member panel role)))
    (pcase (plist-get event :kind)
      ('init-finished
       (when (eq (+agent-shell-pstack-panel-member-state member) 'starting)
         (setf (+agent-shell-pstack-panel-member-state member) 'waiting))
       (+agent-shell-pstack-panel--send-brief panel member))
      ('prompt-ready (+agent-shell-pstack-panel--send-brief panel member))
      ('turn-complete
       (when (eq (+agent-shell-pstack-panel-member-state member) 'running)
         (+agent-shell-pstack-panel--settle
          panel member 'done (+agent-shell-pstack-panel--report-text event))))
      ('clean-up
       (+agent-shell-pstack-panel--settle
        panel member 'error nil
        (or (plist-get event :reason)
            "The member's shell ended before it reported."))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--expire (id)
  "Settle every unfinished member of panel ID as timed out."
  (when-let* ((panel (+agent-shell-pstack-panel--find id)))
    (setf (+agent-shell-pstack-panel-timer panel) nil)
    (dolist (member (+agent-shell-pstack-panel-members panel))
      (+agent-shell-pstack-panel--settle
       panel member 'timeout
       (when-let* ((shell (+agent-shell-pstack-panel-member-shell member)))
         (+agent-shell-drive-partial-text shell))
       (format "No report within the %d-second timeout."
               (+agent-shell-pstack-panel-timeout panel))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--start-member (panel member)
  "Start MEMBER of PANEL in a read-only shell."
  (let ((id (+agent-shell-pstack-panel-id panel))
        (role (+agent-shell-pstack-panel-member-role member)))
    (condition-case err
        (let ((shell (+agent-shell-drive-start
                      :config (+agent-shell-pstack-panel-member-agent member)
                      :directory (+agent-shell-pstack-panel-cwd panel)
                      :read-only t)))
          (setf (+agent-shell-pstack-panel-member-shell member) shell)
          (+agent-shell-drive-watch
           shell 'pstack-panel (cons id role)
           (lambda (event)
             (+agent-shell-pstack-panel--member-event id role event))))
      (error (+agent-shell-pstack-panel--settle
              panel member 'error nil (error-message-string err))))))

(defun +agent-shell-pstack-panel--request (lead session text &optional tool)
  "Start the panel that LEAD requested in TEXT in SESSION.
TOOL names the call LEAD made after TEXT; a new request interrupts LEAD.
Return the panel, or nil when TEXT requests none or one already seen."
  (when-let* ((block (+agent-shell-pstack-panel--block text))
              (id (+agent-shell-pstack-panel--id (or session "") block))
              ((not (+agent-shell-pstack-panel--find id))))
    (let ((panel (condition-case err (+agent-shell-pstack-panel--parse block)
                   (user-error
                    (+agent-shell-pstack-panel--make
                     :rejection (error-message-string err))))))
      (setf (+agent-shell-pstack-panel-id panel) id
            (+agent-shell-pstack-panel-lead panel) lead)
      (puthash id panel +agent-shell-pstack-panel--panels)
      (when tool
        (setf (+agent-shell-pstack-panel-interrupted panel) tool)
        (+agent-shell-drive-interrupt lead))
      (unless (+agent-shell-pstack-panel-rejection panel)
        (setf (+agent-shell-pstack-panel-cwd panel)
              (with-current-buffer lead (agent-shell-cwd))
              (+agent-shell-pstack-panel-timer panel)
              (run-at-time (+agent-shell-pstack-panel-timeout panel) nil
                           #'+agent-shell-pstack-panel--expire id))
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (+agent-shell-pstack-panel--start-member panel member)))
      (+agent-shell-pstack-panel--finish panel)
      (+agent-shell-pstack-panels--refresh)
      panel)))

(defun +agent-shell-pstack-panel--abandon (lead)
  "Stop the unfinished panels of LEAD, whose shell has ended."
  (dolist (panel (+agent-shell-pstack-panel-list))
    (when (eq lead (+agent-shell-pstack-panel-lead panel))
      (dolist (member (+agent-shell-pstack-panel-members panel))
        (+agent-shell-pstack-panel--settle panel member 'error nil
                                           "The lead's shell ended."))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--lead-event (event)
  "Read panel requests from a lead's EVENT and deliver its follow-ups."
  (let ((lead (plist-get event :shell)))
    (pcase (plist-get event :kind)
      ('tool-call-update
       (+agent-shell-pstack-panel--request
        lead (plist-get event :session) (plist-get event :text)
        (or (plist-get event :title) "a tool")))
      ('turn-complete
       (+agent-shell-pstack-panel--request lead (plist-get event :session)
                                           (plist-get event :text))
       (+agent-shell-pstack-panel--deliver-to lead))
      ('prompt-ready (+agent-shell-pstack-panel--deliver-to lead))
      ('clean-up (+agent-shell-pstack-panel--abandon lead)))))

(defun +agent-shell-pstack-panel-list ()
  "Return every panel, oldest first."
  (sort (hash-table-values +agent-shell-pstack-panel--panels)
        :key #'+agent-shell-pstack-panel-created))

;;;; View

(defconst +agent-shell-pstack-panels--buffer "*pstack panels*"
  "Name of the panels buffer.")

(defvar-keymap +agent-shell-pstack-panels-mode-map
  :doc "Keymap for `+agent-shell-pstack-panels-mode'."
  "RET" #'+agent-shell-pstack-panels-visit)

(define-derived-mode +agent-shell-pstack-panels-mode special-mode
  "pstack panels"
  "List pstack panels, their members and reports.
\\<+agent-shell-pstack-panels-mode-map>\\[+agent-shell-pstack-panels-visit] visits the report or shell on the current line."
  (setq-local revert-buffer-function
              (lambda (&rest _) (+agent-shell-pstack-panels--render))
              truncate-lines t))

(defun +agent-shell-pstack-panels--status (panel)
  "Return a word describing PANEL's progress."
  (cond ((+agent-shell-pstack-panel-rejection panel) "rejected")
        ((+agent-shell-pstack-panel-delivered panel) "delivered")
        ((+agent-shell-pstack-panel-followup panel) "waiting for the lead")
        (t "running")))

(defun +agent-shell-pstack-panels--line (target &rest parts)
  "Insert PARTS as one line that visits TARGET."
  (insert (propertize (concat (apply #'concat parts) "\n")
                      '+agent-shell-pstack-panels-target target)))

(defun +agent-shell-pstack-panels--name (buffer)
  "Return BUFFER's name, or a note that it has ended."
  (if (buffer-live-p buffer) (buffer-name buffer) "ended"))

(defun +agent-shell-pstack-panels--render ()
  "Redraw the current panels buffer, keeping point's line."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (panels (+agent-shell-pstack-panel-list)))
    (erase-buffer)
    (unless panels
      (insert "No panels yet.  A pstack lead requests one by ending its \
message with a pstack-panel block.\n"))
    (dolist (panel panels)
      (let ((lead (+agent-shell-pstack-panel-lead panel)))
        (+agent-shell-pstack-panels--line
         lead
         (propertize (+agent-shell-pstack-panel-id panel) 'face 'bold)
         (format "  %s  %s"
                 (or (+agent-shell-pstack-panel-shape panel) "-")
                 (+agent-shell-pstack-panels--status panel)))
        (+agent-shell-pstack-panels--line
         lead "  Lead: " (+agent-shell-pstack-panels--name lead))
        (when-let* ((rejection (+agent-shell-pstack-panel-rejection panel)))
          (+agent-shell-pstack-panels--line lead "  " rejection))
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (let ((report (+agent-shell-pstack-panel-member-report member))
                (shell (+agent-shell-pstack-panel-member-shell member)))
            (+agent-shell-pstack-panels--line
             (or report shell)
             (format "  %-16s %-12s %-9s "
                     (+agent-shell-pstack-panel-member-role member)
                     (+agent-shell-pstack-panel-member-agent member)
                     (+agent-shell-pstack-panel-member-state member))
             (cond (report (abbreviate-file-name report))
                   ((+agent-shell-pstack-panel-member-error member))
                   (t (+agent-shell-pstack-panels--name shell))))))
        (insert "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar-keymap +agent-shell-pstack-panel--mode-line-map
  :doc "Keymap for a lead's panel mode-line item."
  "<mode-line> <mouse-1>" #'+agent-shell-pstack-panels)

(defun +agent-shell-pstack-panel--mode-line-text (lead)
  "Return mode-line text for LEAD's running panels, or nil."
  (when-let* ((panels (seq-filter
                       (lambda (panel)
                         (and (eq lead (+agent-shell-pstack-panel-lead panel))
                              (not (+agent-shell-pstack-panel-rejection panel))
                              (not (+agent-shell-pstack-panel-followup panel))))
                       (+agent-shell-pstack-panel-list))))
    (let ((members (mapcan (lambda (panel)
                             (copy-sequence
                              (+agent-shell-pstack-panel-members panel)))
                           panels)))
      (propertize
       (format "%s %d/%d " (if (cdr panels) "panels" "panel")
               (seq-count #'+agent-shell-pstack-panel--settled-p members)
               (length members))
       'face '+agent-shell-pstack-mode-line
       'mouse-face 'mode-line-highlight
       'local-map +agent-shell-pstack-panel--mode-line-map
       'help-echo
       (concat
        (mapconcat
         (lambda (panel)
           (concat
            (format "pstack panel %s (%s)" (+agent-shell-pstack-panel-id panel)
                    (+agent-shell-pstack-panel-shape panel))
            (mapconcat
             (lambda (member)
               (format "\n  %s  %s  %s"
                       (+agent-shell-pstack-panel-member-role member)
                       (+agent-shell-pstack-panel-member-agent member)
                       (+agent-shell-pstack-panel-member-state member)))
             (+agent-shell-pstack-panel-members panel) "")))
         panels "\n")
        "\nmouse-1: show panels")))))

(defun +agent-shell-pstack-panel--update-mode-lines ()
  "Update the mode line of each lead whose panels changed."
  (dolist (lead (seq-uniq (mapcar #'+agent-shell-pstack-panel-lead
                                  (+agent-shell-pstack-panel-list))))
    (when (buffer-live-p lead)
      (let ((text (+agent-shell-pstack-panel--mode-line-text lead)))
        (with-current-buffer lead
          ;; Redisplay adds properties to the shown string.  The help-echo
          ;; determines the label, so it alone tells whether anything changed.
          (unless (equal (and text (get-text-property 0 'help-echo text))
                         (and +agent-shell-pstack-panel--mode-line
                              (get-text-property
                               0 'help-echo
                               +agent-shell-pstack-panel--mode-line)))
            (setq +agent-shell-pstack-panel--mode-line text)
            (force-mode-line-update)))))))

(defun +agent-shell-pstack-panels--refresh ()
  "Redraw the panels buffer if it exists, and the leads' mode lines."
  (+agent-shell-pstack-panel--update-mode-lines)
  (when-let* ((buffer (get-buffer +agent-shell-pstack-panels--buffer)))
    (with-current-buffer buffer (+agent-shell-pstack-panels--render))))

;;;###autoload
(defun +agent-shell-pstack-panels ()
  "Show pstack panels, their members and reports."
  (interactive)
  (let ((buffer (get-buffer-create +agent-shell-pstack-panels--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-pstack-panels-mode)
        (+agent-shell-pstack-panels-mode))
      (+agent-shell-pstack-panels--render))
    (pop-to-buffer buffer)))

(defun +agent-shell-pstack-panels-visit ()
  "Visit the report, member shell or lead shell on the current line."
  (interactive)
  (pcase (get-text-property (point) '+agent-shell-pstack-panels-target)
    ((and (pred stringp) file) (find-file-other-window file))
    ((and (pred buffer-live-p) buffer) (pop-to-buffer buffer))
    (_ (user-error "Nothing to visit on this line"))))

;;;; Mode

(defun +agent-shell-pstack-panel--track ()
  "Watch the current shell as a lead while `+agent-shell-pstack-mode' is on."
  (if +agent-shell-pstack-mode
      (+agent-shell-drive-watch (current-buffer) 'pstack-lead (current-buffer)
                                #'+agent-shell-pstack-panel--lead-event)
    (+agent-shell-drive-unwatch 'pstack-lead (current-buffer))))

;;;###autoload
(define-minor-mode +agent-shell-pstack-panel-mode
  "Let pstack leads request cross-vendor panels from Emacs.
A lead is a shell with `+agent-shell-pstack-mode' on.  Its first prompt
explains the request protocol; see `+agent-shell-pstack-panel-shapes'."
  :global t
  :group '+agent-shell-pstack-panel
  (if +agent-shell-pstack-panel-mode
      (progn
        (add-hook '+agent-shell-pstack-mode-hook
                  #'+agent-shell-pstack-panel--track)
        (add-to-list '+agent-shell-pstack-prime-texts
                     #'+agent-shell-pstack-panel--protocol t)
        (add-to-list '+agent-shell-pstack-reminder-texts
                     #'+agent-shell-pstack-panel--reminder t)
        (add-to-list 'mode-line-misc-info
                     +agent-shell-pstack-panel--mode-line-construct)
        (dolist (buffer (buffer-list))
          (when (buffer-local-value '+agent-shell-pstack-mode buffer)
            (with-current-buffer buffer (+agent-shell-pstack-panel--track)))))
    (remove-hook '+agent-shell-pstack-mode-hook
                 #'+agent-shell-pstack-panel--track)
    (setq +agent-shell-pstack-prime-texts
          (delq #'+agent-shell-pstack-panel--protocol
                +agent-shell-pstack-prime-texts)
          +agent-shell-pstack-reminder-texts
          (delq #'+agent-shell-pstack-panel--reminder
                +agent-shell-pstack-reminder-texts))
    (setq mode-line-misc-info
          (delete +agent-shell-pstack-panel--mode-line-construct
                  mode-line-misc-info))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (kill-local-variable '+agent-shell-pstack-panel--mode-line)))
    (force-mode-line-update t)
    (+agent-shell-drive-unwatch 'pstack-lead)))

(provide '+agent-shell-pstack-panel)
;;; +agent-shell-pstack-panel.el ends here
