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
;; An arena panel instead snapshots the lead's tree, runs each candidate in
;; a sandboxed worktree, captures each result as a commit, has a read-only
;; judge score the results against a rubric no candidate saw, and sends the
;; lead one follow-up to pick, graft and verify from.  Its results stay on
;; disk until `+agent-shell-pstack-arena-clean' or x in the panels view.
;;
;; Panels live in memory.  Only live `turn-complete' events are read, so a
;; restored session never starts a panel, and a request seen before is
;; ignored.  A lead's mode line shows its running panels.  See
;; docs/AGENT-SHELL-PSTACK.md and docs/AGENT-SHELL-PSTACK-ARENA.md.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'rx)
(require 'ring)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-drive)
(require '+agent-shell-pstack)
(require '+agent-shell-arena-git)
(require '+agent-shell-sandbox)

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
Findings that several agents agree on are high signal.")
    (arena
     :summary "candidates each implement the brief in a sandboxed worktree, \
then a read-only judge scores them against your rubric"
     :contract "Implement the task in the current directory, a snapshot of \
the lead's working tree in its own git worktree.  Work only there: a sandbox \
refuses writes anywhere else.  Leave your changes in the tree, uncommitted; \
Emacs captures them when your turn ends.  Do not run git commands that \
write, such as git add, commit, stash or checkout: the worktree's index \
lives outside the sandbox, so they fail.  End your turn with a rationale as \
your final message: what you built, the alternatives you considered, and \
what you rejected and why.  Nobody will answer questions during the turn."
     :judge "Read every candidate's checkout, not only its rationale.  Score \
each candidate on each rubric criterion from 1 to 5 with a one-line reason.  \
Then recommend one candidate as the base, with reasons, and name parts of \
the others worth grafting onto it.  End your turn with the scores and the \
recommendation as your final message.  Nobody will answer questions during \
the turn."
     :triage "Pick, graft and verify as the arena skill's lead.  Read every \
candidate's diff yourself and score it against your rubric, then compare \
your scores with the judge's.  Pick one candidate as the base and apply its \
diff with its command above.  Graft the best parts of the others by hand, \
never by checking their files out over your tree.  Then verify the result \
as the arena skill says.")
    (delegate
     :summary "one agent you choose implements the task, tests it and \
commits on a branch of its own, in a shell the user can watch"
     :contract "You work alone, with full write access, in your agent's \
full-access mode, so nobody is asked for permission; a permission request \
ends your turn.  Implement the task, run the repository's tests, and commit \
your work with git on the branch you are on, in small commits with clear \
messages.  Do not switch branches, rewrite history or push.  End your turn \
with a report as your final message: what you built, how you tested it, and \
what a reviewer should know.  Nobody answers questions during the turn."
     :sandbox-contract "You work alone in a sandbox that confines writes to \
your workspace, in your agent's full-access mode, so nobody is asked for \
permission; a permission request ends your turn.  Git cannot write its \
metadata there, so do not commit, stash, switch branches or write refs.  \
Implement and verify the task.  Leave your changes uncommitted.  End your \
final message with a report: what you built, how you tested it, and what a \
reviewer should know.  Then end it with exactly one block of this form, \
using this repository's commit style.  Give the subject and body for the \
single commit Emacs will make.  Do not include attribution trailers or any \
text after the block.

```pstack-commit
{\"version\":1,\"subject\":\"Summary line\",\"body\":\"What changed and why.\"}
```

Nobody answers questions during the turn."
     :triage "Review the branch before you merge it: read its commits and \
its diff against the commit it started from, and run the tests yourself.  \
Then merge it, ask for changes in a new delegate panel, or take the work \
over."))
  "Panel shapes by name.
Each value is a plist: :summary describes the shape to the lead, :contract
ends each member's prompt, and :triage ends the follow-up to the lead.  An
arena's :judge ends its judge's prompt, and a sandboxed delegate's prompt
ends with :sandbox-contract instead of :contract."
  :type '(alist :key-type symbol :value-type plist))

(defcustom +agent-shell-pstack-panel-default-judge-timeout 900
  "Seconds an arena's judge may run when the request names none."
  :type 'natnum)

(defcustom +agent-shell-pstack-panel-default-delegate-timeout 2700
  "Seconds a delegate may run when the request names none.
A delegate implements, tests and commits, not just reviews, so this
defaults higher than `+agent-shell-pstack-panel-default-timeout'.  It is
still capped by `+agent-shell-pstack-panel-max-timeout'."
  :type 'natnum)

(defcustom +agent-shell-pstack-arena-measure-egress nil
  "Whether new arenas record where each candidate and the judge connect.
Each member's record goes beside its report as ROLE.egress.json, and
the follow-up gains two lines about them.  A remote arena takes the
value its panel had.  See docs/AGENT-SHELL-PSTACK-ARENA.md."
  :type 'boolean
  :group '+agent-shell-pstack-panel)

(defconst +agent-shell-pstack-panel--egress-wait 30
  "Seconds a measured arena's follow-up waits for its members' records.")

(defconst +agent-shell-pstack-panel--rubric-range '(3 . 6)
  "Fewest and most criteria an arena rubric may list.")

(defconst +agent-shell-pstack-panel--schema 10
  "Layout of the panel and member structs.
Slots are only appended.  Bump this whenever either struct gains one.")

(cl-defstruct (+agent-shell-pstack-panel (:constructor nil)
                                         (:constructor
                                          +agent-shell-pstack-panel--make)
                                         (:copier nil))
  "A panel request and its progress.
PHASE moves through the shape's phases and ends at `finished'; see
`+agent-shell-pstack-panel--advance'.  A panel with a SINK has no lead:
once finished, the SINK is called with the panel in place of delivery.
REMOTE holds a `+agent-shell-arena-remote' when a host runs the arena
or delegate.  A delegate's ARENA is its manifest, whose delegate record
says how it works and what it left; see `+agent-shell-arena-git-delegate'.
A delegate's DELEGATE-OPTIONS is its policy, frozen once by
`+agent-shell-arena-git-delegate-options', and IDENTITY the laptop
user's `+agent-shell-arena-git-user' when it needs one.  A panel a host's
runner made has RUNNER, a plist of :observer, called with each member
and event of its, and :lease, returning a member's process lease.
EXPIRES is the time its current phase's timer expires it."
  id shape brief timeout lead cwd members timer rejection followup delivered
  interrupted (created (float-time))
  (schema +agent-shell-pstack-panel--schema) (phase 'running)
  rubric judge-timeout arena sink remote
  (deadline nil :documentation "Absolute time an arena should end, set once.")
  (login-check nil :documentation "Cancels login admission until it ends.")
  (logins nil :documentation "Each agent's login result, as admitted.")
  (measure-egress nil :documentation "Whether its members' egress is \
recorded.")
  delegate-options identity runner expires)

(defvar +agent-shell-pstack-panel-change-hook nil
  "Hook run after any panel changes, with no arguments.")

(defvar +agent-shell-pstack-arena-extra-deny-reads nil
  "More folders no arena candidate may read, such as a runner's records.")

(defcustom +agent-shell-pstack-panel-retry-delay 60
  "Seconds before a member a transient provider failure stopped tries again.
The member's shell is asked once to continue where it left off.  A retry
that would start after its phase expires or its arena's deadline is not
made; see `+agent-shell-pstack-panel--fail'."
  :type 'natnum)

(defcustom +agent-shell-pstack-panel-arena-preparation-allowance 1800
  "Seconds allowed for provisioning and host login admission in an arena."
  :type 'natnum)

(defcustom +agent-shell-pstack-panel-arena-overhead 300
  "Seconds of arena budget beyond preparation, candidate and judge timeouts."
  :type 'natnum)

(defvar +agent-shell-pstack-panel-arena-grace 120
  "Seconds past an arena's deadline its capture may take.
Its members' logins must outlast this too.")

(defun +agent-shell-pstack-panel-arena-budget (timeout judge-timeout)
  "Return the seconds an arena may take with TIMEOUT and JUDGE-TIMEOUT.
Candidates run in parallel, so their timeout counts once."
  (+ +agent-shell-pstack-panel-arena-preparation-allowance
     timeout judge-timeout +agent-shell-pstack-panel-arena-overhead))

(defun +agent-shell-pstack-panel--horizon (panel)
  "Return the time PANEL's sandboxed members' logins must outlast."
  (when-let* ((deadline (+agent-shell-pstack-panel-deadline panel)))
    (+ deadline +agent-shell-pstack-panel-arena-grace)))

(declare-function +agent-shell-egress-open "+agent-shell-egress")
(declare-function +agent-shell-egress-close "+agent-shell-egress")
(declare-function +agent-shell-egress-record-state "+agent-shell-egress")
(declare-function +agent-shell-egress-record-reason "+agent-shell-egress")
(declare-function +agent-shell-egress-record-limits "+agent-shell-egress")
(declare-function +agent-shell-egress-record-written "+agent-shell-egress")
(declare-function +agent-shell-egress-record-destinations
                  "+agent-shell-egress")
(declare-function +agent-shell-arena-remote-launch "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-cancel "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-abandon "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-describe
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-clean "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-recorded-p
                  "+agent-shell-arena-remote")

(cl-defstruct (+agent-shell-pstack-panel-member
               (:constructor nil)
               (:constructor +agent-shell-pstack-panel--make-member)
               (:copier nil))
  "One panel member.
STATE starts at `pending' (waiting for its phase) or `starting' (shell
initializing), moves to `waiting' (initialized, brief unsent) and `running'
(brief sent), then settles once as `done', `error' or `timeout'.  KIND
says which phase runs the member.

A candidate has a SANDBOX.  Once settled, its CAPTURE moves from
`stopping' to `stopped'.  Once every candidate has stopped, it moves to
`capturing' and ends as `captured' or `failed'.  A candidate that never
started fails once stopped.  RESULT then holds :commit, :ref, :changed,
:stat and :checkout, or :error.

A delegate's CAPTURE takes the same steps without waiting for others.
Its RESULT holds :survivors, the pids that outlived its stop, and :error
when collecting failed; what it left is in its panel's manifest.

FAILURE is the last provider failure from `+agent-shell-drive-failure'
that ended one of its turns.  RETRY is the timer of its pending retry,
then t once it was asked to continue."
  role agent focus shell (state 'starting) text error report (kind 'member)
  sandbox capture result
  (egress-file nil :documentation "Where its egress record is saved.")
  (egress-record nil :documentation "Its final `+agent-shell-egress-record'.")
  failure retry)

(defvar +agent-shell-pstack-panel--panels (make-hash-table :test #'equal)
  "Map panel id to panel, including rejected requests.")

(defvar +agent-shell-pstack-reload--carrying)

(defun +agent-shell-pstack-panel--current-p (panel)
  "Return whether PANEL and its arena manifest have the current structs.
A manifest carries no schema, so its slot count stands in for one."
  (and (eql (ignore-errors (+agent-shell-pstack-panel-schema panel))
            +agent-shell-pstack-panel--schema)
       (let ((arena (+agent-shell-pstack-panel-arena panel)))
         (or (null arena)
             (= (length arena)
                (length (cl-struct-slot-info
                         '+agent-shell-arena-git-manifest)))))))

;; A reload that changed a struct would break every older record in place.
;; `+agent-shell-pstack-reload' carries them over instead, so it skips this.
(unless (bound-and-true-p +agent-shell-pstack-reload--carrying)
  (let (dropped kept)
    (maphash (lambda (id panel)
               (unless (+agent-shell-pstack-panel--current-p panel)
                 (if (eq 'finished
                         (ignore-errors (+agent-shell-pstack-panel-phase panel)))
                     (push id dropped)
                   (push id kept))))
             +agent-shell-pstack-panel--panels)
    (dolist (id dropped)
      (remhash id +agent-shell-pstack-panel--panels))
    (when dropped
      (message "Dropped %d finished pstack panel%s made before the panel \
code changed" (length dropped) (if (cdr dropped) "s" "")))
    (when kept
      (message "Kept %d unfinished pstack panel%s (%s) made with an older \
layout; `+agent-shell-pstack-reload' carries them over"
               (length kept) (if (cdr kept) "s" "")
               (string-join (sort kept #'string<) ", ")))))

(defconst +agent-shell-pstack-panel--settled '(done error timeout)
  "Member states that end a member.")

(defconst +agent-shell-pstack-panel--phase-kinds
  '((running . member) (candidates . candidate) (judging . judge)
    (delegating . delegate))
  "The kind of member each phase runs.
Expiry and abandonment reach only the current phase's members.")

(defconst +agent-shell-pstack-panel--failed-stops
  '("cancelled" "refusal" "max_tokens" "max_turn_requests")
  "Stop reasons that end a member's turn without a report.")

(defvar-local +agent-shell-pstack-panel--mode-line nil
  "Mode-line text for this lead's running panels, or nil.")
(put '+agent-shell-pstack-panel--mode-line 'risky-local-variable t)

(defconst +agent-shell-pstack-panel--mode-line-construct
  '("" +agent-shell-pstack-panel--mode-line)
  "Mode-line construct showing `+agent-shell-pstack-panel--mode-line'.")

;;;; Request diagnostics

(defvar +agent-shell-pstack-panel--logs (make-hash-table :test #'eq)
  "Map each lead buffer to a ring of its last 50 handled events.")

(defvar +agent-shell-pstack-panel--request-digests (make-hash-table :test #'equal)
  "Map panel ids to full request digests, without retaining request text.")

(defvar +agent-shell-pstack-panel--log-entry nil
  "Dynamically bound entry for the lead event being handled.")

(defun +agent-shell-pstack-panel--excerpt (text)
  "Return at most the first 200 characters of TEXT without properties."
  (when text (substring-no-properties text 0 (min 200 (length text)))))

(defun +agent-shell-pstack-panel--trailing-block-text (text)
  "Return TEXT's trailing pstack-panel fence and contents, or nil."
  (when text
    (let ((start 0) trailing)
      (while (string-match (rx bol "```pstack-panel" (* blank) "\n")
                           text start)
        (let ((open (match-beginning 0)))
          (setq start (match-end 0))
          (if (string-match (rx bol "```" (* blank) eol) text start)
              (progn
                (setq start (match-end 0))
                (when (string-blank-p (substring text start))
                  (setq trailing (substring text open))))
            (setq start (length text)))))
      trailing)))

(defun +agent-shell-pstack-panel--ends-with-block-p (text)
  "Return whether TEXT ends with a complete pstack-panel fence."
  (and (+agent-shell-pstack-panel--trailing-block-text text) t))

(defun +agent-shell-pstack-panel--log-event (lead session kind text)
  "Record LEAD's KIND event in SESSION with bounded excerpts of TEXT."
  (let* ((entry (list :time (current-time) :lead (buffer-name lead)
                      :session session :kind kind :length (length text)
                      :source 'final :ends-with-block nil :block nil
                      :excerpt (+agent-shell-pstack-panel--excerpt text)
                      :block-first-line nil
                      :outcome 'no-block :id nil :reason nil))
         (log (or (gethash lead +agent-shell-pstack-panel--logs)
                  (puthash lead (make-ring 50)
                           +agent-shell-pstack-panel--logs))))
    (ring-insert log entry)
    entry))

(defun +agent-shell-pstack-panel--log-request-text (final text)
  "Record FINAL's fence status and the block requested in recovered TEXT."
  (let ((block (+agent-shell-pstack-panel--block text)))
    (setf (plist-get +agent-shell-pstack-panel--log-entry :source)
          (if (equal text final) 'final 'turn-text)
          (plist-get +agent-shell-pstack-panel--log-entry :ends-with-block)
          (+agent-shell-pstack-panel--ends-with-block-p final)
          (plist-get +agent-shell-pstack-panel--log-entry :block) (and block t)
          (plist-get +agent-shell-pstack-panel--log-entry :block-first-line)
          (when block
            (+agent-shell-pstack-panel--excerpt
             (car (split-string block "\n")))))))

(defun +agent-shell-pstack-panel--log-outcome (outcome &optional id reason)
  "Set the current event's OUTCOME, panel ID and bounded REASON."
  (setf (plist-get +agent-shell-pstack-panel--log-entry :outcome) outcome
        (plist-get +agent-shell-pstack-panel--log-entry :id) id
        (plist-get +agent-shell-pstack-panel--log-entry :reason)
        (+agent-shell-pstack-panel--excerpt reason)))

(defun +agent-shell-pstack-panel--log-error (err)
  "Record and message request error ERR."
  (+agent-shell-pstack-panel--log-outcome
   'error (plist-get +agent-shell-pstack-panel--log-entry :id)
   (error-message-string err))
  (message "pstack panel request (%s): %s"
           (plist-get +agent-shell-pstack-panel--log-entry :lead)
           (plist-get +agent-shell-pstack-panel--log-entry :reason)))

(defun +agent-shell-pstack-panel--log-outcome-text (entry)
  "Describe the outcome of log ENTRY on one line."
  (string-join
   (delq nil (list (pcase (plist-get entry :outcome)
                     ('no-block "no block")
                     ('started "panel started")
                     ('rejected "block rejected")
                     ('duplicate "duplicate of existing panel")
                     (other (symbol-name other)))
                   (plist-get entry :id)
                   (when-let* ((reason (plist-get entry :reason)))
                     (replace-regexp-in-string "[\n\r]+" " " reason))))
   " — "))

(defun +agent-shell-pstack-panel--lead-at-point ()
  "Return the lead of the panels row at point, or the current buffer."
  (or (get-text-property (point) '+agent-shell-pstack-panels-lead)
      (when-let* ((panel (get-text-property
                         (point) '+agent-shell-pstack-panels-panel)))
        (+agent-shell-pstack-panel-lead panel))
      (current-buffer)))

;;;###autoload
(defun +agent-shell-pstack-panel-log (&optional lead)
  "Show LEAD's bounded event log, newest first, in a read-only buffer.
Interactively use the lead at point in the panels view or the current shell."
  (interactive)
  (setq lead (or lead (+agent-shell-pstack-panel--lead-at-point)))
  (let ((log (gethash lead +agent-shell-pstack-panel--logs))
        (buffer (get-buffer-create "*pstack panel log*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Panel events for %s (newest first)\n\n"
                        (+agent-shell-pstack-panels--name lead)))
        (if (or (null log) (ring-empty-p log))
            (insert "No handled lead events.\n")
          (dolist (entry (ring-elements log))
            (insert (format "%s  %s  lead=%S session=%S length=%d \
ends-with-block=%s source=%s\n  %s\n"
                            (format-time-string "%F %T" (plist-get entry :time))
                            (plist-get entry :kind) (plist-get entry :lead)
                            (plist-get entry :session) (plist-get entry :length)
                            (plist-get entry :ends-with-block)
                            (plist-get entry :source)
                            (+agent-shell-pstack-panel--log-outcome-text entry)))
            (when (plist-get entry :excerpt)
              (insert (format "  Text: %S\n" (plist-get entry :excerpt))))
            (when (plist-get entry :block-first-line)
              (insert (format "  Block first line: %S\n"
                              (plist-get entry :block-first-line)))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

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

(defun +agent-shell-pstack-panel--candidate-agents ()
  "Return the agent identifiers an arena candidate may use."
  (+agent-shell-drive-sandbox-agents))

(defun +agent-shell-pstack-panel--names (symbols)
  "Return SYMBOLS as a comma-separated string."
  (if symbols (mapconcat #'symbol-name symbols ", ") "none"))

(defun +agent-shell-pstack-panel--object-p (value)
  "Return whether VALUE is a parsed JSON object."
  (and (consp value) (consp (car value))))

(defun +agent-shell-pstack-panel--parse-member (value agents kind)
  "Return a member of KIND from request VALUE, one of AGENTS."
  (unless (+agent-shell-pstack-panel--object-p value)
    (user-error "Each member must be an object"))
  (let ((role (map-elt value 'role))
        (agent (map-elt value 'agent))
        (focus (map-elt value 'focus)))
    (unless (and (stringp role)
                 (string-match-p (rx bos (any "a-z0-9") (* (any "a-z0-9-")) eos)
                                 role))
      (user-error "Member role %S must be lowercase letters, digits and \
hyphens" role))
    (unless (and (stringp agent) (memq (intern agent) agents))
      (user-error "Member %s has agent %S; available agents: %s" role agent
                  (+agent-shell-pstack-panel--names agents)))
    (unless (or (null focus) (stringp focus))
      (user-error "Member %s has a focus that is not a string" role))
    (+agent-shell-pstack-panel--make-member
     :role role :agent (intern agent) :focus focus :kind kind
     :state (if (eq kind 'member) 'starting 'pending))))

(defun +agent-shell-pstack-panel--seconds (request key default name)
  "Return REQUEST's KEY, or DEFAULT, as seconds; NAME the field in errors."
  (let ((seconds (or (map-elt request key) default)))
    (unless (and (natnump seconds)
                 (<= 1 seconds +agent-shell-pstack-panel-max-timeout))
      (user-error "%s must be a whole number of seconds from 1 to %d" name
                  +agent-shell-pstack-panel-max-timeout))
    seconds))

(defun +agent-shell-pstack-panel--parse-rubric (rubric)
  "Return RUBRIC, a list of criteria, or signal `user-error' why not."
  (pcase-let ((`(,least . ,most) +agent-shell-pstack-panel--rubric-range))
    (unless (and (proper-list-p rubric) (<= least (length rubric) most)
                 (seq-every-p (lambda (criterion)
                                (and (stringp criterion)
                                     (not (string-blank-p criterion))))
                              rubric))
      (user-error "Rubric must list %d to %d non-empty strings" least most))
    rubric))

(defun +agent-shell-pstack-panel--parse-judge (value lead-agent)
  "Return the judge member from request VALUE.
Without VALUE, prefer an agent other than LEAD-AGENT.  The judge runs in
a read-only sandbox, so it needs an agent that can run sandboxed."
  (let* ((agents (+agent-shell-pstack-panel--candidate-agents))
         (agent
          (if (null value)
              (or (seq-find (lambda (agent) (not (eq agent lead-agent))) agents)
                  (car agents)
                  (user-error "No sandboxed agent can judge the arena"))
            (let ((name (and (+agent-shell-pstack-panel--object-p value)
                             (map-elt value 'agent))))
              (unless (and (stringp name) (memq (intern name) agents))
                (user-error "Judge must be an object whose agent can run \
sandboxed; available agents: %s" (+agent-shell-pstack-panel--names agents)))
              (intern name)))))
    (+agent-shell-pstack-panel--make-member
     :role "judge" :agent agent :kind 'judge :state 'pending)))

(defun +agent-shell-pstack-panel--parse (block &optional lead-agent)
  "Return a panel described by request BLOCK, or signal `user-error' why not.
LEAD-AGENT is the requesting lead's agent identifier."
  (let* ((request (condition-case nil
                      (json-parse-string block :object-type 'alist
                                         :array-type 'list
                                         :null-object nil :false-object nil)
                    (json-parse-error
                     (user-error "The block is not valid JSON"))))
         (_ (unless (+agent-shell-pstack-panel--object-p request)
              (user-error "The block must hold a JSON object")))
         (shapes (mapcar #'car +agent-shell-pstack-panel-shapes))
         (shape (let ((name (map-elt request 'shape)))
                  (and (stringp name) (car (memq (intern name) shapes)))))
         (arena (eq shape 'arena))
         (brief (map-elt request 'brief))
         (values (map-elt request 'members)))
    (unless (equal (map-elt request 'version) 1)
      (user-error "Version must be 1"))
    (unless shape
      (user-error "Shape %S is unknown; shapes: %s" (map-elt request 'shape)
                  (+agent-shell-pstack-panel--names shapes)))
    (when (and arena (not +agent-shell-sandbox-backend))
      (user-error "Arena panels need a sandbox to confine the candidates, \
macOS's sandbox-exec or Linux's bwrap; this Emacs runs on %s" system-type))
    (unless (and (stringp brief) (not (string-blank-p brief)))
      (user-error "Brief must be a non-empty string"))
    (if (eq shape 'delegate)
        (+agent-shell-pstack-panel--parse-delegate request)
      (let ((timeout (+agent-shell-pstack-panel--seconds
                      request 'timeout +agent-shell-pstack-panel-default-timeout
                      "Timeout"))
            (agents (if arena (+agent-shell-pstack-panel--candidate-agents)
                      (+agent-shell-pstack-panel--agents))))
        (unless (and (proper-list-p values)
                     (<= 1 (length values)
                         +agent-shell-pstack-panel-max-members))
          (user-error "Members must list 1 to %d members"
                      +agent-shell-pstack-panel-max-members))
        (let* ((members (mapcar (lambda (value)
                                  (+agent-shell-pstack-panel--parse-member
                                   value agents (if arena 'candidate 'member)))
                                values))
               (roles (mapcar #'+agent-shell-pstack-panel-member-role members)))
          (when-let* ((duplicate (seq-find (lambda (role)
                                             (member role
                                                     (cdr (member role roles))))
                                           roles)))
            (user-error "Role %s appears more than once" duplicate))
          (if (not arena)
              (+agent-shell-pstack-panel--make
               :shape shape :brief brief :timeout timeout :members members)
            (when (member "judge" roles)
              (user-error "Role judge is reserved for the arena's judge"))
            (when (member "base" roles)
              (user-error "Role base is reserved for the ref holding the base"))
            (let ((rubric (+agent-shell-pstack-panel--parse-rubric
                           (map-elt request 'rubric)))
                  (judge (+agent-shell-pstack-panel--parse-judge
                          (map-elt request 'judge) lead-agent))
                  (judge-timeout
                   (+agent-shell-pstack-panel--seconds
                    request 'judge_timeout
                    +agent-shell-pstack-panel-default-judge-timeout
                    "Judge timeout")))
              (+agent-shell-pstack-panel--make
               :shape shape :brief brief :timeout timeout :rubric rubric
               :judge-timeout judge-timeout :phase 'preparing
               :measure-egress +agent-shell-pstack-arena-measure-egress
               :members (append members (list judge))))))))))

(defun +agent-shell-pstack-panel--parse-delegate (request)
  "Return the delegate panel REQUEST describes, or signal `user-error'.
REQUEST already has a valid version and brief."
  (when-let* ((fields (seq-filter (lambda (field)
                                    (map-contains-key request field))
                                  '(rubric judge judge_timeout))))
    (user-error "Delegate panels have no judge, so remove %s"
                (string-join (mapcar (lambda (field) (format "\"%s\"" field))
                                     fields)
                             " and ")))
  (let ((values (map-elt request 'members)))
    (unless (and (proper-list-p values) (= 1 (length values)))
      (user-error "Delegate panels take exactly one member, the agent you \
hand the task to."))
    (+agent-shell-pstack-panel--make
     :shape 'delegate :brief (map-elt request 'brief) :phase 'preparing
     :timeout (+agent-shell-pstack-panel--seconds
               request 'timeout
               +agent-shell-pstack-panel-default-delegate-timeout "Timeout")
     :members (list (+agent-shell-pstack-panel--parse-member
                     (car values)
                     (+agent-shell-pstack-panel--candidate-agents)
                     'delegate)))))

;;;; Prompts

(defun +agent-shell-pstack-panel--shapes ()
  "Return the entries of the shapes a lead may request here.
An arena needs a sandbox backend and an agent that can run sandboxed.  A
delegate needs an agent that can run full-access, but no sandbox: it is
never confined."
  (seq-remove (lambda (shape)
                (pcase (car shape)
                  ('arena (not (and +agent-shell-sandbox-backend
                                    (+agent-shell-pstack-panel--candidate-agents))))
                  ('delegate
                   (not (+agent-shell-pstack-panel--candidate-agents)))))
              +agent-shell-pstack-panel-shapes))

(defun +agent-shell-pstack-panel--series (shapes conjunction)
  "Return the names of SHAPES as a series joined by CONJUNCTION."
  (let ((names (mapcar (lambda (shape) (symbol-name (car shape))) shapes)))
    (if (cdr names)
        (format "%s %s %s" (string-join (butlast names) ", ") conjunction
                (car (last names)))
      (car names))))

(defun +agent-shell-pstack-panel--arena-protocol ()
  "Return the protocol paragraph about arenas."
  (format "\nAn arena's members are candidates that write code.  Each starts \
in its own sandboxed git worktree holding a snapshot of your working tree, and \
sees only the brief and its focus.  Candidate agents: %s.  An arena request \
also carries \"rubric\", %d to %d criteria that only the judge sees.  It may \
add \"judge\":{\"agent\":\"%s\"}, a candidate agent that by default differs \
from yours and reads the results in a read-only sandbox, and \"judge_timeout\", %d seconds by default; \"timeout\" bounds \
the candidates.  Emacs runs the candidates, then the judge, and sends you one \
prompt with each candidate's result commit, diff stat, rationale and the \
command that applies its diff, then the judge's scores and how to pick, \
graft and verify."
          (+agent-shell-pstack-panel--names
           (+agent-shell-pstack-panel--candidate-agents))
          (car +agent-shell-pstack-panel--rubric-range)
          (cdr +agent-shell-pstack-panel--rubric-range)
          (car (+agent-shell-pstack-panel--candidate-agents))
          +agent-shell-pstack-panel-default-judge-timeout))

(defun +agent-shell-pstack-panel--delegate-protocol ()
  "Return the protocol paragraph about delegates."
  (format "\nA delegate's one member writes code: it implements the brief, \
runs the tests and commits on a new branch, in a git worktree of its own, \
on the repository's host when its profile names one, or, where the profile \
says so, in your own checkout, which you then leave alone until the \
follow-up arrives.  Delegate agents: %s.  A delegate \
request has no judge, and its \"timeout\", %d seconds by default, \
bounds the whole run.  Emacs sends you one prompt with its report, its \
branch, its commits and how to review and merge them."
          (+agent-shell-pstack-panel--names
           (+agent-shell-pstack-panel--candidate-agents))
          +agent-shell-pstack-panel-default-delegate-timeout))

(defun +agent-shell-pstack-panel--protocol ()
  "Return the text telling a lead how to request a panel, or nil."
  (when-let* ((agents (+agent-shell-pstack-panel--agents)))
    (let ((shapes (+agent-shell-pstack-panel--shapes)))
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
Each how or interrogate member starts fresh, read-only, in your working \
directory, and sees only the brief and its own focus.  Use 1 to %d members \
with unique roles of lowercase letters, digits and hyphens.  The timeout \
is in seconds, at most %d.  End your turn right after the block.  Emacs starts the panel as soon as \
the block is complete, and interrupts your turn if you call a tool after it.
Do not wait or poll after the block.  When every member has finished or \
timed out, Emacs sends you a prompt with each report inline, the path it was \
saved to, and how to triage the reports.%s"
              (+agent-shell-pstack-panel--series shapes "or")
              (caar shapes) +agent-shell-pstack-panel-default-timeout
              (car agents)
              (mapconcat (lambda (shape)
                           (format "%s (%s)" (car shape)
                                   (plist-get (cdr shape) :summary)))
                         shapes "; ")
              (+agent-shell-pstack-panel--names agents)
              +agent-shell-pstack-panel-max-members
              +agent-shell-pstack-panel-max-timeout
              (concat
               (when (assq 'arena shapes)
                 (+agent-shell-pstack-panel--arena-protocol))
               (when (assq 'delegate shapes)
                 (+agent-shell-pstack-panel--delegate-protocol)))))))

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
      (format "Request %s panels from Emacs, never on your own subagents: \
end your final message with a pstack-panel block, then end your turn.  The \
protocol is in %s.  Reread it if it is no longer in context."
              (+agent-shell-pstack-panel--series
               (+agent-shell-pstack-panel--shapes) "and")
              file))))

(defun +agent-shell-pstack-panel--shape (panel key)
  "Return PANEL's shape property KEY."
  (plist-get (alist-get (+agent-shell-pstack-panel-shape panel)
                        +agent-shell-pstack-panel-shapes)
             key))

(defun +agent-shell-pstack-panel--brief (panel member)
  "Return the prompt that briefs MEMBER of PANEL."
  (let ((role (+agent-shell-pstack-panel-member-role member))
        (focus (when-let* ((focus (+agent-shell-pstack-panel-member-focus
                                   member)))
                 (format "\nYour focus:\n%s\n" focus))))
    (pcase (+agent-shell-pstack-panel-member-kind member)
      ('member
       (concat
        (format "You are the %s member of a pstack %s panel that Emacs runs \
for another agent.  You are read-only: Emacs refuses edits and commands that \
change state.\n\nBrief:\n%s\n"
                role (+agent-shell-pstack-panel-shape panel)
                (+agent-shell-pstack-panel-brief panel))
        focus "\n" (+agent-shell-pstack-panel--shape panel :contract)))
      ('candidate
       (concat
        (format "You are the %s candidate of a pstack arena that Emacs runs \
for another agent.  Other candidates work on the same task elsewhere.\n\n\
Task:\n%s\n"
                role (+agent-shell-pstack-panel-brief panel))
        focus "\n" (+agent-shell-pstack-panel--shape panel :contract)))
      ('delegate
       (let ((manifest (+agent-shell-pstack-panel-arena panel)))
         (concat
          (format "You are the %s delegate of a pstack panel that Emacs runs \
for another agent.  You work on branch %s, %s.\n\nTask:\n%s\n"
                  role (+agent-shell-arena-git-delegate-branch manifest)
                  (if (eq (+agent-shell-pstack-panel--isolation panel) 'branch)
                      "in the other agent's own checkout, which nobody else \
touches until you finish"
                    "in a git worktree of your own")
                  (+agent-shell-pstack-panel-brief panel))
          focus "\n" (+agent-shell-pstack-panel--shape
                      panel (if (+agent-shell-pstack-panel--sandboxed-p panel)
                                :sandbox-contract
                              :contract)))))
      ('judge (+agent-shell-pstack-panel--judge-brief panel)))))

(defun +agent-shell-pstack-panel--label (panel candidate)
  "Return CANDIDATE's letter among PANEL's candidates."
  (string (+ ?A (seq-position (+agent-shell-pstack-panel--kind-members
                               panel 'candidate)
                              candidate #'eq))))

(defun +agent-shell-pstack-panel--outcome (candidate)
  "Return a phrase saying how CANDIDATE's turn ended."
  (pcase (+agent-shell-pstack-panel-member-state candidate)
    ('done "finished its turn")
    ('timeout "timed out; its work was captured as it stood")
    (_ (format "ended early; its work was captured as it stood: %s"
               (+agent-shell-pstack-panel-member-error candidate)))))

(defun +agent-shell-pstack-panel--rationale (candidate)
  "Return CANDIDATE's final text, or a note that it left none."
  (let ((text (+agent-shell-pstack-panel-member-text candidate)))
    (if (and text (not (string-blank-p text))) (string-trim text)
      "(none)")))

(defun +agent-shell-pstack-panel--judge-brief (panel)
  "Return the prompt that briefs the judge of arena PANEL."
  (let* ((candidates (+agent-shell-pstack-panel--kind-members panel 'candidate))
         (usable (seq-filter #'+agent-shell-pstack-panel--usable-p candidates))
         (unusable (seq-remove #'+agent-shell-pstack-panel--usable-p
                               candidates)))
    (concat
     "You are the judge of a pstack arena that Emacs runs for another \
agent.  You run in a read-only sandbox: you can read the checkouts below \
and the repository's git objects, and write only your temp folder.  Each \
candidate below worked on the same task from the same base commit, alone \
in a sandbox.  "
     (format "A checkout holds the files its candidate changed, or every \
file when it changed more than %d.  Read any other file at the base with \
git -C CHECKOUT show %s:PATH."
             +agent-shell-arena-git-checkout-limit
             (+agent-shell-arena-git-manifest-base
              (+agent-shell-pstack-panel-arena panel)))
     "\n\nTask:\n"
     (+agent-shell-pstack-panel-brief panel)
     "\n\nRubric:\n"
     (mapconcat #'identity
                (seq-map-indexed (lambda (criterion index)
                                   (format "%d. %s" (1+ index) criterion))
                                 (+agent-shell-pstack-panel-rubric panel))
                "\n")
     "\n"
     (mapconcat
      (lambda (candidate)
        (let ((result (+agent-shell-pstack-panel-member-result candidate)))
          (format "\n## Candidate %s\nIt %s.\nCheckout: %s\nDiff against \
the base:\n%s\nRationale:\n%s\n"
                  (+agent-shell-pstack-panel--label panel candidate)
                  (+agent-shell-pstack-panel--outcome candidate)
                  (plist-get result :checkout) (plist-get result :stat)
                  (+agent-shell-pstack-panel--rationale candidate))))
      usable "")
     (when unusable
       (format "\nNot judged, because they left no usable change: %s.\n"
               (mapconcat (lambda (candidate)
                            (+agent-shell-pstack-panel--label panel candidate))
                          unusable ", ")))
     "\n" (+agent-shell-pstack-panel--shape panel :judge))))

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
  (cond
   ((+agent-shell-pstack-panel-rejection panel)
    (format "Emacs rejected your pstack-panel request: %s\nNo panel \
started.  To retry, end a message with a corrected pstack-panel block."
            (+agent-shell-pstack-panel-rejection panel)))
   ((+agent-shell-pstack-panel--arena-p panel)
    (+agent-shell-pstack-panel--arena-text panel))
   ((+agent-shell-pstack-panel--delegate-p panel)
    (+agent-shell-pstack-panel--delegate-text panel))
   (t
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
       (mapconcat #'+agent-shell-pstack-panel--member-text members "")
       "\n" (+agent-shell-pstack-panel--shape panel :triage))))))

(defun +agent-shell-pstack-panel--member-text (member)
  "Return MEMBER's section of a follow-up: its state, report and text."
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

(defun +agent-shell-pstack-panel--candidate-text (panel candidate)
  "Return CANDIDATE's section of arena PANEL's follow-up."
  (let* ((result (+agent-shell-pstack-panel-member-result candidate))
         (commit (plist-get result :commit))
         (manifest (+agent-shell-pstack-panel-arena panel)))
    (concat
     (format "\n## %s: %s (%s): %s\n"
             (+agent-shell-pstack-panel--label panel candidate)
             (+agent-shell-pstack-panel-member-role candidate)
             (+agent-shell-pstack-panel-member-agent candidate)
             (+agent-shell-pstack-panel-member-state candidate))
     (when-let* ((error (+agent-shell-pstack-panel-member-error candidate)))
       (format "%s\n" error))
     (cond
      ((plist-get result :error)
       (format "Capture failed: %s\n" (plist-get result :error)))
      ((not (plist-get result :changed))
       (if commit
           (format "Result: %s (%s), the same tree as the base.\n" commit
                   (plist-get result :ref))
         "No result.\n"))
      (t
       (concat
        (format "Result: %s (%s)\nDiff against the base:\n%s\nApply: (cd %s \
&& git diff --binary %s %s | git apply --3way)\n"
                commit (plist-get result :ref) (plist-get result :stat)
                (shell-quote-argument
                 (directory-file-name
                  (+agent-shell-arena-git-manifest-toplevel manifest)))
                (+agent-shell-arena-git-manifest-base manifest) commit)
        (when-let* ((paths (plist-get result :outside)))
          (format "Outside your sparse-checkout patterns: %s.  Applying \
the diff writes them into your tree anyway; leave them out if they were \
not meant to change.\n"
                  (string-join paths ", ")))
        (when-let* ((paths (plist-get result :lfs)))
          (format "Git LFS files: %s.  The diff changes their pointers, so \
applying it needs git-lfs, and git apply --3way cannot merge a conflict in \
one; graft such a file by hand.%s\n"
                  (string-join paths ", ")
                  (if (+agent-shell-pstack-panel-remote panel)
                      "  The bundle carried only the pointers, so their \
contents are still on the host."
                    ""))))))
     (format "Rationale:\n%s\n"
             (+agent-shell-pstack-panel--rationale candidate)))))

(defun +agent-shell-pstack-panel--arena-text (panel)
  "Return the part of arena PANEL's follow-up that reports its outcome."
  (let* ((candidates (+agent-shell-pstack-panel--kind-members panel 'candidate))
         (judge (car (+agent-shell-pstack-panel--kind-members panel 'judge)))
         (manifest (+agent-shell-pstack-panel-arena panel)))
    (if (null manifest)
        (format "pstack panel %s (arena) ended before its candidates \
started: %s"
                (+agent-shell-pstack-panel-id panel)
                (+agent-shell-pstack-panel-member-error judge))
      (concat
       (format "pstack panel %s (arena) has finished: %d of %d candidates \
changed the base.\n"
               (+agent-shell-pstack-panel-id panel)
               (seq-count #'+agent-shell-pstack-panel--usable-p candidates)
               (length candidates))
       (format "Base: %s, a snapshot of %s when the arena started.  Arena \
directory: %s\n"
               (+agent-shell-arena-git-manifest-base manifest)
               (directory-file-name
                (+agent-shell-arena-git-manifest-toplevel manifest))
               (directory-file-name
                (+agent-shell-arena-git-manifest-directory manifest)))
       (when-let* ((remote (+agent-shell-pstack-panel-remote panel)))
         (format "It ran on %s.\n"
                 (+agent-shell-arena-remote-describe remote)))
       (mapconcat (lambda (candidate)
                    (+agent-shell-pstack-panel--candidate-text panel candidate))
                  candidates "")
       (+agent-shell-pstack-panel--member-text judge)
       (+agent-shell-pstack-panel--egress-text panel)
       "\n" (+agent-shell-pstack-panel--shape panel :triage)
       "\nThe results stay until you clean the arena up: press x on its line \
in the pstack panels view, or run M-x +agent-shell-pstack-arena-clean."))))

(defun +agent-shell-pstack-panel--return-text (manifest)
  "Return what happened to the lead's checkout MANIFEST's delegate took."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (origin (+agent-shell-arena-git-delegate-origin delegate))
         (name (+agent-shell-arena-git-origin-name origin))
         (note (+agent-shell-arena-git-delegate-note delegate))
         (switch (pcase origin
                   (`(branch . ,_) (format "git switch %s" name))
                   (`(detached . ,commit)
                    (format "git switch --detach %s" commit)))))
    (pcase (+agent-shell-arena-git-delegate-restore delegate)
      ('restored (format "Your checkout was clean, so Emacs switched it back \
to %s." name))
      ('unneeded (format "Your checkout was already back on %s." name))
      ('dirty (format "Emacs left your checkout there instead of switching \
back to %s.  %s  Commit or stash, then run %s." name note switch))
      ((or 'changed 'missing) note)
      (_ (format "Emacs left your checkout there: %s  Once nothing runs \
there, press x on its line in the pstack panels view to switch back to %s."
                 (or note "it could not check it.") name)))))

(defun +agent-shell-pstack-panel--finalization-text (delegate where)
  "Return what came of sandboxed DELEGATE's one commit, for its follow-up.
DELEGATE is its `+agent-shell-arena-git-delegate'.  WHERE says where its
uncommitted work stays."
  (let ((finalization (+agent-shell-arena-git-delegate-finalization delegate)))
    (pcase (plist-get finalization :state)
      ('committed
       (format "Sandboxed: Emacs made its one commit, %s, as %s <%s>.\n"
               (substring (plist-get finalization :oid) 0 12)
               (plist-get finalization :name) (plist-get finalization :email)))
      ('empty "Sandboxed: it changed nothing, so Emacs made no commit.\n")
      (_ (format "Sandboxed: No commit created.  %s  Its changes stay \
uncommitted %s.\n"
                 (or (plist-get finalization :reason)
                     "Emacs never reached its commit.")
                 where)))))

(defun +agent-shell-pstack-panel--snapshot-text (manifest)
  "Return the line naming MANIFEST's snapshot base, or nil without one."
  (when-let* ((head (+agent-shell-arena-git-delegate-head
                     (+agent-shell-arena-git-manifest-delegate manifest))))
    (format "Started from: your uncommitted work, which Emacs snapshotted as \
commit %s on top of %s, your HEAD then.  Your own checkout, index and stash \
are unchanged.\n"
            (+agent-shell-arena-git-manifest-base manifest) head)))

(defun +agent-shell-pstack-panel--review-text (manifest)
  "Return how to review and merge MANIFEST's delegate branch."
  (let ((base (+agent-shell-arena-git-manifest-base manifest))
        (branch (+agent-shell-arena-git-delegate-branch manifest)))
    (if-let* ((head (+agent-shell-arena-git-delegate-head
                     (+agent-shell-arena-git-manifest-delegate manifest))))
        (format "\nTo review the delegate's own commits: git log %s..%s\n\
To review everything, the snapshot included: git log %s..%s\n\
To merge: merging the branch also brings the snapshot commit %s, so commit \
your own work first, or rebase the delegate's commits onto yours with \
git rebase --onto HEAD %s %s.\n"
                base branch head branch base base branch)
      (format "\nTo review: git log %s..%s\nTo merge: git merge --ff-only \
%s, or push it and open a pull request.\n" base branch branch))))

(defun +agent-shell-pstack-panel--remote-delegate-text (panel)
  "Return the outcome part of the follow-up of PANEL's delegate on a host."
  (let* ((member (car (+agent-shell-pstack-panel-members panel)))
         (role (+agent-shell-pstack-panel-member-role member))
         (remote (+agent-shell-pstack-panel-remote panel))
         (manifest (+agent-shell-pstack-panel-arena panel))
         (result (+agent-shell-pstack-panel-member-result member))
         (outcome (plist-get result :outcome))
         (base (+agent-shell-arena-git-manifest-base manifest))
         (branch (+agent-shell-arena-git-delegate-branch manifest))
         (commits (+agent-shell-arena-git-delegate-commits
                   (+agent-shell-arena-git-manifest-delegate manifest)))
         (published (plist-get result :published))
         (workspace (and outcome
                         (+agent-shell-arena-remote-outcome-workspace outcome)))
         (host (+agent-shell-arena-remote-describe remote))
         (dirty (and outcome (+agent-shell-arena-remote-outcome-dirty-count
                              outcome))))
    (concat
     (format "pstack panel %s (delegate) has finished: %s (%s): %s.\n"
             (+agent-shell-pstack-panel-id panel) role
             (+agent-shell-pstack-panel-member-agent member)
             (+agent-shell-pstack-panel-member-state member))
     (when-let* ((error (+agent-shell-pstack-panel-member-error member)))
       (format "%s\n" error))
     (format "Ran on: %s, %s.\n" host
             (if (+agent-shell-pstack-panel--sandboxed-p panel) "sandboxed"
               "full access"))
     (cond
      ((null outcome)
       "Branch: none came back; the host reported no delegate branch.\n")
      (published
       (concat
        (format "Branch: %s, %d commit%s since %s, the commit it started \
from%s\n"
                branch (length commits) (if (= 1 (length commits)) "" "s")
                base (if commits ":" "."))
        (mapconcat (lambda (commit)
                     (format "  %s %s\n" (substring (car commit) 0 12)
                             (cdr commit)))
                   commits "")))
      ((plist-get result :collision)
       (format "Branch: not fetched.  %s\n" (plist-get result :collision)))
      (t (format "Branch: not fetched: %s\n"
                 (or (plist-get result :error) "the results had none"))))
     (+agent-shell-pstack-panel--snapshot-text manifest)
     (when outcome
       (pcase (+agent-shell-arena-remote-outcome-finalization outcome)
         ('committed "Sandboxed: Emacs made its one commit on the host as \
your git identity.\n")
         ('empty "Sandboxed: it changed nothing, so Emacs made no commit.\n")
         ('blocked
          (format "Sandboxed: No commit created.  %s  Its changes stay \
uncommitted in %s on %s.\n"
                  (or (+agent-shell-arena-remote-outcome-retained-reason outcome)
                      "")
                  workspace
                  (+agent-shell-arena-remote--host-label
                   (+agent-shell-arena-remote-host remote))))))
     (when (and dirty (> dirty 0))
       (format "The host worktree has %d uncommitted path%s: %s.  They stay on \
the host, apart from the branch; cleaning the workspace up loses them.\n"
               dirty (if (= dirty 1) "" "s")
               (concat (string-join
                        (+agent-shell-arena-remote-outcome-dirty-paths outcome)
                        ", ")
                       (when (> dirty 10)
                         (format " and %d more" (- dirty 10))))))
     (when workspace (format "Workspace on host: %s\n" workspace))
     (when-let* ((report (+agent-shell-pstack-panel-member-report member)))
       (format "Report saved at: %s\n" report))
     (format "%s\n%s\n"
             (if (eq 'done (+agent-shell-pstack-panel-member-state member))
                 "Report:"
               "Partial text, not a final response:")
             (+agent-shell-pstack-panel--rationale member))
     (when (and published commits)
       (+agent-shell-pstack-panel--review-text manifest))
     "The host workspace stays until you clean it up: press x on its line in \
the pstack panels view, or run M-x +agent-shell-pstack-arena-clean.  The \
branch stays either way.\n"
     "\n" (+agent-shell-pstack-panel--shape panel :triage))))

(defun +agent-shell-pstack-panel--delegate-text (panel)
  "Return the part of delegate PANEL's follow-up that reports its outcome."
  (if (and (+agent-shell-pstack-panel-remote panel)
           (+agent-shell-pstack-panel-arena panel))
      (+agent-shell-pstack-panel--remote-delegate-text panel)
    (+agent-shell-pstack-panel--local-delegate-text panel)))

(defun +agent-shell-pstack-panel--local-delegate-text (panel)
  "Return the outcome part of the follow-up of PANEL's delegate here."
  (let* ((member (car (+agent-shell-pstack-panel-members panel)))
         (role (+agent-shell-pstack-panel-member-role member))
         (manifest (+agent-shell-pstack-panel-arena panel))
         (result (+agent-shell-pstack-panel-member-result member))
         (error (plist-get result :error))
         (dropped (plist-get result :dropped)))
    (if (not (and manifest (+agent-shell-pstack-panel-member-shell member)))
        (format "pstack panel %s (delegate) ended before its delegate \
started: %s"
                (+agent-shell-pstack-panel-id panel)
                (+agent-shell-pstack-panel-member-error member))
      (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
             (base (+agent-shell-arena-git-manifest-base manifest))
             (branch (+agent-shell-arena-git-delegate-branch manifest))
             (commits (+agent-shell-arena-git-delegate-commits delegate))
             (worktree (eq (+agent-shell-arena-git-delegate-isolation delegate)
                           'worktree)))
        (concat
         (format "pstack panel %s (delegate) has finished: %s (%s): %s.\n"
                 (+agent-shell-pstack-panel-id panel) role
                 (+agent-shell-pstack-panel-member-agent member)
                 (+agent-shell-pstack-panel-member-state member))
         (when-let* ((error (+agent-shell-pstack-panel-member-error member)))
           (format "%s\n" error))
         (when error
           (format "Emacs could not collect what it left: %s\n" error))
         (if dropped
             (format "Branch: none.  The delegate made no commits since %s, \
the commit it started from, so Emacs removed its empty branch %s.\n"
                     base branch)
           (format "Branch: %s, %d commit%s since %s, the commit it started \
from%s\n"
                   branch (length commits) (if (= 1 (length commits)) "" "s")
                   base (if commits ":" ".")))
         (mapconcat (lambda (commit)
                      (format "  %s %s\n" (substring (car commit) 0 12)
                              (cdr commit)))
                    commits "")
         (+agent-shell-pstack-panel--snapshot-text manifest)
         (when (+agent-shell-arena-git-delegate-sandbox delegate)
           (+agent-shell-pstack-panel--finalization-text
            delegate (if worktree "in its workspace"
                             "in your checkout")))
         (if worktree
             (concat
              (format "Workspace: %s\n"
                      (+agent-shell-arena-git-directory
                       manifest (+agent-shell-arena-git-role manifest role)))
              (when-let* ((note (+agent-shell-arena-git-delegate-note
                                 delegate)))
                (format "%s  Cleaning the workspace up loses them.\n" note)))
           (format "Isolation: your own checkout, on %s while it ran.  %s\n"
                   branch (+agent-shell-pstack-panel--return-text manifest)))
         (format "Report:\n%s\n"
                 (+agent-shell-pstack-panel--rationale member))
         (unless dropped
           (+agent-shell-pstack-panel--review-text manifest))
         (when worktree
           "The workspace stays until you clean it up: press x on its line in \
the pstack panels view, or run M-x +agent-shell-pstack-arena-clean.  The \
branch stays either way.\n")
         "\n" (+agent-shell-pstack-panel--shape panel :triage))))))

;;;; Egress

(defun +agent-shell-pstack-panel--egress-members (panel)
  "Return arena PANEL's candidates and judge."
  (seq-remove (lambda (member)
                (eq 'member (+agent-shell-pstack-panel-member-kind member)))
              (+agent-shell-pstack-panel-members panel)))

(defun +agent-shell-pstack-panel--egress-file (panel member)
  "Return where MEMBER of PANEL keeps its egress record, and note it."
  (setf (+agent-shell-pstack-panel-member-egress-file member)
        (expand-file-name
         (concat (+agent-shell-pstack-panel-member-role member) ".egress.json")
         (expand-file-name (+agent-shell-pstack-panel-id panel)
                           +agent-shell-pstack-panel-directory))))

(defun +agent-shell-pstack-panel--egress-identity (panel member)
  "Return the identity MEMBER of PANEL's egress record carries."
  (list :panel (+agent-shell-pstack-panel-id panel)
        :arena (when-let* ((manifest (+agent-shell-pstack-panel-arena panel)))
                 (+agent-shell-arena-git-manifest-id manifest))
        :role (+agent-shell-pstack-panel-member-role member)
        :kind (+agent-shell-pstack-panel-member-kind member)
        :agent (symbol-name (+agent-shell-pstack-panel-member-agent member))
        :repository
        (when-let* ((toplevel
                     (or (when-let* ((manifest
                                      (+agent-shell-pstack-panel-arena panel)))
                           (+agent-shell-arena-git-manifest-toplevel manifest))
                         (when-let* ((cwd (+agent-shell-pstack-panel-cwd panel)))
                           (locate-dominating-file cwd ".git")))))
          (abbreviate-file-name (directory-file-name toplevel)))))

(defun +agent-shell-pstack-panel--egress-session (member)
  "Return sandboxed MEMBER's egress session, or nil."
  (when-let* ((sandbox (+agent-shell-pstack-panel-member-sandbox member)))
    (+agent-shell-sandbox-egress sandbox)))

(defun +agent-shell-pstack-panel--keep-egress (member)
  "Return a function keeping the record it gets as MEMBER's."
  (lambda (record)
    (setf (+agent-shell-pstack-panel-member-egress-record member) record)))

(defun +agent-shell-pstack-panel--close-egress (panel callback)
  "Call CALLBACK once every member of measured PANEL has its egress record.
A member that never launched gets a not-started record.  A sandboxed
member's record comes when its stop closes its session; past
`+agent-shell-pstack-panel--egress-wait' the session is closed here.  A
panel that measures nothing, or ran on a host, calls CALLBACK at once:
a host's records arrive with its results."
  (if (or (not (+agent-shell-pstack-panel-measure-egress panel))
          (+agent-shell-pstack-panel-remote panel))
      (funcall callback)
    (require '+agent-shell-egress)
    (let ((members (+agent-shell-pstack-panel--egress-members panel))
          (deadline (+ (float-time) +agent-shell-pstack-panel--egress-wait))
          (forced nil))
      (dolist (member members)
        (unless (or (+agent-shell-pstack-panel--egress-session member)
                    (+agent-shell-pstack-panel-member-egress-record member))
          (+agent-shell-egress-close
           (+agent-shell-egress-open
            nil (+agent-shell-pstack-panel--egress-file panel member)
            (+agent-shell-pstack-panel--egress-identity panel member))
           (+agent-shell-pstack-panel--keep-egress member))))
      (cl-labels
          ((check ()
             (if-let* ((waiting
                        (seq-remove
                         #'+agent-shell-pstack-panel-member-egress-record
                         members)))
                 (progn
                   (when (and (not forced) (> (float-time) deadline))
                     (setq forced t)
                     (dolist (member waiting)
                       (when-let* ((session
                                    (+agent-shell-pstack-panel--egress-session
                                     member)))
                         (+agent-shell-egress-close
                          session
                          (+agent-shell-pstack-panel--keep-egress member)))))
                   (run-at-time 0.1 nil #'check))
               (funcall callback))))
        (check)))))

(defun +agent-shell-pstack-panel--egress-phrase (member)
  "Return what MEMBER's egress record says, in a few words."
  (let* ((role (+agent-shell-pstack-panel-member-role member))
         (record (+agent-shell-pstack-panel-member-egress-record member))
         (reason (and record (+agent-shell-egress-record-reason record))))
    (pcase (and record (+agent-shell-egress-record-state record))
      ('nil (format "%s unavailable (artifact-missing)" role))
      ('not-started (format "%s not started" role))
      ('unavailable (format "%s unavailable (%s)" role reason))
      (_ (let ((count (length (+agent-shell-egress-record-destinations
                               record))))
           (format "%s %d destination%s%s" role count (if (= count 1) "" "s")
                   (if (+agent-shell-egress-record-written record) ""
                     " (record-write-failed)")))))))

(defun +agent-shell-pstack-panel--egress-text (panel)
  "Return the lines about measured PANEL's egress records, or nil.
They give each member's destination count or why it has none, where
the records are, and what the observed ones cannot show, never an
address or a name."
  (when (+agent-shell-pstack-panel-measure-egress panel)
    (let* ((members (+agent-shell-pstack-panel--egress-members panel))
           (records (delq nil (mapcar
                               #'+agent-shell-pstack-panel-member-egress-record
                               members)))
           (states (delq 'not-started
                         (mapcar #'+agent-shell-egress-record-state records)))
           (limits (seq-uniq
                    (mapcan (lambda (record)
                              (when (memq (+agent-shell-egress-record-state
                                           record)
                                          '(partial finished))
                                (copy-sequence
                                 (+agent-shell-egress-record-limits record))))
                            records)))
           (partial (seq-keep
                     (lambda (member)
                       (when-let* ((record
                                    (+agent-shell-pstack-panel-member-egress-record
                                     member))
                                   ((eq 'partial
                                        (+agent-shell-egress-record-state
                                         record)))
                                   (reason (+agent-shell-egress-record-reason
                                            record)))
                         (format "%s %s"
                                 (+agent-shell-pstack-panel-member-role member)
                                 reason)))
                     members)))
      (concat
       (format "\nEgress (%s): %s; records: %s.\n"
               (cond ((and states (seq-every-p (lambda (state)
                                                 (eq state 'unavailable))
                                               states))
                      "unavailable")
                     ((or (memq 'partial states) (memq 'unavailable states)
                          (length< records (length members)))
                      "partial")
                     (t "finished"))
               (mapconcat #'+agent-shell-pstack-panel--egress-phrase members
                          ", ")
               (expand-file-name "*.egress.json"
                                 (expand-file-name
                                  (+agent-shell-pstack-panel-id panel)
                                  +agent-shell-pstack-panel-directory)))
       (when (or limits partial)
         (format "Egress limits: %s.\n"
                 (string-join (append (mapcar #'symbol-name limits) partial)
                              ", ")))))))

;;;; Lifecycle

(defun +agent-shell-pstack-panel--arena-p (panel)
  "Return whether PANEL is an arena."
  (eq (+agent-shell-pstack-panel-shape panel) 'arena))

(defun +agent-shell-pstack-panel--delegate-p (panel)
  "Return whether PANEL is a delegate."
  (eq (+agent-shell-pstack-panel-shape panel) 'delegate))

(defun +agent-shell-pstack-panel--sandboxed-p (panel)
  "Return whether delegate PANEL's frozen policy confines it."
  (plist-get (+agent-shell-pstack-panel-delegate-options panel) :sandbox))

(defun +agent-shell-pstack-panel--isolation (panel)
  "Return delegate PANEL's isolation, once its manifest exists, or nil."
  (when-let* ((manifest (+agent-shell-pstack-panel-arena panel))
              (delegate (+agent-shell-arena-git-manifest-delegate manifest)))
    (+agent-shell-arena-git-delegate-isolation delegate)))

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

(defun +agent-shell-pstack-panel--disarm (panel)
  "Cancel PANEL's timer."
  (when-let* ((timer (+agent-shell-pstack-panel-timer panel)))
    (cancel-timer timer))
  (setf (+agent-shell-pstack-panel-timer panel) nil))

(defun +agent-shell-pstack-panel--arm (panel seconds)
  "Expire PANEL's current phase after SECONDS."
  (+agent-shell-pstack-panel--disarm panel)
  (setf (+agent-shell-pstack-panel-expires panel) (+ (float-time) seconds)
        (+agent-shell-pstack-panel-timer panel)
        (run-at-time seconds nil #'+agent-shell-pstack-panel--expire
                     (+agent-shell-pstack-panel-id panel)
                     (+agent-shell-pstack-panel-phase panel))))

(defun +agent-shell-pstack-panel--finish (panel)
  "End PANEL, then compose its follow-up and deliver it, once.
While its login admission runs, that is cancelled and the follow-up waits
for it to end; see `+agent-shell-pstack-panel--launch'."
  (unless (eq (+agent-shell-pstack-panel-phase panel) 'finished)
    (+agent-shell-pstack-panel--disarm panel)
    (setf (+agent-shell-pstack-panel-phase panel) 'finished)
    (if-let* ((cancel (+agent-shell-pstack-panel-login-check panel)))
        (funcall cancel)
      (+agent-shell-pstack-panel--finalize panel))))

(defun +agent-shell-pstack-panel--finalize (panel)
  "Compose finished PANEL's follow-up and deliver it.
A panel with a sink hands itself to the sink instead.  A measured arena
first waits for its members' egress records; see
`+agent-shell-pstack-panel--close-egress'."
  (+agent-shell-pstack-panel--close-egress
   panel
   (lambda ()
     (setf (+agent-shell-pstack-panel-followup panel)
           (+agent-shell-pstack-panel-followup-text panel))
     (if-let* ((sink (+agent-shell-pstack-panel-sink panel)))
         (funcall sink panel)
       (+agent-shell-pstack-panel--deliver-to
        (+agent-shell-pstack-panel-lead panel))))))

(defun +agent-shell-pstack-panel--kind-members (panel kind)
  "Return PANEL's members of KIND."
  (seq-filter (lambda (member)
                (eq kind (+agent-shell-pstack-panel-member-kind member)))
              (+agent-shell-pstack-panel-members panel)))

(defun +agent-shell-pstack-panel--phase-members (panel)
  "Return the members PANEL's current phase runs."
  (when-let* ((kind (alist-get (+agent-shell-pstack-panel-phase panel)
                               +agent-shell-pstack-panel--phase-kinds)))
    (+agent-shell-pstack-panel--kind-members panel kind)))

(defun +agent-shell-pstack-panel--advance (panel)
  "Move PANEL on from its phase once that phase is over.
Safe to call after any change: a phase that is not over stays."
  (let ((settled (lambda (members)
                   (seq-every-p #'+agent-shell-pstack-panel--settled-p
                                members))))
    (pcase (+agent-shell-pstack-panel-phase panel)
      ((or 'running 'interrupting 'preparing)
       (when (funcall settled (+agent-shell-pstack-panel-members panel))
         (+agent-shell-pstack-panel--finish panel)))
      ('candidates
       (when (funcall settled (+agent-shell-pstack-panel--kind-members
                               panel 'candidate))
         (+agent-shell-pstack-panel--disarm panel)
         (setf (+agent-shell-pstack-panel-phase panel) 'capturing)
         (+agent-shell-pstack-panel--advance panel)))
      ;; A running candidate can read the git store, so no result enters
      ;; it until every candidate has stopped.
      ('capturing
       (let* ((candidates (+agent-shell-pstack-panel--kind-members
                           panel 'candidate))
              (judge (car (+agent-shell-pstack-panel--kind-members
                           panel 'judge)))
              (captures (mapcar #'+agent-shell-pstack-panel-member-capture
                                candidates)))
         (cond
          ((seq-some (lambda (capture) (memq capture '(nil stopping)))
                     captures))
          ((memq 'stopped captures)
           (dolist (member candidates)
             (when (eq 'stopped (+agent-shell-pstack-panel-member-capture
                                 member))
               (+agent-shell-pstack-panel--collect panel member))))
          ((seq-every-p (lambda (capture) (memq capture '(captured failed)))
                        captures)
           (cond
            ((not (eq 'pending (+agent-shell-pstack-panel-member-state judge)))
             (+agent-shell-pstack-panel--finish panel))
            ((seq-some #'+agent-shell-pstack-panel--usable-p candidates)
             (+agent-shell-pstack-panel--judge panel judge))
            (t
             (setf (+agent-shell-pstack-panel-member-state judge) 'error
                   (+agent-shell-pstack-panel-member-error judge)
                   "No candidate changed the base, so there was nothing to \
judge.")
             (+agent-shell-pstack-panel--finish panel)))))))
      ('judging
       (when (funcall settled (+agent-shell-pstack-panel--kind-members
                               panel 'judge))
         (+agent-shell-pstack-panel--finish panel)))
      ('delegating
       (let ((delegate (car (+agent-shell-pstack-panel-members panel))))
         (pcase (+agent-shell-pstack-panel-member-capture delegate)
           ('stopped (+agent-shell-pstack-panel--collect-delegate
                      panel delegate))
           ((or 'captured 'failed)
            (+agent-shell-pstack-panel--finish panel))))))))

(defun +agent-shell-pstack-panel--usable-p (candidate)
  "Return whether CANDIDATE's captured result changed the base."
  (and (eq (+agent-shell-pstack-panel-member-capture candidate) 'captured)
       (plist-get (+agent-shell-pstack-panel-member-result candidate)
                  :changed)))

(defun +agent-shell-pstack-panel--judge (panel judge)
  "Start PANEL's JUDGE in its read-only sandbox, with its own timer."
  (setf (+agent-shell-pstack-panel-phase panel) 'judging)
  (+agent-shell-pstack-panel--arm
   panel (+agent-shell-pstack-panel-judge-timeout panel))
  (condition-case err
      (progn
        (setf (+agent-shell-pstack-panel-member-sandbox judge)
              (+agent-shell-pstack-arena-judge-sandbox
               (+agent-shell-pstack-panel-arena panel)
               (+agent-shell-pstack-panel--horizon panel)))
        (+agent-shell-pstack-panel--start-member panel judge))
    (error (+agent-shell-pstack-panel--settle
            panel judge 'error nil (error-message-string err))))
  (+agent-shell-pstack-panel--advance panel)
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--report-file (panel member &optional suffix)
  "Return MEMBER of PANEL's report file, or its sibling ending in SUFFIX."
  (expand-file-name
   (concat (+agent-shell-pstack-panel-member-role member) (or suffix ".md"))
   (expand-file-name (+agent-shell-pstack-panel-id panel)
                     +agent-shell-pstack-panel-directory)))

(defun +agent-shell-pstack-panel--write-atomically (file text)
  "Write TEXT to FILE through a temporary file and a rename."
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-temp-file (expand-file-name
                                    (concat "." (file-name-nondirectory file)
                                            "-")
                                    (file-name-directory file))))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil temporary nil 'silent)
    (rename-file temporary file t)))

(defun +agent-shell-pstack-panel--write (panel member &optional stop-reason)
  "Write MEMBER's text as its report under PANEL's directory.
STOP-REASON, given only for a turn that completed successfully, also
writes the successful-final marker ROLE.final.json after the report: its
hash and that stop reason, never the text itself.  A report without a
matching marker cannot prove its turn completed; see
`+agent-shell-pstack-panel--final-text'."
  (let ((text (+agent-shell-pstack-panel-member-text member))
        (marker (+agent-shell-pstack-panel--report-file panel member
                                                        ".final.json")))
    (ignore-errors (delete-file marker))
    (unless (or (null text) (string-blank-p text))
      (let ((file (+agent-shell-pstack-panel--report-file panel member)))
        (+agent-shell-pstack-panel--write-atomically file text)
        (setf (+agent-shell-pstack-panel-member-report member) file)
        (when stop-reason
          (+agent-shell-pstack-panel--write-atomically
           marker
           (concat (json-serialize
                    `((version . 1)
                      (sha256 . ,(secure-hash 'sha256
                                              (encode-coding-string
                                               text 'utf-8-unix)))
                      (stop_reason . ,stop-reason)
                      (successful . t)))
                   "\n")))))))

(defun +agent-shell-pstack-panel--final-text (panel member)
  "Return MEMBER of PANEL's saved final text when its marker proves it.
That is the report, when ROLE.final.json says a successful terminal turn
produced exactly it; otherwise nil."
  (let ((file (+agent-shell-pstack-panel--report-file panel member))
        (marker (+agent-shell-pstack-panel--report-file panel member
                                                        ".final.json")))
    (ignore-errors
      (let ((text (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents file))
                    (buffer-string)))
            (json (with-temp-buffer
                    (insert-file-contents marker)
                    (json-parse-buffer :object-type 'alist
                                       :false-object nil))))
        (when (and (eq t (alist-get 'successful json))
                   (equal (alist-get 'sha256 json)
                          (secure-hash 'sha256 (encode-coding-string
                                                text 'utf-8-unix))))
          text)))))

(defun +agent-shell-pstack-panel--settle (panel member state text
                                                &optional error stop-reason)
  "Settle MEMBER of PANEL as STATE with TEXT and ERROR, once.
STOP-REASON comes with a turn that completed successfully."
  (unless (+agent-shell-pstack-panel--settled-p member)
    (when (timerp (+agent-shell-pstack-panel-member-retry member))
      (cancel-timer (+agent-shell-pstack-panel-member-retry member))
      (setf (+agent-shell-pstack-panel-member-retry member) t))
    (setf (+agent-shell-pstack-panel-member-state member) state
          (+agent-shell-pstack-panel-member-text member) text
          (+agent-shell-pstack-panel-member-error member) error)
    (+agent-shell-pstack-panel--write panel member stop-reason)
    ;; Killing a shell from its own event callback corrupts upstream state.
    (cond ((eq (+agent-shell-pstack-panel-member-kind member) 'candidate)
           (run-at-time 0 nil #'+agent-shell-pstack-panel--halt panel member))
          ((eq (+agent-shell-pstack-panel-member-kind member) 'delegate)
           (run-at-time 0 nil #'+agent-shell-pstack-panel--halt-delegate
                        panel member))
          ((+agent-shell-pstack-panel-member-sandbox member)
           (run-at-time 0 nil #'+agent-shell-pstack-panel--stop member
                        #'ignore))
          ((when-let* ((shell (+agent-shell-pstack-panel-member-shell member)))
             (run-at-time 0 nil #'+agent-shell-drive-kill shell))))
    (+agent-shell-pstack-panel--advance panel)))

(defun +agent-shell-pstack-panel--stop (member callback)
  "Stop settled sandboxed MEMBER and every process it started.
Then close its egress session, if any, and call CALLBACK while its
sandbox's private folders are deleted, a failure to delete them reported
with `message'."
  (+agent-shell-pstack-panel--stop-noting member #'ignore callback))

(defun +agent-shell-pstack-panel--stop-noting (member note callback)
  "Stop sandboxed MEMBER as `+agent-shell-pstack-panel--stop' does.
NOTE is called with the pids that outlived the stop before CALLBACK."
  (if-let* ((sandbox (+agent-shell-pstack-panel-member-sandbox member)))
      (let* ((shell (+agent-shell-pstack-panel-member-shell member))
             (role (+agent-shell-pstack-panel-member-role member))
             (discard (lambda ()
                        (+agent-shell-sandbox-discard
                         sandbox
                         (lambda (error)
                           (when error
                             (message "pstack arena: %s's private folders \
stayed: %s" role error))))
                        (funcall callback)))
             (done (lambda (survivors)
                     (funcall note survivors)
                     (when survivors
                       (message "pstack arena: processes %s outlived %s"
                                survivors role))
                     (if-let* ((session (+agent-shell-sandbox-egress sandbox)))
                         (+agent-shell-egress-close
                          session
                          (lambda (record)
                            (funcall (+agent-shell-pstack-panel--keep-egress
                                      member)
                                     record)
                            (funcall discard)))
                       (funcall discard)))))
        (if (buffer-live-p shell)
            (+agent-shell-drive-stop shell nil done)
          ;; Its adapter is gone, so only the sweep can find what it left.
          (+agent-shell-sandbox-sweep
           sandbox (lambda (survivors)
                     (funcall done survivors)))))
    (funcall callback)))

(defun +agent-shell-pstack-panel--halt (panel member)
  "Stop settled candidate MEMBER of PANEL so its tree stops changing.
A candidate without a worktree fails, since there is nothing to capture,
and so does one whose stop signals, which would otherwise stay stopping
and hold every capture back."
  (setf (+agent-shell-pstack-panel-member-capture member) 'stopping)
  (+agent-shell-pstack-panels--refresh)
  (cl-flet ((end (capture &optional error)
              (when (eq (+agent-shell-pstack-panel-member-capture member)
                        'stopping)
                (setf (+agent-shell-pstack-panel-member-capture member) capture)
                (when error
                  (setf (+agent-shell-pstack-panel-member-result member)
                        (list :error error)))
                (+agent-shell-pstack-panel--advance panel)
                (+agent-shell-pstack-panels--refresh))))
    (condition-case err
        (+agent-shell-pstack-panel--stop
         member
         (lambda ()
           (if-let* ((manifest (+agent-shell-pstack-panel-arena panel))
                     ((+agent-shell-pstack-panel-member-sandbox member))
                     ((+agent-shell-arena-git-role
                       manifest (+agent-shell-pstack-panel-member-role
                                 member))))
               (end 'stopped)
             (end 'failed "The candidate never started."))))
      (error (end 'failed (format "Stopping the candidate failed: %s"
                                  (error-message-string err)))))))

(defun +agent-shell-pstack-panel--collect (panel member)
  "Capture what stopped candidate MEMBER of PANEL produced.
A usable result also gets a checkout of its own for the judge to read."
  (let* ((name (+agent-shell-pstack-panel-member-role member))
         (manifest (+agent-shell-pstack-panel-arena panel))
         (role (+agent-shell-arena-git-role manifest name)))
    (cl-labels
        ((end (capture result)
           (setf (+agent-shell-pstack-panel-member-capture member) capture
                 (+agent-shell-pstack-panel-member-result member) result)
           (+agent-shell-pstack-panel--advance panel)
           (+agent-shell-pstack-panels--refresh))
         (failed (error) (end 'failed (list :error error)))
         ;; A step that signals would leave the candidate capturing forever.
         (step (function)
           (lambda (&rest arguments)
             (condition-case err (apply function arguments)
               (error (failed (error-message-string err))))))
         (compare (commit)
           (+agent-shell-arena-git-diff
            manifest name
            (step (lambda (diff error)
                    (let ((result (append
                                   (list :commit commit
                                         :ref (+agent-shell-arena-git-role-ref
                                               role))
                                   diff)))
                      (cond (error (failed error))
                            ((plist-get diff :changed) (check-out result))
                            (t (end 'captured result))))))))
         (check-out (result)
           (+agent-shell-arena-git-checkout
            manifest name
            (step (lambda (checkout error)
                    (if error (failed error)
                      (end 'captured
                           (append result (list :checkout checkout)))))))))
      (setf (+agent-shell-pstack-panel-member-capture member) 'capturing
            (+agent-shell-arena-git-role-state role)
            (symbol-name (+agent-shell-pstack-panel-member-state member)))
      (+agent-shell-pstack-panels--refresh)
      (condition-case err
          (+agent-shell-arena-git-capture
           manifest name
           (step (lambda (commit error)
                   (if (not error)
                       (compare commit)
                     (+agent-shell-arena-git-write-manifest manifest)
                     (failed error)))))
        (error (failed (error-message-string err)))))))

(defun +agent-shell-pstack-panel--delegate-directory (panel)
  "Return where PANEL's delegate works: its worktree, or the lead's directory."
  (let ((manifest (+agent-shell-pstack-panel-arena panel)))
    (if-let* ((role (car (+agent-shell-arena-git-manifest-roles manifest))))
        (+agent-shell-arena-git-directory manifest role)
      (+agent-shell-pstack-panel-cwd panel))))

(defun +agent-shell-pstack-panel--halt-delegate (panel member)
  "Stop settled delegate MEMBER of PANEL and every process it started.
A sandboxed one stops as a candidate does, and its sandbox goes.  Under
a process lease the stop ends the lease's scope; see
`+agent-shell-drive-stop-lease'."
  (setf (+agent-shell-pstack-panel-member-capture member) 'stopping)
  (+agent-shell-pstack-panels--refresh)
  (let* ((stopped nil)
         (done (lambda (survivors)
                 (unless stopped
                   (setq stopped t)
                   (setf (+agent-shell-pstack-panel-member-capture member)
                         'stopped
                         (+agent-shell-pstack-panel-member-result member)
                         (when survivors (list :survivors survivors)))
                   (+agent-shell-pstack-panel--advance panel)
                   (+agent-shell-pstack-panels--refresh))))
         (shell (+agent-shell-pstack-panel-member-shell member))
         (lease (+agent-shell-drive-lease shell)))
    (condition-case err
        (cond
         ((+agent-shell-pstack-panel-member-sandbox member)
          (if lease
              (+agent-shell-drive-stop-lease
               lease shell (lambda (survivors)
                             (+agent-shell-pstack-panel--stop
                              member (lambda () (funcall done survivors)))))
            (let ((survivors nil))
              (+agent-shell-pstack-panel--stop-noting
               member (lambda (pids) (setq survivors pids))
               (lambda () (funcall done survivors))))))
         (lease (+agent-shell-drive-stop-lease lease shell done))
         (t (+agent-shell-drive-stop-unconfined shell nil done)))
      (error (funcall done (list (format "the stop failed: %s"
                                         (error-message-string err))))))))

(defun +agent-shell-pstack-panel--finalize-delegate (panel member done)
  "Make sandboxed delegate MEMBER of PANEL's one commit, then call DONE.
Only a saved final response its successful-final marker proves may
propose the message, and only once every writer has stopped; otherwise
the work stays uncommitted and finalization is blocked, saying why.  See
`+agent-shell-arena-git-finalize-delegate'."
  (let* ((manifest (+agent-shell-pstack-panel-arena panel))
         (survivors (plist-get (+agent-shell-pstack-panel-member-result member)
                               :survivors))
         (text (+agent-shell-pstack-panel--final-text panel member))
         (block (lambda (reason)
                  (+agent-shell-arena-git-block-delegate manifest reason)
                  (funcall done))))
    (cond
     (survivors
      (funcall block (format "Processes %s outlived the delegate, so its files might still change." (mapconcat #'prin1-to-string survivors ", "))))
     ((null text)
      (funcall block "The delegate's turn did not end with a successful final response, so it proposed no commit message."))
     (t
      (condition-case err
          (+agent-shell-arena-git-finalize-delegate
           manifest (+agent-shell-pstack-panel-identity panel)
           (+agent-shell-pstack-panel--delegate-commit-message text)
           (lambda (_delegate error)
             (if error (funcall block error) (funcall done))))
        (user-error (funcall block (error-message-string err))))))))

(defun +agent-shell-pstack-panel--collect-delegate (panel member)
  "Record what stopped delegate MEMBER of PANEL left, then advance PANEL.
A sandboxed delegate first gets its one commit; see
`+agent-shell-pstack-panel--finalize-delegate'.  A process that outlived
the stop keeps the lead's checkout on the delegate's branch; see
`+agent-shell-arena-git-finish-delegate'.  A branch without commits of
its own goes; see `+agent-shell-arena-git-drop-branch'."
  (if (and (+agent-shell-pstack-panel--sandboxed-p panel)
           (not (eq (+agent-shell-pstack-panel-member-capture member)
                    'capturing)))
      (progn
        (setf (+agent-shell-pstack-panel-member-capture member) 'capturing)
        (+agent-shell-pstack-panels--refresh)
        (condition-case err
            (+agent-shell-pstack-panel--finalize-delegate
             panel member
             (lambda () (+agent-shell-pstack-panel--finish-collecting
                         panel member)))
          (error
           (+agent-shell-arena-git-block-delegate
            (+agent-shell-pstack-panel-arena panel) (error-message-string err))
           (+agent-shell-pstack-panel--finish-collecting panel member))))
    (+agent-shell-pstack-panel--finish-collecting panel member)))

(defun +agent-shell-pstack-panel--finish-collecting (panel member)
  "Record what delegate MEMBER of PANEL left once it is finalized."
  (let ((survivors (plist-get (+agent-shell-pstack-panel-member-result member)
                              :survivors)))
    (cl-flet ((end (capture &optional error)
                (setf (+agent-shell-pstack-panel-member-capture member) capture)
                (when error
                  (setf (+agent-shell-pstack-panel-member-result member)
                        (append (+agent-shell-pstack-panel-member-result member)
                                (list :error error))))
                (+agent-shell-pstack-panel--advance panel)
                (+agent-shell-pstack-panels--refresh)))
      (setf (+agent-shell-pstack-panel-member-capture member) 'capturing)
      (+agent-shell-pstack-panels--refresh)
      (condition-case err
          (+agent-shell-arena-git-finish-delegate
           (+agent-shell-pstack-panel-arena panel)
           (when survivors
             (format "Processes %s outlived the delegate."
                     (mapconcat #'prin1-to-string survivors ", ")))
           (lambda (delegate error)
             (cond
              (error (end 'failed error))
              ((+agent-shell-arena-git-delegate-commits delegate)
               (end 'captured))
              (t (+agent-shell-arena-git-drop-branch
                  (+agent-shell-pstack-panel-arena panel)
                  (lambda (branch error)
                    (when branch
                      (setf (+agent-shell-pstack-panel-member-result member)
                            (append (+agent-shell-pstack-panel-member-result
                                     member)
                                    (list :dropped branch))))
                    (end 'captured error)))))))
        (error (end 'failed (error-message-string err)))))))

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
  "Advance member ROLE of panel ID through EVENT.
A runner's panel shows its observer the event first."
  (when-let* ((panel (+agent-shell-pstack-panel--find id))
              (member (+agent-shell-pstack-panel--member panel role)))
    (when-let* ((observer (plist-get (+agent-shell-pstack-panel-runner panel)
                                     :observer)))
      (with-demoted-errors "pstack panel: observer failed: %S"
        (funcall observer member event)))
    (pcase (plist-get event :kind)
      ('init-finished
       (when (eq (+agent-shell-pstack-panel-member-state member) 'starting)
         (setf (+agent-shell-pstack-panel-member-state member) 'waiting))
       (+agent-shell-pstack-panel--send-brief panel member))
      ('prompt-ready (+agent-shell-pstack-panel--send-brief panel member))
      ('turn-complete
       (when (+agent-shell-pstack-panel--turn-open-p member)
         (let* ((text (+agent-shell-pstack-panel--report-text event))
                (reason (plist-get event :stop-reason))
                (failure (+agent-shell-pstack-panel--failure member event)))
           (cond
            (failure (+agent-shell-pstack-panel--fail panel member failure
                                                      text))
            ((member reason +agent-shell-pstack-panel--failed-stops)
             (+agent-shell-pstack-panel--settle
              panel member 'error text
              (format "The turn ended with stop reason %s." reason)))
            (t (+agent-shell-pstack-panel--settle panel member 'done text nil
                                                  (or reason "end_turn")))))))
      ('error
       (when (+agent-shell-pstack-panel--turn-open-p member)
         (let* ((text (+agent-shell-drive-partial-text
                       (+agent-shell-pstack-panel-member-shell member)))
                (failure (+agent-shell-pstack-panel--failure
                          member (append (list :text text) event))))
           (if failure
               (+agent-shell-pstack-panel--fail panel member failure text)
             (+agent-shell-pstack-panel--settle
              panel member 'error text
              (format "The agent reported an error: %s"
                      (or (plist-get event :message) "no message")))))))
      ('clean-up
       (+agent-shell-pstack-panel--settle
        panel member 'error nil
        (or (plist-get event :reason)
            "The member's shell ended before it reported."))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--turn-open-p (member)
  "Return whether MEMBER runs a turn whose end Emacs awaits."
  (and (eq (+agent-shell-pstack-panel-member-state member) 'running)
       (not (timerp (+agent-shell-pstack-panel-member-retry member)))))

(defun +agent-shell-pstack-panel--agent-name (member)
  "Return the name of MEMBER's agent."
  (let ((agent (+agent-shell-pstack-panel-member-agent member)))
    (or (plist-get (alist-get agent +agent-shell-sandbox-agents) :name)
        (format "%s" agent))))

(defun +agent-shell-pstack-panel--failure (member event)
  "Return the provider failure that ended MEMBER's turn in EVENT, or nil.
See `+agent-shell-drive-failure'; the text patterns are its agent's."
  (+agent-shell-drive-failure
   event (plist-get (alist-get (+agent-shell-pstack-panel-member-agent member)
                               +agent-shell-sandbox-agents)
                    :failure-patterns)))

(defun +agent-shell-pstack-panel--failure-text (member failure)
  "Return the sentence saying FAILURE ended MEMBER's turn."
  (let ((message (string-trim (or (plist-get failure :message) ""))))
    (format "%s %s%s" (+agent-shell-pstack-panel--agent-name member)
            (+agent-shell-drive-failure-phrase (plist-get failure :kind))
            (if (string-empty-p message) "." (concat ": " message)))))

(defun +agent-shell-pstack-panel--retry-time (panel)
  "Return when a retry PANEL's members wait for now would start, or nil.
That is `+agent-shell-pstack-panel-retry-delay' from now, unless it
would not come before PANEL's current phase expires and its deadline."
  (let ((start (+ (float-time) +agent-shell-pstack-panel-retry-delay)))
    (unless (seq-some (lambda (limit) (and limit (>= start limit)))
                      (list (+agent-shell-pstack-panel-expires panel)
                            (+agent-shell-pstack-panel-deadline panel)))
      start)))

(defun +agent-shell-pstack-panel--fail (panel member failure text)
  "Handle provider FAILURE ending running MEMBER of PANEL's turn with TEXT.
A transient kind is retried once: after `+agent-shell-pstack-panel-retry-delay'
the same shell is asked to continue.  Otherwise, or when no retry fits
before PANEL's phase or deadline ends, MEMBER settles as `error' with
TEXT, saying what failed."
  (setf (+agent-shell-pstack-panel-member-failure member) failure)
  (let* ((kind (plist-get failure :kind))
         (retried (+agent-shell-pstack-panel-member-retry member))
         (transient (+agent-shell-drive-failure-transient-p kind))
         (start (and transient (not retried)
                     (+agent-shell-pstack-panel--retry-time panel)))
         (id (+agent-shell-pstack-panel-id panel))
         (role (+agent-shell-pstack-panel-member-role member)))
    (if start
        (setf (+agent-shell-pstack-panel-member-retry member)
              (run-at-time (- start (float-time)) nil
                           #'+agent-shell-pstack-panel--retry id role))
      (+agent-shell-pstack-panel--settle
       panel member 'error text
       (concat (+agent-shell-pstack-panel--failure-text member failure)
               (cond (retried "  Emacs had already asked it once to \
continue, and it failed again.")
                     (transient "  Emacs did not retry, since the panel \
would end before a retry could start.")))))))

(defun +agent-shell-pstack-panel--retry (id role)
  "Ask member ROLE of panel ID, which a provider failure stopped, to continue."
  (when-let* ((panel (+agent-shell-pstack-panel--find id))
              (member (+agent-shell-pstack-panel--member panel role))
              ((timerp (+agent-shell-pstack-panel-member-retry member))))
    (setf (+agent-shell-pstack-panel-member-retry member) t)
    (let ((failure (+agent-shell-pstack-panel-member-failure member)))
      (unless (+agent-shell-drive-send
               (+agent-shell-pstack-panel-member-shell member)
               (format "Your last turn ended early because of a provider \
failure: %s\nContinue where you left off, and finish the task your brief \
gave you." (+agent-shell-pstack-panel--failure-text member failure)))
        (+agent-shell-pstack-panel--settle
         panel member 'error
         (+agent-shell-drive-partial-text
          (+agent-shell-pstack-panel-member-shell member))
         (concat (+agent-shell-pstack-panel--failure-text member failure)
                 "  Emacs could not send its retry, since the shell was \
not ready."))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--phase-timeout (panel)
  "Return the seconds PANEL's current phase may run."
  (if (eq (+agent-shell-pstack-panel-phase panel) 'judging)
      (+agent-shell-pstack-panel-judge-timeout panel)
    (+agent-shell-pstack-panel-timeout panel)))

(defun +agent-shell-pstack-panel--expire (id phase)
  "Settle the unfinished members of panel ID's PHASE as timed out.
Do nothing once the panel has left PHASE."
  (when-let* ((panel (+agent-shell-pstack-panel--find id))
              ((eq phase (+agent-shell-pstack-panel-phase panel))))
    (setf (+agent-shell-pstack-panel-timer panel) nil)
    (dolist (member (+agent-shell-pstack-panel--phase-members panel))
      (+agent-shell-pstack-panel--settle
       panel member 'timeout
       (when-let* ((shell (+agent-shell-pstack-panel-member-shell member)))
         (+agent-shell-drive-partial-text shell))
       (format "No report within the %d-second timeout."
               (+agent-shell-pstack-panel--phase-timeout panel))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--start-member (panel member)
  "Start MEMBER of PANEL: sandboxed, full-access for a delegate, or read-only."
  (let* ((id (+agent-shell-pstack-panel-id panel))
         (role (+agent-shell-pstack-panel-member-role member))
         (sandbox (+agent-shell-pstack-panel-member-sandbox member))
         (kind (+agent-shell-pstack-panel-member-kind member))
         (delegate (eq kind 'delegate)))
    (when (and sandbox (+agent-shell-pstack-panel-measure-egress panel))
      (with-demoted-errors "pstack arena: egress measurement failed: %S"
        (require '+agent-shell-egress)
        (+agent-shell-egress-open
         sandbox (+agent-shell-pstack-panel--egress-file panel member)
         (+agent-shell-pstack-panel--egress-identity panel member))))
    (condition-case err
        (let ((shell (apply
                      #'+agent-shell-drive-start
                      :config (+agent-shell-pstack-panel-member-agent member)
                      :directory
                      (cond (delegate
                             (+agent-shell-pstack-panel--delegate-directory
                              panel))
                            ((not sandbox) (+agent-shell-pstack-panel-cwd panel))
                            ((+agent-shell-sandbox-read-only sandbox)
                             (+agent-shell-sandbox-root sandbox))
                            (t (let ((manifest (+agent-shell-pstack-panel-arena
                                                panel)))
                                 (+agent-shell-arena-git-directory
                                  manifest (+agent-shell-arena-git-role
                                            manifest role)))))
                      :policy (cond (sandbox sandbox)
                                    (delegate 'full-access)
                                    (t 'read-only))
                      (append
                       (when-let* (((and delegate (not sandbox)))
                                   (environment
                                    (+agent-shell-pstack-panel--identity-environment
                                     panel)))
                         (list :environment environment))
                       (when-let* (((eq 'delegate kind))
                                   (lease (plist-get
                                           (+agent-shell-pstack-panel-runner
                                            panel)
                                           :lease)))
                         (list :lease (funcall lease member)))))))
          (setf (+agent-shell-pstack-panel-member-state member) 'starting
                (+agent-shell-pstack-panel-member-shell member) shell)
          (+agent-shell-drive-watch
           shell 'pstack-panel (cons id role)
           (lambda (event)
             (+agent-shell-pstack-panel--member-event id role event)))
          (when-let* ((observer (plist-get (+agent-shell-pstack-panel-runner
                                            panel)
                                           :observer)))
            ;; Lifecycle events reach the observer through the member's
            ;; own handler, first.
            (+agent-shell-drive-watch-progress
             shell 'pstack-panel (cons id role)
             (lambda (event)
               (when (eq (plist-get event :kind) 'progress)
                 (funcall observer member event))))))
      (error (+agent-shell-pstack-panel--settle
              panel member 'error nil (error-message-string err))))))

(defun +agent-shell-pstack-panel--identity-environment (panel)
  "Return the git identity entries PANEL's unsandboxed delegate commits with.
They name the laptop user's identity as author and committer, or are nil
for a local delegate, which commits as its repository says."
  (when-let* ((identity (+agent-shell-pstack-panel-identity panel)))
    (let ((name (+agent-shell-arena-git-user-name identity))
          (email (+agent-shell-arena-git-user-email identity)))
      (list (concat "GIT_AUTHOR_NAME=" name) (concat "GIT_AUTHOR_EMAIL=" email)
            (concat "GIT_COMMITTER_NAME=" name)
            (concat "GIT_COMMITTER_EMAIL=" email)))))

(defun +agent-shell-pstack-panel--lead-agent (lead)
  "Return the agent identifier of LEAD's shell, or nil."
  (when (local-variable-p 'agent-shell--state lead)
    (map-nested-elt (buffer-local-value 'agent-shell--state lead)
                    '(:agent-config :identifier))))

(defun +agent-shell-pstack-panel--request (lead session text &optional tool)
  "Handle LEAD's request in TEXT from SESSION, logging every outcome.
TOOL is the call to interrupt.  Catch, log and message request errors."
  (let* ((own-entry (null +agent-shell-pstack-panel--log-entry))
         (+agent-shell-pstack-panel--log-entry
          (or +agent-shell-pstack-panel--log-entry
              (+agent-shell-pstack-panel--log-event
               lead session (if tool 'tool-call-update 'turn-complete) text))))
    (condition-case err
        (progn
          (when own-entry (+agent-shell-pstack-panel--log-request-text text text))
          (+agent-shell-pstack-panel--start-request lead session text tool))
      (error (+agent-shell-pstack-panel--log-error err) nil))))

(defun +agent-shell-pstack-panel--start-request (lead session text &optional tool)
  "Start the panel that LEAD requested in TEXT in SESSION.
TOOL names the call LEAD made after TEXT; a new request interrupts LEAD.
Return the panel, or nil when TEXT requests none or one already seen."
  (when-let* ((block (+agent-shell-pstack-panel--block text))
              (short-id (+agent-shell-pstack-panel--id (or session "") block))
              (digest (secure-hash 'sha256 (concat session "\0" block)))
              (id (let ((previous
                         (gethash short-id
                                  +agent-shell-pstack-panel--request-digests)))
                    (if (and previous (not (equal digest previous)))
                        digest short-id))))
    (if (+agent-shell-pstack-panel--find id)
        (progn (+agent-shell-pstack-panel--log-outcome 'duplicate id) nil)
      (+agent-shell-pstack-panel--log-outcome 'started id)
      (let ((panel (condition-case err
                       (+agent-shell-pstack-panel--parse
                        block (+agent-shell-pstack-panel--lead-agent lead))
                     (user-error
                      (+agent-shell-pstack-panel--make
                       :rejection (error-message-string err))))))
        (setf (+agent-shell-pstack-panel-id panel) id
              (+agent-shell-pstack-panel-lead panel) lead)
        (puthash id panel +agent-shell-pstack-panel--panels)
        (puthash id digest +agent-shell-pstack-panel--request-digests)
        (when tool
          (setf (+agent-shell-pstack-panel-interrupted panel) tool)
          (+agent-shell-drive-interrupt lead)
          (+agent-shell-pstack-panel--log-outcome 'interrupted id tool))
        (cond
         ((+agent-shell-pstack-panel-rejection panel)
          (+agent-shell-pstack-panel--log-outcome
           'rejected id (+agent-shell-pstack-panel-rejection panel))
          (+agent-shell-pstack-panel--finish panel))
         ((memq (+agent-shell-pstack-panel-shape panel) '(arena delegate))
          (setf (+agent-shell-pstack-panel-cwd panel)
                (with-current-buffer lead (agent-shell-cwd)))
          ;; The base must include the edit the interrupt may cut short, and
          ;; a delegate's checkout must be clean of it.
          (if tool
              (setf (+agent-shell-pstack-panel-phase panel) 'interrupting)
            (+agent-shell-pstack-panel--prepare panel)))
         (t
          (setf (+agent-shell-pstack-panel-cwd panel)
                (with-current-buffer lead (agent-shell-cwd)))
          (+agent-shell-pstack-panel--arm
           panel (+agent-shell-pstack-panel--phase-timeout panel))
          (dolist (member (+agent-shell-pstack-panel-members panel))
            (+agent-shell-pstack-panel--start-member panel member))
          (+agent-shell-pstack-panel--advance panel)))
        (+agent-shell-pstack-panels--refresh)
        panel))))

;;;; Arenas

(defun +agent-shell-pstack-panel--discard-arena (panel &optional done)
  "Remove PANEL's arena from disk; no candidate in it ever ran.
A delegate's branch goes too while it is empty; see
`+agent-shell-arena-git-drop-branch'.  Then call DONE, when non-nil,
with the branch removed or nil."
  (if-let* ((manifest (+agent-shell-pstack-panel-arena panel)))
      (let ((id (+agent-shell-arena-git-manifest-id manifest)))
        (setf (+agent-shell-pstack-panel-arena panel) nil)
        (+agent-shell-arena-git-clean
         manifest
         (lambda (left error)
           (when (or left error)
             (message "pstack arena %s: cleanup left %s"
                      id (or error (string-join left "; "))))
           (if (+agent-shell-arena-git-manifest-delegate manifest)
               (+agent-shell-arena-git-drop-branch
                manifest (lambda (branch error)
                           (when error
                             (message "pstack arena %s: %s" id error))
                           (when done (funcall done branch))))
             (when done (funcall done nil))))))
    (when done (funcall done nil))))

(defun +agent-shell-pstack-panel--fail-preparation (panel reason)
  "End PANEL, whose arena could not be prepared, because of REASON.
A delegate's rejection waits for its arena to go, so it can say whether
its empty branch went too."
  (cl-flet ((end (branch)
              (setf (+agent-shell-pstack-panel-rejection panel)
                    (if branch
                        (format "%s  Emacs removed the delegate's empty \
branch %s." reason branch)
                      reason))
              (dolist (member (+agent-shell-pstack-panel-members panel))
                (+agent-shell-pstack-panel--settle
                 panel member 'error nil reason))))
    (if (+agent-shell-pstack-panel--delegate-p panel)
        (+agent-shell-pstack-panel--discard-arena
         panel (lambda (branch)
                 (end branch)
                 (+agent-shell-pstack-panel--finish panel)
                 (+agent-shell-pstack-panels--refresh)))
      (end nil)
      (+agent-shell-pstack-panel--discard-arena panel)
      (+agent-shell-pstack-panel--finish panel)
      (+agent-shell-pstack-panels--refresh))))

(defun +agent-shell-pstack-panel--preparing (panel error next)
  "Continue preparing PANEL by calling NEXT, unless ERROR or it has ended.
A preparation the lead abandoned discards its arena."
  (cond ((not (eq (+agent-shell-pstack-panel-phase panel) 'preparing))
         (+agent-shell-pstack-panel--discard-arena panel))
        (error (+agent-shell-pstack-panel--fail-preparation panel error))
        (t (condition-case err (funcall next)
             (error (+agent-shell-pstack-panel--fail-preparation
                     panel (error-message-string err)))))))

(defun +agent-shell-pstack-panel--prepare (panel)
  "Prepare arena or delegate PANEL, whose lead's turn has ended."
  (if (+agent-shell-pstack-panel--delegate-p panel)
      (+agent-shell-pstack-panel--prepare-delegate panel)
    (+agent-shell-pstack-panel--prepare-arena panel)))

(defun +agent-shell-pstack-panel--prepare-arena (panel)
  "Snapshot the lead's tree as PANEL's base and give each candidate a worktree.
Then start the candidates.  When the repository's profile names a host,
the arena runs there instead; see `+agent-shell-arena-remote-launch'."
  (setf (+agent-shell-pstack-panel-phase panel) 'preparing)
  (+agent-shell-arena-git-capture-base
   (+agent-shell-pstack-panel-cwd panel)
   (lambda (base error)
     (+agent-shell-pstack-panel--preparing
      panel error
      (lambda ()
        (let ((profile (+agent-shell-arena-git-profile
                        (+agent-shell-arena-git-base-toplevel base))))
          (if (plist-get profile :host)
              (progn (require '+agent-shell-arena-remote)
                     (+agent-shell-arena-remote-launch panel base profile))
            (+agent-shell-pstack-panel--provision panel base)))))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--provision (panel base)
  "Create PANEL's arena over BASE and give each candidate a worktree.
Then start its members.  PANEL is preparing.  Its deadline is fixed now,
unless a host's runner fixed it already."
  (unless (+agent-shell-pstack-panel-deadline panel)
    (setf (+agent-shell-pstack-panel-deadline panel)
          (+ (float-time) (+agent-shell-pstack-panel-arena-budget
                           (+agent-shell-pstack-panel-timeout panel)
                           (+agent-shell-pstack-panel-judge-timeout panel)))))
  (let ((manifest (+agent-shell-arena-git-create
                   base (+agent-shell-pstack-panel-id panel))))
    (setf (+agent-shell-pstack-panel-arena panel) manifest)
    (cl-labels
        ((provision (members)
           (if (null members)
               (+agent-shell-pstack-panel--launch panel)
             (+agent-shell-arena-git-provision
              manifest
              (+agent-shell-pstack-panel-member-role (car members))
              (lambda (_directory error)
                (+agent-shell-pstack-panel--preparing
                 panel error
                 (lambda () (provision (cdr members)))))))))
      (provision (+agent-shell-pstack-panel--unsettled panel 'candidate)))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--unsettled (panel kind)
  "Return PANEL's members of KIND that have not settled."
  (seq-remove #'+agent-shell-pstack-panel--settled-p
              (+agent-shell-pstack-panel--kind-members panel kind)))

(defun +agent-shell-pstack-arena-sandbox (manifest name deadline
                                                   &optional trace)
  "Return a sandbox confining candidate NAME of MANIFEST to its worktree.
DEADLINE is its arena's fixed absolute stop time plus capture grace.
It is explicit so concurrent local panels cannot share dynamic state.
Only a diagnostic command that starts no agent may pass nil.
With TRACE non-nil, its profile reports what it would refuse instead.
It cannot read the lead's checkout, other than the shared dependencies
and the git directory, the panel reports, the agents' own config, or any
other candidate, nor what every sandbox refuses; see
`+agent-shell-sandbox-create'.  It may stat the lead's top level, which
git resolves.
The repository's arena profile adds allowances and environment entries,
and its :files are written into the worktree for the new sandbox."
  (let* ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (profile (+agent-shell-arena-git-profile toplevel))
         (role (+agent-shell-arena-git-role manifest name))
         (worktree (+agent-shell-arena-git-role-worktree role))
         (sandbox
          (+agent-shell-sandbox-create
           worktree
           :deny-reads
           (append (list toplevel +agent-shell-pstack-panel-directory
                         "~/.claude" "~/.codex")
                   +agent-shell-pstack-arena-extra-deny-reads
                   (delete worktree
                           (mapcar #'+agent-shell-arena-git-role-worktree
                                   (+agent-shell-arena-git-manifest-roles
                                    manifest))))
           :allow-reads (append
                         (list (+agent-shell-arena-git-common-dir manifest))
                         (mapcar (lambda (path)
                                   (expand-file-name path toplevel))
                                 (+agent-shell-arena-git-role-shared role))
                         (plist-get profile :read))
           :allow-metadata (list toplevel)
           :allow-writes (plist-get profile :write)
           :allow-connects (plist-get profile :connect)
           :env (plist-get profile :env)
           :trace trace :deadline deadline)))
    (condition-case err
        (+agent-shell-arena-git-write-files
         worktree (plist-get profile :files) sandbox)
      (error (+agent-shell-sandbox-discard sandbox)
             (signal (car err) (cdr err))))
    sandbox))

(defun +agent-shell-pstack-arena-judge-sandbox (manifest deadline)
  "Return a read-only sandbox for the judge of MANIFEST's arena.
DEADLINE is the same absolute horizon used for candidate sandboxes.
It works in the arena's checkouts folder and reads the checkouts and the
git store, but nothing a candidate's sandbox cannot read, nor any
candidate's worktree.  It writes only its private folders."
  (let ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
        (checkouts (expand-file-name
                    "checkouts" (+agent-shell-arena-git-manifest-directory
                                 manifest))))
    (make-directory checkouts t)
    (+agent-shell-sandbox-create
     checkouts
     :deny-reads (append (list toplevel +agent-shell-pstack-panel-directory
                               "~/.claude" "~/.codex")
                         +agent-shell-pstack-arena-extra-deny-reads
                         (mapcar #'+agent-shell-arena-git-role-worktree
                                 (+agent-shell-arena-git-manifest-roles
                                  manifest)))
     :allow-reads (list (+agent-shell-arena-git-common-dir manifest))
     :allow-metadata (list toplevel)
     :read-only t :deadline deadline)))

(defun +agent-shell-pstack-panel--launch (panel)
  "Check every member's host login, then start PANEL's candidates.
One refused login, even the judge's, refuses the whole arena, and no
member starts before every login is checked.  A panel that ended while
its logins were checked gets its follow-up once the check has."
  (setf (+agent-shell-pstack-panel-login-check panel)
        (+agent-shell-sandbox-check-logins
         (mapcar #'+agent-shell-pstack-panel-member-agent
                 (+agent-shell-pstack-panel-members panel))
         (+agent-shell-pstack-panel--horizon panel)
         (lambda (results)
           (let ((ended (eq (+agent-shell-pstack-panel-phase panel)
                            'finished)))
             (setf (+agent-shell-pstack-panel-login-check panel) nil
                   (+agent-shell-pstack-panel-logins panel) results)
             (+agent-shell-pstack-panel--preparing
              panel
              (or (seq-some #'+agent-shell-sandbox-login-result-error results)
                  (when (>= (float-time)
                            (+agent-shell-pstack-panel-deadline panel))
                    "The arena's deadline passed while its logins were \
checked."))
              (lambda ()
                (+agent-shell-pstack-panel--start-candidates panel)))
             (when ended (+agent-shell-pstack-panel--finalize panel))
             (+agent-shell-pstack-panels--refresh)))
         (lambda (results)
           (setf (+agent-shell-pstack-panel-logins panel) results)
           (+agent-shell-pstack-panels--refresh)))))

(defun +agent-shell-pstack-panel--start-candidates (panel)
  "Start PANEL's candidates in their sandboxes and their timer."
  (let ((candidates (+agent-shell-pstack-panel--unsettled panel 'candidate))
        (manifest (+agent-shell-pstack-panel-arena panel)))
    (dolist (member candidates)
      (setf (+agent-shell-pstack-panel-member-sandbox member)
            (+agent-shell-pstack-arena-sandbox
             manifest (+agent-shell-pstack-panel-member-role member)
             (+agent-shell-pstack-panel--horizon panel))))
    (setf (+agent-shell-pstack-panel-phase panel) 'candidates)
    (+agent-shell-pstack-panel--arm panel (+agent-shell-pstack-panel-timeout
                                           panel))
    (dolist (member candidates)
      (+agent-shell-pstack-panel--start-member panel member))
    (+agent-shell-pstack-panel--advance panel)))

(defun +agent-shell-pstack-panel--resume (lead)
  "Prepare the panels that waited for LEAD's interrupted turn to end."
  (dolist (panel (+agent-shell-pstack-panel-list))
    (when (and (eq lead (+agent-shell-pstack-panel-lead panel))
               (eq (+agent-shell-pstack-panel-phase panel) 'interrupting))
      (+agent-shell-pstack-panel--prepare panel))))

(defun +agent-shell-pstack-panel-cancel (panel reason &optional state)
  "Stop PANEL, settling its unfinished members as STATE because of REASON.
Settle the current phase's members and every pending member, which can
no longer start, as `error' unless STATE says otherwise.  An arena still
captures what its candidates made and never starts its judge, and one
still checking its logins ends once the check does.  An arena on a host
gets a cancel instead; see `+agent-shell-arena-remote-cancel'."
  (if (+agent-shell-pstack-panel-remote panel)
      (+agent-shell-arena-remote-cancel panel reason)
    (dolist (member (+agent-shell-pstack-panel-members panel))
      (when (or (memq member (+agent-shell-pstack-panel--phase-members panel))
                (eq 'pending (+agent-shell-pstack-panel-member-state member)))
        (+agent-shell-pstack-panel--settle
         panel member (or state 'error)
         (when-let* (((eq state 'timeout))
                     (shell (+agent-shell-pstack-panel-member-shell member)))
           (+agent-shell-drive-partial-text shell))
         reason))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--abandon (lead)
  "Stop the unfinished panels of LEAD, whose shell has ended.
An arena on a host decides for itself; see
`+agent-shell-arena-remote-abandon'."
  (dolist (panel (+agent-shell-pstack-panel-list))
    (when (eq lead (+agent-shell-pstack-panel-lead panel))
      (if (+agent-shell-pstack-panel-remote panel)
          (+agent-shell-arena-remote-abandon panel)
        (+agent-shell-pstack-panel-cancel panel "The lead's shell ended."))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--lead-event (event)
  "Read panel requests from a lead's EVENT and log every handled outcome."
  (let ((lead (plist-get event :shell))
        (kind (plist-get event :kind)))
    (when (memq kind '(turn-complete tool-call-update prompt-ready clean-up))
      (let* ((final (plist-get event :text))
             (+agent-shell-pstack-panel--log-entry
              (+agent-shell-pstack-panel--log-event
               lead (plist-get event :session) kind final)))
        (condition-case err
            (let ((text (or (and (eq kind 'turn-complete)
                                 (not (+agent-shell-pstack-panel--block final))
                                 (+agent-shell-pstack-panel--trailing-block-text
                                  (plist-get event :turn-text)))
                            final)))
              (+agent-shell-pstack-panel--log-request-text final text)
              (pcase kind
                ('tool-call-update
                 (+agent-shell-pstack-panel--request
                  lead (plist-get event :session) text
                  (or (plist-get event :title) "a tool")))
                ('turn-complete
                 (+agent-shell-pstack-panel--request
                  lead (plist-get event :session) text)
                 (+agent-shell-pstack-panel--resume lead)
                 (+agent-shell-pstack-panel--deliver-to lead))
                ('prompt-ready (+agent-shell-pstack-panel--deliver-to lead))
                ('clean-up (+agent-shell-pstack-panel--abandon lead))))
          (error (+agent-shell-pstack-panel--log-error err)))
        (condition-case err
            (+agent-shell-pstack-panels--refresh)
          (error (+agent-shell-pstack-panel--log-error err)))))))

(defun +agent-shell-pstack-panel-list ()
  "Return every panel, oldest first."
  (sort (hash-table-values +agent-shell-pstack-panel--panels)
        :key #'+agent-shell-pstack-panel-created))

;;;; Delegates

(defconst +agent-shell-pstack-panel--commit-limit 65536
  "Most bytes a sandboxed delegate's proposed commit message may take.")

(defun +agent-shell-pstack-panel--commit-blocks (text)
  "Return (START END JSON) for each pstack-commit block in TEXT."
  (let ((start 0) blocks)
    (while (string-match (rx bol "```pstack-commit" (* blank) "\n") text start)
      (let ((open (match-beginning 0))
            (body (match-end 0)))
        (if (string-match (rx bol "```" (* blank) eol) text body)
            (progn
              (push (list open (match-end 0)
                          (substring text body (max body (1- (match-beginning
                                                              0)))))
                    blocks)
              (setq start (match-end 0)))
          (push (list open (length text) nil) blocks)
          (setq start (length text)))))
    (nreverse blocks)))

(defun +agent-shell-pstack-panel--trailer-p (body)
  "Return whether BODY ends with a git trailer block, such as Co-authored-by."
  (let ((paragraph (car (last (split-string (string-trim-right body)
                                            "\n[ \t]*\n" t)))))
    (and paragraph
         (seq-every-p (lambda (line)
                        (string-match-p
                         "\\`[A-Za-z0-9][A-Za-z0-9-]*:[ \t]" line))
                      (split-string paragraph "\n" t)))))

(defun +agent-shell-pstack-panel--delegate-commit-message (text)
  "Return the commit message a sandboxed delegate's final TEXT proposes.
TEXT must end with exactly one version-1 pstack-commit JSON block, whose
nonempty one-line subject and string body take at most
`+agent-shell-pstack-panel--commit-limit' bytes of UTF-8.  Duplicate or
unknown keys, control characters other than the body's newlines and
tabs, and a body ending in git trailers are refused.  The message is the
subject, then a blank line and the body when there is one, as given;
nothing is invented, appended or rewritten.  Signal `user-error' saying
why there is no message, and collection keeps the work uncommitted."
  (let* ((text (or text ""))
         (blocks (+agent-shell-pstack-panel--commit-blocks text))
         (block (car blocks)))
    (cond
     ((null blocks)
      (user-error "The final response has no pstack-commit block"))
     ((cdr blocks)
      (user-error "The final response has %d pstack-commit blocks, not one"
                  (length blocks)))
     ((null (nth 2 block))
      (user-error "The pstack-commit block is not closed"))
     ((not (string-blank-p (substring text (nth 1 block))))
      (user-error "Text follows the pstack-commit block")))
    (let* ((json (nth 2 block))
           (object (condition-case nil
                       (json-parse-string json :object-type 'alist
                                          :array-type 'list
                                          :null-object nil :false-object :false)
                     (json-error
                      (user-error "The pstack-commit block is not valid JSON"))))
           (keys (and (consp object) (consp (car object))
                      (mapcar #'car object)))
           (subject (alist-get 'subject object))
           (body (alist-get 'body object)))
      (unless keys
        (user-error "The pstack-commit block must hold a JSON object"))
      (when-let* ((twice (seq-find (lambda (key) (memq key (cdr (memq key keys))))
                                   keys)))
        (user-error "The pstack-commit block names %s more than once" twice))
      (when-let* ((unknown (seq-difference keys '(version subject body))))
        (user-error "The pstack-commit block has unknown keys: %s"
                    (mapconcat #'symbol-name unknown ", ")))
      (unless (equal (alist-get 'version object) 1)
        (user-error "The pstack-commit block's version must be 1"))
      (unless (and (stringp subject) (not (string-blank-p subject)))
        (user-error "The pstack-commit subject must be a nonempty string"))
      (when (string-match-p "[[:cntrl:]]" subject)
        (user-error "The pstack-commit subject must be one line without \
control characters"))
      (unless (stringp body)
        (user-error "The pstack-commit body must be a string"))
      (when (string-match-p "[\0-\b\v-\x1f\x7f]" body)
        (user-error "The pstack-commit body may hold no control characters \
but newlines and tabs"))
      (when (+agent-shell-pstack-panel--trailer-p body)
        (user-error "The pstack-commit body ends with trailers, which Emacs \
does not add to a delegate's commit"))
      (let ((message (if (string-blank-p body) subject
                       (concat subject "\n\n" body))))
        (when (> (string-bytes (encode-coding-string message 'utf-8))
                 +agent-shell-pstack-panel--commit-limit)
          (user-error "The pstack-commit message is longer than %d bytes"
                      +agent-shell-pstack-panel--commit-limit))
        message))))

(defun +agent-shell-pstack-panel--prepare-delegate (panel)
  "Give PANEL's delegate a branch and somewhere to work, then start it.
Its policy comes from the repository's profile, once; see
`+agent-shell-arena-git-delegate-options'.  On a host it runs as
`+agent-shell-arena-remote-launch' says.  Here it gets a worktree of its
own or, with :delegate (:isolation branch), the lead's checkout, which
must be clean.  A worktree, here or on a host, starts from the snapshot
of the lead's working tree an arena takes, or HEAD when that is clean.
A delegate needs the laptop user's git identity first.  The panel's
timeout starts now and covers this preparation too."
  (setf (+agent-shell-pstack-panel-phase panel) 'preparing
        (+agent-shell-pstack-panel-deadline panel)
        (+ (float-time) (+agent-shell-pstack-panel-timeout panel)))
  (+agent-shell-arena-git-repository
   (+agent-shell-pstack-panel-cwd panel)
   (lambda (base error)
     (+agent-shell-pstack-panel--preparing
      panel error
      (lambda ()
        (let* ((toplevel (+agent-shell-arena-git-base-toplevel base))
               (profile (+agent-shell-arena-git-profile toplevel))
               (options (+agent-shell-arena-git-delegate-options profile))
               (route
                (lambda (identity)
                  (if (eq (plist-get options :isolation) 'branch)
                      (+agent-shell-pstack-panel--take-checkout panel base)
                    (+agent-shell-arena-git-capture-base
                     (+agent-shell-pstack-panel-cwd panel)
                     (lambda (snapshot error)
                       (+agent-shell-pstack-panel--preparing
                        panel error
                        (lambda ()
                          (if (plist-get options :host)
                              (progn
                                (require '+agent-shell-arena-remote)
                                (+agent-shell-arena-remote-launch
                                 panel snapshot profile))
                            (+agent-shell-pstack-panel--provision-delegate
                             panel snapshot)))))
                     t identity
                     (format "Snapshot uncommitted work for pstack delegate %s"
                             (+agent-shell-pstack-panel-id panel)))))))
          (setf (+agent-shell-pstack-panel-delegate-options panel) options)
          (when (and (plist-get options :sandbox)
                     (not (plist-get options :host))
                     (not +agent-shell-sandbox-backend))
            (user-error "This repository's profile sandboxes delegates, which \
needs macOS's sandbox-exec or Linux's bwrap; this Emacs runs on %s"
                        system-type))
          (+agent-shell-arena-git-user-identity
           toplevel
           (lambda (identity error)
             (+agent-shell-pstack-panel--preparing
              panel error
              (lambda ()
                (when (or (plist-get options :host)
                          (plist-get options :sandbox))
                  (setf (+agent-shell-pstack-panel-identity panel) identity))
                (funcall route identity))))))))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--provision-delegate (panel base)
  "Give PANEL's delegate a worktree on its new branch at BASE, then start it.
A branch of that name already there is refused, never reused or reset."
  (let* ((options (+agent-shell-pstack-panel-delegate-options panel))
         (branch (concat "pstack/delegate/" (+agent-shell-pstack-panel-id panel))))
    (when (+agent-shell-arena-git--read-in
           (+agent-shell-arena-git-base-git-dir base)
           (+agent-shell-arena-git-base-toplevel base)
           "rev-parse" "--verify" "-q" (concat "refs/heads/" branch))
      (user-error "Branch %s already exists in %s; Emacs never reuses it.  \
Delete or rename it, then request the delegate again" branch
                  (abbreviate-file-name
                   (+agent-shell-arena-git-base-toplevel base))))
    (let ((manifest (+agent-shell-arena-git-create
                     base (+agent-shell-pstack-panel-id panel)
                     (+agent-shell-arena-git--delegate
                      :isolation 'worktree
                      :sandbox (plist-get options :sandbox)))))
      (setf (+agent-shell-pstack-panel-arena panel) manifest)
      (+agent-shell-arena-git-provision
       manifest
       (+agent-shell-pstack-panel-member-role
        (car (+agent-shell-pstack-panel-members panel)))
       (lambda (_directory error)
         (+agent-shell-pstack-panel--preparing
          panel error (lambda () (+agent-shell-pstack-panel--start-delegate
                                  panel))))
       branch))))

(defun +agent-shell-pstack-panel--take-checkout (panel base)
  "Put the lead's clean checkout at BASE on PANEL's delegate branch.
Record where it was first, so a restart can still return it."
  (+agent-shell-arena-git-changes
   (+agent-shell-arena-git-base-git-dir base)
   (+agent-shell-arena-git-base-toplevel base) nil
   (lambda (paths error)
     (+agent-shell-pstack-panel--preparing
      panel (or error
                (when paths
                  (format "Branch-mode delegates need a clean checkout; \
commit or stash your changes first.  Changed: %s."
                          (+agent-shell-arena-git-describe-paths paths))))
      (lambda ()
        (let ((manifest
               (+agent-shell-arena-git-create
                base (+agent-shell-pstack-panel-id panel)
                (+agent-shell-arena-git--delegate
                 :isolation 'branch
                 :sandbox (+agent-shell-pstack-panel--sandboxed-p panel)
                 :origin (if-let* ((ref (+agent-shell-arena-git-base-branch
                                         base)))
                             (cons 'branch ref)
                           (cons 'detached
                                 (+agent-shell-arena-git-base-head base)))))))
          (setf (+agent-shell-pstack-panel-arena panel) manifest)
          (+agent-shell-arena-git-take
           manifest
           (lambda (_branch error)
             (+agent-shell-pstack-panel--preparing
              panel error
              (lambda () (+agent-shell-pstack-panel--start-delegate
                          panel)))))))))))

(defun +agent-shell-pstack-panel--start-delegate (panel)
  "Start PANEL's delegate for what remains of its timeout.
A sandboxed delegate has its host login admitted first, as an arena's
members do, then runs confined; see `+agent-shell-pstack-delegate-sandbox'."
  (let ((remaining (- (+agent-shell-pstack-panel-deadline panel) (float-time))))
    (cond
     ((<= remaining 0)
      (+agent-shell-pstack-panel--fail-preparation
       panel (format "The %d-second timeout passed while Emacs prepared \
the delegate's workspace."
                     (+agent-shell-pstack-panel-timeout panel))))
     ((+agent-shell-pstack-panel--sandboxed-p panel)
      (+agent-shell-pstack-panel--admit-delegate panel))
     (t (+agent-shell-pstack-panel--run-delegate panel)))))

(defun +agent-shell-pstack-panel--run-delegate (panel)
  "Start PANEL's delegate now, with its timer for what remains."
  (setf (+agent-shell-pstack-panel-phase panel) 'delegating)
  (+agent-shell-pstack-panel--arm
   panel (max 0 (- (+agent-shell-pstack-panel-deadline panel) (float-time))))
  (let ((member (car (+agent-shell-pstack-panel-members panel))))
    (when (+agent-shell-pstack-panel--sandboxed-p panel)
      (condition-case err
          (setf (+agent-shell-pstack-panel-member-sandbox member)
                (+agent-shell-pstack-delegate-sandbox
                 (+agent-shell-pstack-panel-arena panel)
                 (+agent-shell-pstack-panel--horizon panel)))
        (error (+agent-shell-pstack-panel--settle
                panel member 'error nil (error-message-string err)))))
    (unless (+agent-shell-pstack-panel--settled-p member)
      (+agent-shell-pstack-panel--start-member panel member)))
  (+agent-shell-pstack-panel--advance panel)
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-pstack-panel--admit-delegate (panel)
  "Check sandboxed PANEL's delegate's host login, then start it.
A panel that ended while its login was checked gets its follow-up once
the check has."
  (setf (+agent-shell-pstack-panel-login-check panel)
        (+agent-shell-sandbox-check-logins
         (list (+agent-shell-pstack-panel-member-agent
                (car (+agent-shell-pstack-panel-members panel))))
         (+agent-shell-pstack-panel--horizon panel)
         (lambda (results)
           (let ((ended (eq (+agent-shell-pstack-panel-phase panel) 'finished)))
             (setf (+agent-shell-pstack-panel-login-check panel) nil
                   (+agent-shell-pstack-panel-logins panel) results)
             (+agent-shell-pstack-panel--preparing
              panel
              (or (seq-some #'+agent-shell-sandbox-login-result-error results)
                  (when (>= (float-time)
                            (+agent-shell-pstack-panel-deadline panel))
                    "The delegate's timeout passed while its login was \
checked."))
              (lambda () (+agent-shell-pstack-panel--run-delegate panel)))
             (when ended (+agent-shell-pstack-panel--finalize panel))
             (+agent-shell-pstack-panels--refresh)))
         (lambda (results)
           (setf (+agent-shell-pstack-panel-logins panel) results)
           (+agent-shell-pstack-panels--refresh)))))

(defun +agent-shell-pstack-delegate-sandbox (manifest deadline)
  "Return a sandbox confining MANIFEST's delegate to where it works.
DEADLINE is its fixed absolute stop time plus capture grace.  Its root is
its worktree, or the lead's checkout in branch isolation.  Git's
metadata stays read-only: the worktree's .git pointer, its own git
directory and the common one, even where the repository profile's :write
overlaps them, so it cannot commit, write a ref or change git config.
Otherwise it gets what an arena candidate gets, from the same profile;
in a worktree, its :files are written for the new sandbox."
  (let* ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (profile (+agent-shell-arena-git-profile toplevel))
         (branch (eq 'branch (+agent-shell-arena-git-delegate-isolation
                              (+agent-shell-arena-git-manifest-delegate
                               manifest))))
         (role (car (+agent-shell-arena-git-manifest-roles manifest)))
         (root (if branch toplevel (+agent-shell-arena-git-role-worktree role)))
         (common (+agent-shell-arena-git-common-dir manifest))
         (sandbox
          (+agent-shell-sandbox-create
           root
           :deny-reads (append (list +agent-shell-pstack-panel-directory
                                     "~/.claude" "~/.codex")
                               +agent-shell-pstack-arena-extra-deny-reads
                               (unless branch (list toplevel)))
           :allow-reads (append (list common)
                                (unless branch
                                  (mapcar (lambda (path)
                                            (expand-file-name path toplevel))
                                          (+agent-shell-arena-git-role-shared
                                           role)))
                                (plist-get profile :read))
           :allow-metadata (unless branch (list toplevel))
           :allow-writes (plist-get profile :write)
           :allow-connects (plist-get profile :connect)
           :env (plist-get profile :env)
           :protect (delete-dups
                     (delq nil
                           (list (expand-file-name ".git" root)
                                 (if branch
                                     (+agent-shell-arena-git-manifest-git-dir
                                      manifest)
                                   (+agent-shell-arena-git-role-git-dir role))
                                 common)))
           :deadline deadline)))
    (unless branch
      (condition-case err
          (+agent-shell-arena-git-write-files
           root (plist-get profile :files) sandbox)
        (error (+agent-shell-sandbox-discard sandbox)
               (signal (car err) (cdr err)))))
    sandbox))

(defun +agent-shell-pstack-panel--interrupted ()
  "Return the manifests of branch-mode delegates no panel holds, unreturned.
Each may have left the lead's checkout on its branch, as when Emacs
exited mid-run."
  (let ((held (delq nil (mapcar (lambda (panel)
                                  (when-let* ((manifest
                                               (+agent-shell-pstack-panel-arena
                                                panel)))
                                    (+agent-shell-arena-git-manifest-id
                                     manifest)))
                                (+agent-shell-pstack-panel-list)))))
    (seq-filter
     (lambda (manifest)
       (when-let* ((delegate (+agent-shell-arena-git-manifest-delegate
                              manifest)))
         (and (eq (+agent-shell-arena-git-delegate-isolation delegate) 'branch)
              (not (memq (+agent-shell-arena-git-delegate-restore delegate)
                         '(restored unneeded)))
              (not (member (+agent-shell-arena-git-manifest-id manifest)
                           held)))))
     (+agent-shell-arena-git-arenas))))

;;;; View

(defconst +agent-shell-pstack-panels--buffer "*pstack panels*"
  "Name of the panels buffer.")

(defvar-keymap +agent-shell-pstack-panels-mode-map
  :doc "Keymap for `+agent-shell-pstack-panels-mode'."
  "RET" #'+agent-shell-pstack-panels-visit
  "x" #'+agent-shell-pstack-panels-clean
  "c" #'+agent-shell-pstack-panels-cancel
  "l" #'+agent-shell-pstack-panel-log)

(define-derived-mode +agent-shell-pstack-panels-mode special-mode
  "pstack panels"
  "List pstack panels, their members and reports.
\\<+agent-shell-pstack-panels-mode-map>\\[+agent-shell-pstack-panels-visit] visits the report or shell on the current line.
\\[+agent-shell-pstack-panels-clean] cleans up the finished arena or delegate on the line.
\\[+agent-shell-pstack-panels-cancel] cancels the unfinished panel on the line.
\\[+agent-shell-pstack-panel-log] shows the lead's request log."
  (setq-local revert-buffer-function
              (lambda (&rest _) (+agent-shell-pstack-panels--render))
              truncate-lines t))

(defun +agent-shell-pstack-panels--status (panel)
  "Return a word describing PANEL's progress."
  (cond ((+agent-shell-pstack-panel-rejection panel) "rejected")
        ((+agent-shell-pstack-panel-delivered panel) "delivered")
        ((+agent-shell-pstack-panel-followup panel) "waiting for the lead")
        (t (symbol-name (+agent-shell-pstack-panel-phase panel)))))

(defun +agent-shell-pstack-panels--line (target &rest parts)
  "Insert PARTS as one line that visits TARGET."
  (insert (propertize (concat (apply #'concat parts) "\n")
                      '+agent-shell-pstack-panels-target target)))

(defun +agent-shell-pstack-panels--name (buffer)
  "Return BUFFER's name, or a note that it has ended or never started."
  (cond ((buffer-live-p buffer) (buffer-name buffer))
        (buffer "ended")
        (t "not started")))

(defun +agent-shell-pstack-panels--return (manifest)
  "Describe returning the checkout branch-mode MANIFEST's delegate took."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (name (+agent-shell-arena-git-origin-name
                (+agent-shell-arena-git-delegate-origin delegate))))
    (if (memq (+agent-shell-arena-git-delegate-restore delegate)
              '(restored unneeded))
        ;; Its record goes once it is back, unless removing it failed.
        (format "back on %s%s" name
                (if (file-exists-p (+agent-shell-arena-git-manifest-directory
                                    manifest))
                    "  (x forgets its record)"
                  ""))
      (format "%s(x switches it back to %s)"
              (if-let* ((note (+agent-shell-arena-git-delegate-note delegate)))
                  (concat note "  ")
                "")
              name))))

(defun +agent-shell-pstack-panels--last-request (lead)
  "Return LEAD's latest turn if it had a block but started no panel."
  (when-let* ((log (gethash lead +agent-shell-pstack-panel--logs))
              (entry (seq-find (lambda (entry)
                                 (eq (plist-get entry :kind) 'turn-complete))
                               (ring-elements log)))
              ((plist-get entry :block))
              ((not (memq (plist-get entry :outcome) '(started interrupted)))))
    entry))

(defun +agent-shell-pstack-panels--render ()
  "Redraw the current panels buffer, keeping point's line."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (panels (+agent-shell-pstack-panel-list)))
    (erase-buffer)
    (unless panels
      (insert "No panels yet.  A pstack lead requests one by ending its \
message with a pstack-panel block.\n"))
    (maphash
     (lambda (lead _log)
       (when-let* ((entry (+agent-shell-pstack-panels--last-request lead)))
         (let ((start (point)))
           (+agent-shell-pstack-panels--line
            lead (+agent-shell-pstack-panels--name lead)
            "  Last request: " (+agent-shell-pstack-panel--log-outcome-text entry)
            "  (l: log)")
           (put-text-property start (point) '+agent-shell-pstack-panels-lead
                              lead))))
     +agent-shell-pstack-panel--logs)
    (dolist (panel panels)
      (let ((lead (+agent-shell-pstack-panel-lead panel))
            (start (point)))
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
        (when-let* ((manifest (+agent-shell-pstack-panel-arena panel)))
          (let ((directory (+agent-shell-arena-git-manifest-directory
                            manifest))
                (finished (eq (+agent-shell-pstack-panel-phase panel)
                              'finished)))
            (pcase (if (and (+agent-shell-pstack-panel-remote panel)
                            (+agent-shell-pstack-panel--delegate-p panel))
                       'remote
                     (+agent-shell-pstack-panel--isolation panel))
              ('remote
               (+agent-shell-pstack-panels--line
                directory "  Branch: "
                (+agent-shell-arena-git-delegate-branch manifest))
               (+agent-shell-pstack-panels--line
                directory "  Workspace on host: "
                (if-let* ((arena (+agent-shell-arena-remote-arena
                                  (+agent-shell-pstack-panel-remote panel))))
                    (format ".../arenas/%s/%s" arena
                            (+agent-shell-pstack-panel-member-role
                             (car (+agent-shell-pstack-panel-members panel))))
                  "not made yet")
                (if finished "  (x cleans it up there)" "")))
              ('branch
               (+agent-shell-pstack-panels--line
                (+agent-shell-arena-git-manifest-toplevel manifest)
                "  Isolation: "
                (if finished
                    (+agent-shell-pstack-panels--return manifest)
                  "your checkout, on the delegate's branch; do not edit or \
commit there until this finishes")))
              (isolation
               (+agent-shell-pstack-panels--line
                directory (if isolation "  Workspace: " "  Arena: ")
                (abbreviate-file-name directory)
                (if finished "  (x cleans it up)" ""))))
            (when-let* ((remote (+agent-shell-pstack-panel-remote panel)))
              (+agent-shell-pstack-panels--line
               directory "  Host: "
               (+agent-shell-arena-remote-describe remote t)))))
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (let ((report (+agent-shell-pstack-panel-member-report member))
                (shell (+agent-shell-pstack-panel-member-shell member))
                (capture (+agent-shell-pstack-panel-member-capture member))
                (result (+agent-shell-pstack-panel-member-result member))
                (remote (and (eq 'delegate
                                 (+agent-shell-pstack-panel-member-kind member))
                             (+agent-shell-pstack-panel-remote panel))))
            (if remote
                (+agent-shell-pstack-panels--transcript-lines remote member)
            (+agent-shell-pstack-panels--line
             (or report shell)
             (format "  %-16s %-12s %-9s "
                     (+agent-shell-pstack-panel-member-role member)
                     (+agent-shell-pstack-panel-member-agent member)
                     (+agent-shell-pstack-panel-member-state member))
             (cond (capture
                    (string-join
                     (delq nil (list (symbol-name capture)
                                     (plist-get result :commit)
                                     (plist-get result :error)))
                     " "))
                   (report (abbreviate-file-name report))
                   ((+agent-shell-pstack-panel-member-error member))
                   (t (+agent-shell-pstack-panels--name shell)))))))
        (insert "\n")
        (put-text-property start (point) '+agent-shell-pstack-panels-panel
                           panel)))
    (dolist (manifest (+agent-shell-pstack-panel--interrupted))
      (let ((start (point))
            (toplevel (+agent-shell-arena-git-manifest-toplevel manifest)))
        (+agent-shell-pstack-panels--line
         toplevel
         (propertize (+agent-shell-arena-git-manifest-id manifest) 'face 'bold)
         (if (+agent-shell-arena-git-delegate-restore
              (+agent-shell-arena-git-manifest-delegate manifest))
             "  delegate  not returned"
           "  delegate  interrupted"))
        (+agent-shell-pstack-panels--line
         toplevel "  Isolation: " (abbreviate-file-name toplevel) " is on "
         (+agent-shell-arena-git-delegate-branch manifest)
         "  " (+agent-shell-pstack-panels--return manifest))
        (insert "\n")
        (put-text-property start (point) '+agent-shell-pstack-panels-manifest
                           manifest)))
    (goto-char (point-min))
    (forward-line (1- line))))

(declare-function +agent-shell-arena-remote-transcript-buffer
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-arena "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-polled "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-phase "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-host "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote--host-label
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-outcome-workspace
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-outcome-dirty-count
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-outcome-dirty-paths
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-outcome-finalization
                  "+agent-shell-arena-remote")
(declare-function +agent-shell-arena-remote-outcome-retained-reason
                  "+agent-shell-arena-remote")

(defun +agent-shell-pstack-panels--transcript-lines (remote member)
  "Insert the lines of remote delegate MEMBER, whose record is REMOTE.
Its member line opens its read-only transcript, even once it finished,
and a Report line its saved final report."
  (let ((report (+agent-shell-pstack-panel-member-report member))
        (polled (+agent-shell-arena-remote-polled remote)))
    (+agent-shell-pstack-panels--line
     (cons 'pstack-transcript remote)
     (format "  %-16s %-12s %-9s "
             (+agent-shell-pstack-panel-member-role member)
             (+agent-shell-pstack-panel-member-agent member)
             (+agent-shell-pstack-panel-member-state member))
     (format "transcript (%s; RET)"
             (if polled
                 (format "updated %ds ago" (round (- (float-time) polled)))
               (format "%s" (+agent-shell-arena-remote-phase remote)))))
    (when report
      (+agent-shell-pstack-panels--line
       report "    Report: " (abbreviate-file-name report)))))

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
            (format "pstack panel %s (%s%s)"
                    (+agent-shell-pstack-panel-id panel)
                    (+agent-shell-pstack-panel-shape panel)
                    (let ((phase (+agent-shell-pstack-panel-phase panel)))
                      (if (eq phase 'running) "" (format ", %s" phase))))
            (mapconcat
             (lambda (member)
               (format "\n  %s  %s  %s"
                       (+agent-shell-pstack-panel-member-role member)
                       (+agent-shell-pstack-panel-member-agent member)
                       (+agent-shell-pstack-panel-member-state member)))
             (+agent-shell-pstack-panel-members panel) "")
            (when (eq (+agent-shell-pstack-panel--isolation panel) 'branch)
              "\n  the delegate works in your checkout; do not edit or \
commit there until it finishes")))
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
  "Redraw the panels buffer if it exists, and the leads' mode lines.
Then run `+agent-shell-pstack-panel-change-hook'."
  (+agent-shell-pstack-panel--update-mode-lines)
  (run-hooks '+agent-shell-pstack-panel-change-hook)
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
  "Visit the report, member shell or lead shell on the current line.
A remote delegate's member line opens its read-only transcript."
  (interactive)
  (pcase (get-text-property (point) '+agent-shell-pstack-panels-target)
    ((and (pred stringp) file) (find-file-other-window file))
    (`(pstack-transcript . ,remote)
     (pop-to-buffer (+agent-shell-arena-remote-transcript-buffer remote)))
    ((and (pred buffer-live-p) buffer) (pop-to-buffer buffer))
    (_ (user-error "Nothing to visit on this line"))))

(defun +agent-shell-pstack-panels-cancel ()
  "Cancel the unfinished panel on the current line, after confirmation.
An arena captures what its candidates made and skips its judge."
  (interactive)
  (let ((panel (get-text-property (point) '+agent-shell-pstack-panels-panel)))
    (cond
     ((null panel) (user-error "No panel on this line"))
     ((+agent-shell-pstack-panel-followup panel)
      (user-error "Panel %s has finished" (+agent-shell-pstack-panel-id panel)))
     ((yes-or-no-p (format "Cancel panel %s? "
                           (+agent-shell-pstack-panel-id panel)))
      (+agent-shell-pstack-panel-cancel panel "You cancelled the panel.")))))

(defun +agent-shell-pstack-arena--clean (manifest)
  "Clean MANIFEST's arena after confirmation, forgetting it in its panel.
An arena that ran on a host is cleaned there too."
  (let ((id (+agent-shell-arena-git-manifest-id manifest))
        (remote (and (featurep '+agent-shell-arena-remote)
                     (+agent-shell-arena-remote-recorded-p manifest))))
    (when (yes-or-no-p
           (pcase (when-let* ((delegate
                               (+agent-shell-arena-git-manifest-delegate
                                manifest)))
                    (if remote 'remote
                      (+agent-shell-arena-git-delegate-isolation delegate)))
             ('remote
              (let* ((panel (seq-find
                             (lambda (panel)
                               (when-let* ((held (+agent-shell-pstack-panel-arena
                                                  panel)))
                                 (equal id (+agent-shell-arena-git-manifest-id
                                            held))))
                             (+agent-shell-pstack-panel-list)))
                     (outcome (and panel
                                   (plist-get
                                    (+agent-shell-pstack-panel-member-result
                                     (car (+agent-shell-pstack-panel-members
                                           panel)))
                                    :outcome)))
                     (dirty (if outcome
                                (+agent-shell-arena-remote-outcome-dirty-count
                                 outcome)
                              0)))
                (format "Remove delegate %s's job and worktree from its \
host%s?  Its branch %s stays. " id
                        (if (> dirty 0)
                            (format ", losing the %d uncommitted path%s there"
                                    dirty (if (= dirty 1) "" "s"))
                          "")
                        (+agent-shell-arena-git-delegate-branch manifest))))
             ('worktree
              (format "Remove delegate %s's worktree from %s?  Its branch %s \
stays. " id (+agent-shell-arena-git-manifest-toplevel manifest)
                      (+agent-shell-arena-git-delegate-branch manifest)))
             ('branch
              (format "Return %s from %s if it is clean, and forget delegate \
%s?  The branch stays. " (+agent-shell-arena-git-manifest-toplevel manifest)
                      (+agent-shell-arena-git-delegate-branch manifest) id))
             (_ (format "Remove arena %s's worktrees, checkouts and result \
refs from %s%s? " id (+agent-shell-arena-git-manifest-toplevel manifest)
                        (if remote " and its host" "")))))
      (dolist (panel (+agent-shell-pstack-panel-list))
        (when-let* ((held (+agent-shell-pstack-panel-arena panel))
                    ((equal id (+agent-shell-arena-git-manifest-id held))))
          (setf (+agent-shell-pstack-panel-arena panel) nil)))
      (funcall
       (if remote #'+agent-shell-arena-remote-clean
         #'+agent-shell-arena-git-clean)
       manifest
       (lambda (left error)
         (cond (error (message "Arena %s: cleanup failed: %s" id error))
               (left (message "Arena %s: cleaned, but left %s" id
                              (string-join left "; ")))
               (t (message "Arena %s: cleaned" id)))
         (+agent-shell-pstack-panels--refresh)))
      (+agent-shell-pstack-panels--refresh))))

(defun +agent-shell-pstack-panels-clean ()
  "Clean up the finished arena or delegate on the current line.
Ask for confirmation first."
  (interactive)
  (let ((panel (get-text-property (point) '+agent-shell-pstack-panels-panel))
        (manifest (get-text-property (point)
                                     '+agent-shell-pstack-panels-manifest)))
    (cond
     (manifest (+agent-shell-pstack-arena--clean manifest))
     ((not (and panel (+agent-shell-pstack-panel-arena panel)))
      (user-error "No arena on this line"))
     ((not (eq (+agent-shell-pstack-panel-phase panel) 'finished))
      (user-error "Arena %s is still %s" (+agent-shell-pstack-panel-id panel)
                  (+agent-shell-pstack-panel-phase panel)))
     ((not (file-exists-p (+agent-shell-arena-git-manifest-directory
                           (+agent-shell-pstack-panel-arena panel))))
      (user-error "Panel %s left nothing to clean up"
                  (+agent-shell-pstack-panel-id panel)))
     (t (+agent-shell-pstack-arena--clean
         (+agent-shell-pstack-panel-arena panel))))))

;;;###autoload
(defun +agent-shell-pstack-arena-clean ()
  "Choose an arena on disk and clean it up, after confirmation.
Arenas whose panel still runs are left out."
  (interactive)
  (let* ((running
          (seq-keep (lambda (panel)
                      (when-let* (((not (eq (+agent-shell-pstack-panel-phase
                                             panel)
                                            'finished)))
                                  (manifest (+agent-shell-pstack-panel-arena
                                             panel)))
                        (+agent-shell-arena-git-manifest-id manifest)))
                    (+agent-shell-pstack-panel-list)))
         (choices
          (seq-keep (lambda (manifest)
                      (let ((id (+agent-shell-arena-git-manifest-id manifest))
                            (toplevel (+agent-shell-arena-git-manifest-toplevel
                                       manifest)))
                        (unless (member id running)
                          (cons (format "%s  %s" id
                                        (abbreviate-file-name toplevel))
                                manifest))))
                    (+agent-shell-arena-git-arenas))))
    (unless choices
      (user-error "No arenas to clean in %s"
                  +agent-shell-pstack-arena-directory))
    (+agent-shell-pstack-arena--clean
     (cdr (assoc (completing-read "Clean arena: " choices nil t) choices)))))

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
