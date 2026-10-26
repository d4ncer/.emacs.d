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
as the arena skill says."))
  "Panel shapes by name.
Each value is a plist: :summary describes the shape to the lead, :contract
ends each member's prompt, and :triage ends the follow-up to the lead.  An
arena's :judge ends its judge's prompt."
  :type '(alist :key-type symbol :value-type plist))

(defcustom +agent-shell-pstack-panel-default-judge-timeout 900
  "Seconds an arena's judge may run when the request names none."
  :type 'natnum)

(defconst +agent-shell-pstack-panel--rubric-range '(3 . 6)
  "Fewest and most criteria an arena rubric may list.")

(defconst +agent-shell-pstack-panel--schema 6
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
REMOTE holds a `+agent-shell-arena-remote' when a host runs the arena."
  id shape brief timeout lead cwd members timer rejection followup delivered
  interrupted (created (float-time))
  (schema +agent-shell-pstack-panel--schema) (phase 'running)
  rubric judge-timeout arena sink remote)

(defvar +agent-shell-pstack-panel-change-hook nil
  "Hook run after any panel changes, with no arguments.")

(defvar +agent-shell-pstack-arena-extra-deny-reads nil
  "More folders no arena candidate may read, such as a runner's records.")

(defvar +agent-shell-pstack-arena-deadline nil
  "Time each arena candidate's login must outlast, or nil for now.
A remote arena's runner sets it to its deadline.")

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
:stat and :checkout, or :error."
  role agent focus shell (state 'starting) text error report (kind 'member)
  sandbox capture result)

(defvar +agent-shell-pstack-panel--panels (make-hash-table :test #'equal)
  "Map panel id to panel, including rejected requests.")

(defun +agent-shell-pstack-panel--current-p (panel)
  "Return whether PANEL was made with the current struct schema."
  (eql (ignore-errors (+agent-shell-pstack-panel-schema panel))
       +agent-shell-pstack-panel--schema))

;; A reload that changed a struct would break every older record in place.
(let ((stale (seq-remove
              #'+agent-shell-pstack-panel--current-p
              (hash-table-values +agent-shell-pstack-panel--panels))))
  (when stale
    (dolist (id (hash-table-keys +agent-shell-pstack-panel--panels))
      (unless (+agent-shell-pstack-panel--current-p
               (gethash id +agent-shell-pstack-panel--panels))
        (remhash id +agent-shell-pstack-panel--panels)))
    (message "Dropped %d pstack panel%s made before the panel code changed; \
kill their member shells by hand" (length stale) (if (cdr stale) "s" ""))))

(defconst +agent-shell-pstack-panel--settled '(done error timeout)
  "Member states that end a member.")

(defconst +agent-shell-pstack-panel--phase-kinds
  '((running . member) (candidates . candidate) (judging . judge))
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
Without VALUE, prefer a read-only agent other than LEAD-AGENT."
  (let* ((agents (+agent-shell-pstack-panel--agents))
         (agent
          (if (null value)
              (or (seq-find (lambda (agent) (not (eq agent lead-agent))) agents)
                  (car agents)
                  (user-error "No read-only agent can judge the arena"))
            (let ((name (and (+agent-shell-pstack-panel--object-p value)
                             (map-elt value 'agent))))
              (unless (and (stringp name) (memq (intern name) agents))
                (user-error "Judge must be an object whose agent is \
read-only; available agents: %s" (+agent-shell-pstack-panel--names agents)))
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
    (let ((timeout (+agent-shell-pstack-panel--seconds
                    request 'timeout +agent-shell-pstack-panel-default-timeout
                    "Timeout"))
          (agents (if arena (+agent-shell-pstack-panel--candidate-agents)
                    (+agent-shell-pstack-panel--agents))))
      (unless (and (proper-list-p values)
                   (<= 1 (length values) +agent-shell-pstack-panel-max-members))
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
                (judge-timeout (+agent-shell-pstack-panel--seconds
                                request 'judge_timeout
                                +agent-shell-pstack-panel-default-judge-timeout
                                "Judge timeout")))
            (+agent-shell-pstack-panel--make
             :shape shape :brief brief :timeout timeout :rubric rubric
             :judge-timeout judge-timeout :phase 'preparing
             :members (append members (list judge)))))))))

;;;; Prompts

(defun +agent-shell-pstack-panel--shapes ()
  "Return the entries of the shapes a lead may request here.
An arena needs a sandbox backend and an agent that can run sandboxed."
  (seq-remove (lambda (shape)
                (and (eq (car shape) 'arena)
                     (not (and +agent-shell-sandbox-backend
                               (+agent-shell-pstack-panel--candidate-agents)))))
              +agent-shell-pstack-panel-shapes))

(defun +agent-shell-pstack-panel--series (shapes conjunction)
  "Return the names of SHAPES as a series joined by CONJUNCTION."
  (let ((names (mapcar (lambda (shape) (symbol-name (car shape))) shapes)))
    (if (cdr names)
        (format "%s %s %s" (string-join (butlast names) ", ") conjunction
                (car (last names)))
      (car names))))

(defun +agent-shell-pstack-panel--arena-protocol (agents)
  "Return the protocol paragraph about arenas, naming read-only AGENTS."
  (format "\nAn arena's members are candidates that write code.  Each starts \
in its own sandboxed git worktree holding a snapshot of your working tree, and \
sees only the brief and its focus.  Candidate agents: %s.  An arena request \
also carries \"rubric\", %d to %d criteria that only the judge sees.  It may \
add \"judge\":{\"agent\":\"%s\"}, a read-only agent that by default differs \
from yours, and \"judge_timeout\", %d seconds by default; \"timeout\" bounds \
the candidates.  Emacs runs the candidates, then the judge, and sends you one \
prompt with each candidate's result commit, diff stat, rationale and the \
command that applies its diff, then the judge's scores and how to pick, \
graft and verify."
          (+agent-shell-pstack-panel--names
           (+agent-shell-pstack-panel--candidate-agents))
          (car +agent-shell-pstack-panel--rubric-range)
          (cdr +agent-shell-pstack-panel--rubric-range)
          (car agents) +agent-shell-pstack-panel-default-judge-timeout))

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
Each member starts fresh, read-only, in your working directory, and sees \
only the brief and its own focus.  Use 1 to %d members with unique roles of \
lowercase letters, digits and hyphens.  The timeout is in seconds, at most \
%d.  End your turn right after the block.  Emacs starts the panel as soon as \
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
              (if (assq 'arena shapes)
                  (+agent-shell-pstack-panel--arena-protocol agents)
                "")))))

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
agent.  You are read-only: Emacs refuses edits and commands that change \
state.  Each candidate below worked on the same task from the same base \
commit, alone in a sandbox.\n\nTask:\n"
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
       "\n" (+agent-shell-pstack-panel--shape panel :triage)
       "\nThe results stay until you clean the arena up: press x on its line \
in the pstack panels view, or run M-x +agent-shell-pstack-arena-clean."))))

;;;; Lifecycle

(defun +agent-shell-pstack-panel--arena-p (panel)
  "Return whether PANEL is an arena."
  (eq (+agent-shell-pstack-panel-shape panel) 'arena))

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
  (setf (+agent-shell-pstack-panel-timer panel)
        (run-at-time seconds nil #'+agent-shell-pstack-panel--expire
                     (+agent-shell-pstack-panel-id panel)
                     (+agent-shell-pstack-panel-phase panel))))

(defun +agent-shell-pstack-panel--finish (panel)
  "End PANEL, compose its follow-up and deliver it, once.
A panel with a sink hands itself to the sink instead."
  (unless (eq (+agent-shell-pstack-panel-phase panel) 'finished)
    (+agent-shell-pstack-panel--disarm panel)
    (setf (+agent-shell-pstack-panel-phase panel) 'finished
          (+agent-shell-pstack-panel-followup panel)
          (+agent-shell-pstack-panel-followup-text panel))
    (if-let* ((sink (+agent-shell-pstack-panel-sink panel)))
        (funcall sink panel)
      (+agent-shell-pstack-panel--deliver-to
       (+agent-shell-pstack-panel-lead panel)))))

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
         (+agent-shell-pstack-panel--finish panel))))))

(defun +agent-shell-pstack-panel--usable-p (candidate)
  "Return whether CANDIDATE's captured result changed the base."
  (and (eq (+agent-shell-pstack-panel-member-capture candidate) 'captured)
       (plist-get (+agent-shell-pstack-panel-member-result candidate)
                  :changed)))

(defun +agent-shell-pstack-panel--judge (panel judge)
  "Start PANEL's JUDGE, read-only, with its own timer."
  (setf (+agent-shell-pstack-panel-phase panel) 'judging)
  (+agent-shell-pstack-panel--arm
   panel (+agent-shell-pstack-panel-judge-timeout panel))
  (+agent-shell-pstack-panel--start-member panel judge)
  (+agent-shell-pstack-panel--advance panel)
  (+agent-shell-pstack-panels--refresh))

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
    ;; Killing a shell from its own event callback corrupts upstream state.
    (if (eq (+agent-shell-pstack-panel-member-kind member) 'candidate)
        (run-at-time 0 nil #'+agent-shell-pstack-panel--halt panel member)
      (when-let* ((shell (+agent-shell-pstack-panel-member-shell member)))
        (run-at-time 0 nil #'+agent-shell-drive-kill shell)))
    (+agent-shell-pstack-panel--advance panel)))

(defun +agent-shell-pstack-panel--stop (member callback)
  "Stop settled candidate MEMBER and every process it started.
Then call CALLBACK while its sandbox's private folders are deleted, a
failure to delete them reported with `message'."
  (if-let* ((sandbox (+agent-shell-pstack-panel-member-sandbox member)))
      (let* ((shell (+agent-shell-pstack-panel-member-shell member))
             (role (+agent-shell-pstack-panel-member-role member))
             (done (lambda (survivors)
                     (when survivors
                       (message "pstack arena: processes %s outlived \
candidate %s" survivors role))
                     (+agent-shell-sandbox-discard
                      sandbox
                      (lambda (error)
                        (when error
                          (message "pstack arena: candidate %s's private \
folders stayed: %s" role error))))
                     (funcall callback))))
        (if (buffer-live-p shell)
            (+agent-shell-drive-stop shell nil done)
          ;; Its adapter is gone, so only the sweep can find what it left.
          (+agent-shell-sandbox-sweep
           sandbox (lambda (survivors)
                     (+agent-shell-sandbox-finish sandbox)
                     (funcall done survivors)))))
    (funcall callback)))

(defun +agent-shell-pstack-panel--halt (panel member)
  "Stop settled candidate MEMBER of PANEL so its tree stops changing.
A candidate without a worktree fails, since there is nothing to capture."
  (setf (+agent-shell-pstack-panel-member-capture member) 'stopping)
  (+agent-shell-pstack-panels--refresh)
  (+agent-shell-pstack-panel--stop
   member
   (lambda ()
     (if-let* ((manifest (+agent-shell-pstack-panel-arena panel))
               ((+agent-shell-pstack-panel-member-sandbox member))
               ((+agent-shell-arena-git-role
                 manifest (+agent-shell-pstack-panel-member-role member))))
         (setf (+agent-shell-pstack-panel-member-capture member) 'stopped)
       (setf (+agent-shell-pstack-panel-member-capture member) 'failed
             (+agent-shell-pstack-panel-member-result member)
             (list :error "The candidate never started.")))
     (+agent-shell-pstack-panel--advance panel)
     (+agent-shell-pstack-panels--refresh))))

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
         (let ((text (+agent-shell-pstack-panel--report-text event))
               (reason (plist-get event :stop-reason)))
           (if (member reason +agent-shell-pstack-panel--failed-stops)
               (+agent-shell-pstack-panel--settle
                panel member 'error text
                (format "The turn ended with stop reason %s." reason))
             (+agent-shell-pstack-panel--settle panel member 'done text)))))
      ('error
       (when (eq (+agent-shell-pstack-panel-member-state member) 'running)
         (+agent-shell-pstack-panel--settle
          panel member 'error
          (+agent-shell-drive-partial-text
           (+agent-shell-pstack-panel-member-shell member))
          (format "The agent reported an error: %s"
                  (or (plist-get event :message) "no message")))))
      ('clean-up
       (+agent-shell-pstack-panel--settle
        panel member 'error nil
        (or (plist-get event :reason)
            "The member's shell ended before it reported."))))
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
  "Start MEMBER of PANEL: a candidate in its sandbox, others read-only."
  (let ((id (+agent-shell-pstack-panel-id panel))
        (role (+agent-shell-pstack-panel-member-role member))
        (sandbox (+agent-shell-pstack-panel-member-sandbox member)))
    (condition-case err
        (let ((shell (+agent-shell-drive-start
                      :config (+agent-shell-pstack-panel-member-agent member)
                      :directory
                      (if sandbox
                          (let ((manifest (+agent-shell-pstack-panel-arena
                                           panel)))
                            (+agent-shell-arena-git-directory
                             manifest (+agent-shell-arena-git-role
                                       manifest role)))
                        (+agent-shell-pstack-panel-cwd panel))
                      :policy (or sandbox 'read-only))))
          (setf (+agent-shell-pstack-panel-member-state member) 'starting
                (+agent-shell-pstack-panel-member-shell member) shell)
          (+agent-shell-drive-watch
           shell 'pstack-panel (cons id role)
           (lambda (event)
             (+agent-shell-pstack-panel--member-event id role event))))
      (error (+agent-shell-pstack-panel--settle
              panel member 'error nil (error-message-string err))))))

(defun +agent-shell-pstack-panel--lead-agent (lead)
  "Return the agent identifier of LEAD's shell, or nil."
  (when (local-variable-p 'agent-shell--state lead)
    (map-nested-elt (buffer-local-value 'agent-shell--state lead)
                    '(:agent-config :identifier))))

(defun +agent-shell-pstack-panel--request (lead session text &optional tool)
  "Start the panel that LEAD requested in TEXT in SESSION.
TOOL names the call LEAD made after TEXT; a new request interrupts LEAD.
Return the panel, or nil when TEXT requests none or one already seen."
  (when-let* ((block (+agent-shell-pstack-panel--block text))
              (id (+agent-shell-pstack-panel--id (or session "") block))
              ((not (+agent-shell-pstack-panel--find id))))
    (let ((panel (condition-case err
                     (+agent-shell-pstack-panel--parse
                      block (+agent-shell-pstack-panel--lead-agent lead))
                   (user-error
                    (+agent-shell-pstack-panel--make
                     :rejection (error-message-string err))))))
      (setf (+agent-shell-pstack-panel-id panel) id
            (+agent-shell-pstack-panel-lead panel) lead)
      (puthash id panel +agent-shell-pstack-panel--panels)
      (when tool
        (setf (+agent-shell-pstack-panel-interrupted panel) tool)
        (+agent-shell-drive-interrupt lead))
      (cond
       ((+agent-shell-pstack-panel-rejection panel)
        (+agent-shell-pstack-panel--finish panel))
       ((+agent-shell-pstack-panel--arena-p panel)
        (setf (+agent-shell-pstack-panel-cwd panel)
              (with-current-buffer lead (agent-shell-cwd)))
        ;; The base must include the edit the interrupt may cut short.
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
      panel)))

;;;; Arenas

(defun +agent-shell-pstack-panel--discard-arena (panel)
  "Remove PANEL's arena from disk; no candidate in it ever ran."
  (when-let* ((manifest (+agent-shell-pstack-panel-arena panel)))
    (setf (+agent-shell-pstack-panel-arena panel) nil)
    (+agent-shell-arena-git-clean
     manifest (lambda (left error)
                (when (or left error)
                  (message "pstack arena %s: cleanup left %s"
                           (+agent-shell-arena-git-manifest-id manifest)
                           (or error (string-join left "; "))))))))

(defun +agent-shell-pstack-panel--fail-preparation (panel reason)
  "End PANEL, whose arena could not be prepared, because of REASON."
  (setf (+agent-shell-pstack-panel-rejection panel) reason)
  (dolist (member (+agent-shell-pstack-panel-members panel))
    (+agent-shell-pstack-panel--settle panel member 'error nil reason))
  (+agent-shell-pstack-panel--discard-arena panel)
  (+agent-shell-pstack-panel--finish panel)
  (+agent-shell-pstack-panels--refresh))

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
Then start the candidates.  PANEL is preparing."
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
      (provision (+agent-shell-pstack-panel--kind-members panel 'candidate)))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-arena-sandbox (manifest name &optional trace)
  "Return a sandbox confining candidate NAME of MANIFEST to its worktree.
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
           :trace trace :deadline +agent-shell-pstack-arena-deadline)))
    (condition-case err
        (+agent-shell-arena-git-write-files
         worktree (plist-get profile :files) sandbox)
      (error (+agent-shell-sandbox-discard sandbox)
             (signal (car err) (cdr err))))
    sandbox))

(defun +agent-shell-pstack-panel--launch (panel)
  "Start PANEL's candidates in their sandboxes and their timer."
  (let ((candidates (+agent-shell-pstack-panel--kind-members panel 'candidate))
        (manifest (+agent-shell-pstack-panel-arena panel)))
    (dolist (member candidates)
      (setf (+agent-shell-pstack-panel-member-sandbox member)
            (+agent-shell-pstack-arena-sandbox
             manifest (+agent-shell-pstack-panel-member-role member))))
    (setf (+agent-shell-pstack-panel-phase panel) 'candidates)
    (+agent-shell-pstack-panel--arm panel (+agent-shell-pstack-panel-timeout
                                           panel))
    (dolist (member candidates)
      (+agent-shell-pstack-panel--start-member panel member))
    (+agent-shell-pstack-panel--advance panel)
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-pstack-panel--resume (lead)
  "Prepare the arenas that waited for LEAD's interrupted turn to end."
  (dolist (panel (+agent-shell-pstack-panel-list))
    (when (and (eq lead (+agent-shell-pstack-panel-lead panel))
               (eq (+agent-shell-pstack-panel-phase panel) 'interrupting))
      (+agent-shell-pstack-panel--prepare panel))))

(defun +agent-shell-pstack-panel-cancel (panel reason &optional state)
  "Stop PANEL, settling its unfinished members as STATE because of REASON.
Settle the current phase's members and every pending member, which can
no longer start, as `error' unless STATE says otherwise.  An arena still
captures what its candidates made and never starts its judge.  An arena
on a host gets a cancel instead; see `+agent-shell-arena-remote-cancel'."
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
       (+agent-shell-pstack-panel--resume lead)
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
  "RET" #'+agent-shell-pstack-panels-visit
  "x" #'+agent-shell-pstack-panels-clean
  "c" #'+agent-shell-pstack-panels-cancel)

(define-derived-mode +agent-shell-pstack-panels-mode special-mode
  "pstack panels"
  "List pstack panels, their members and reports.
\\<+agent-shell-pstack-panels-mode-map>\\[+agent-shell-pstack-panels-visit] visits the report or shell on the current line.
\\[+agent-shell-pstack-panels-clean] cleans up the finished arena on the line.
\\[+agent-shell-pstack-panels-cancel] cancels the unfinished panel on the line."
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
                            manifest)))
            (+agent-shell-pstack-panels--line
             directory "  Arena: " (abbreviate-file-name directory)
             (if (eq (+agent-shell-pstack-panel-phase panel) 'finished)
                 "  (x cleans it up)" ""))
            (when-let* ((remote (+agent-shell-pstack-panel-remote panel)))
              (+agent-shell-pstack-panels--line
               directory "  Host: "
               (+agent-shell-arena-remote-describe remote t)))))
        (dolist (member (+agent-shell-pstack-panel-members panel))
          (let ((report (+agent-shell-pstack-panel-member-report member))
                (shell (+agent-shell-pstack-panel-member-shell member))
                (capture (+agent-shell-pstack-panel-member-capture member))
                (result (+agent-shell-pstack-panel-member-result member)))
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
                   (t (+agent-shell-pstack-panels--name shell))))))
        (insert "\n")
        (put-text-property start (point) '+agent-shell-pstack-panels-panel
                           panel)))
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
  "Visit the report, member shell or lead shell on the current line."
  (interactive)
  (pcase (get-text-property (point) '+agent-shell-pstack-panels-target)
    ((and (pred stringp) file) (find-file-other-window file))
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
    (when (yes-or-no-p (format "Remove arena %s's worktrees, checkouts and \
result refs from %s%s? " id (+agent-shell-arena-git-manifest-toplevel manifest)
                               (if remote " and its host" "")))
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
  "Clean up the finished arena on the current line, after confirmation."
  (interactive)
  (let ((panel (get-text-property (point) '+agent-shell-pstack-panels-panel)))
    (cond
     ((not (and panel (+agent-shell-pstack-panel-arena panel)))
      (user-error "No arena on this line"))
     ((not (eq (+agent-shell-pstack-panel-phase panel) 'finished))
      (user-error "Arena %s is still %s" (+agent-shell-pstack-panel-id panel)
                  (+agent-shell-pstack-panel-phase panel)))
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
