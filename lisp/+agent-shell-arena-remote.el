;;; +agent-shell-arena-remote.el --- pstack arenas on a remote host -*- lexical-binding: t; -*-

;;; Commentary:
;; An arena whose repository profile names a :host runs there.  The laptop
;; snapshots the lead's tree as a local arena does, then submits a job: a
;; JSON record, a git bundle of the base against a prerequisite the host
;; has, and a tarball of the Elisp the runner needs, named by its content
;; hash.  On the host, a batch Emacs started from this file runs the arena
;; with the panel code and no lead, rewriting its status as it goes and
;; writing its results when it ends.  The laptop polls the status, mirrors
;; it into the panel, fetches the results into the lead's repository and
;; sends the lead the follow-up a local arena would.
;;
;; Four records are versioned JSON, each a kind in
;; `+agent-shell-arena-remote--kinds': the job, the runner's status and
;; report, and the laptop's own record of each arena, remote.json in the
;; arena's folder, from which it re-attaches after a restart.  The
;; laptop's phases for an arena are `+agent-shell-arena-remote--transitions'.
;; Calls to a host run one at a time per host; see
;; `+agent-shell-arena-remote--call'.
;;
;; docs/PSTACK-REMOTE-PROTOCOL.md is the protocol; see
;; docs/AGENT-SHELL-PSTACK-ARENA-REMOTE.md for its design.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require '+agent-shell-arena-git)
(require '+agent-shell-sandbox)
(require '+agent-shell-pstack-panel)

(declare-function agent-shell-session-id "agent-shell")
(declare-function +agent-shell-egress-import "+agent-shell-egress")

(defvar agent-shell--state)
(defvar agent-shell-inhibit-system-sleep)
(defvar agent-shell-transcript-file-path-function)
(defvar shell-maker-prompt-before-killing-buffer)

(defgroup +agent-shell-arena-remote nil
  "pstack arenas that run on a remote host."
  :group '+agent-shell-pstack-arena)

(defcustom +agent-shell-arena-remote-poll-interval 30
  "Seconds between status polls of an arena running on a host."
  :type 'number)

(defcustom +agent-shell-arena-remote-max-interval 600
  "Longest wait, in seconds, between polls that keep failing."
  :type 'number)

(defcustom +agent-shell-arena-remote-stale-after 120
  "Seconds without a new heartbeat after which a stopped runner is restarted."
  :type 'number)

(defcustom +agent-shell-arena-remote-unreachable-after 900
  "Seconds without a new heartbeat after which the lead hears of it."
  :type 'number)

(defcustom +agent-shell-arena-remote-wake-timeout 600
  "Seconds a submission waits for a host that is starting to get ready.
That covers starting a stopped Coder workspace, its agent connecting and
its setup writing a readiness file since it booted."
  :type 'number)

(defvar +agent-shell-arena-remote-wake-interval 10
  "Seconds between checks while a host gets ready.")

(defcustom +agent-shell-arena-remote-restarts 2
  "Times the laptop restarts a host's runner that stopped unfinished."
  :type 'natnum)

(defcustom +agent-shell-arena-remote-slack 300
  "Seconds beyond an arena's deadline to request in workspace extensions.
Arena runtime overhead belongs to the panel's shared budget instead."
  :type 'natnum)

(defcustom +agent-shell-arena-remote-ssh-options
  '("-o" "BatchMode=yes" "-o" "ConnectTimeout=15"
    "-o" "ServerAliveInterval=15" "-o" "ServerAliveCountMax=3"
    "-o" "ControlMaster=auto" "-o" "ControlPath=~/.ssh/pstack-arena-%C"
    "-o" "ControlPersist=10m")
  "Options for every ssh call to a host.
One shared connection, a bounded connect, and keepalives that end a call
whose connection died."
  :type '(repeat string))

(defcustom +agent-shell-arena-remote-host-root
  ".local/state/pstack-arena-remote"
  "The runner's state folder on an SSH host, relative to its home folder."
  :type 'string)

(defcustom +agent-shell-arena-remote-ready-file
  "~/.local/share/pstack-arena/ready.json"
  "The readiness file an SSH host's setup writes once its smoke check passes.
A local host's is ready.json in its state folder."
  :type 'string)

(defcustom +agent-shell-arena-remote-coder "coder"
  "The Coder CLI, which extends a workspace's stop time from the laptop."
  :type 'string)

(defcustom +agent-shell-arena-remote-libraries
  '(+agent-shell-arena-remote +agent-shell-pstack-panel +agent-shell-pstack
    +agent-shell-drive +agent-shell-sandbox +agent-shell-arena-git
    +agent-shell-egress)
  "Local libraries a runner loads, shipped to the host with each job."
  :type '(repeat symbol))

(defcustom +agent-shell-arena-remote-files '("+agent-shell-sandbox-launch.sh")
  "Other files on `load-path' a runner needs, such as the Linux launcher."
  :type '(repeat string))

(defcustom +agent-shell-arena-remote-packages '(agent-shell acp shell-maker)
  "Packages whose Elisp sources a runner loads, shipped as this Emacs has them."
  :type '(repeat symbol))

(defconst +agent-shell-arena-remote-minimum-emacs 30
  "Oldest Emacs major version a host may run the runner on.")

(defvar +agent-shell-arena-remote-spawn-function
  #'+agent-shell-arena-remote--spawn
  "Function running one command; see `+agent-shell-arena-remote--spawn'.")

(defvar +agent-shell-arena-remote-heartbeat 10
  "Seconds between a runner's heartbeats.")

(defvar +agent-shell-arena-remote-tick 2
  "Seconds between a runner's checks for a cancel and its deadline.")

(defvar +agent-shell-arena-remote-transcript-segment-bytes 1048576
  "Most bytes a host transcript segment holds.")

(defvar +agent-shell-arena-remote-transcript-segments 8
  "Segments a host keeps of a delegate's transcript, newest last.")

(defvar +agent-shell-arena-remote-transcript-record-bytes 16384
  "Most bytes one encoded transcript record takes, newline included.")

(defvar +agent-shell-arena-remote-transcript-page-bytes 65536
  "Most bytes of whole transcript records one poll brings.")

(defvar +agent-shell-arena-remote-transcript-cache-bytes 8388608
  "Most bytes of transcript the laptop keeps and shows per delegate.")

(defcustom +agent-shell-arena-remote-transcript-interval 5
  "Seconds between polls of a delegate whose transcript is shown."
  :type 'number)

;;;; Records on the wire

(cl-defstruct (+agent-shell-arena-remote-request-member
               (:constructor +agent-shell-arena-remote--make-request-member)
               (:copier nil))
  "One member a job asks for.
KIND is `candidate' or `judge' for an arena, `delegate' for a delegate.
Only the wire decoder turns a version-1 job's candidates and judge into
these.  A status or report member is an observation instead."
  role agent kind focus)

(defconst +agent-shell-arena-remote-wire-version 2
  "Version of the records this code writes.
It reads every version from 1 up to this one, so a laptop that updated
mid-arena still reads an older runner's status and report.")

(define-error '+agent-shell-arena-remote-wire "Malformed pstack arena record")

(cl-defstruct (+agent-shell-arena-remote-job
               (:constructor +agent-shell-arena-remote--make-job)
               (:copier nil))
  "What the laptop asks a host to run.
SHAPE is `arena' or `delegate'.  MEMBERS lists what it runs, each a
`+agent-shell-arena-remote-request-member'; an arena's judge is the one
of kind `judge'.  DELEGATE holds a delegate job's
`+agent-shell-arena-remote-delegate-job', and an arena has none.
PREFIX is the lead's subdirectory, BASE the snapshot commit and
PREREQUISITE the commit its bundle is cut against, nil for a whole
history.  SPARSE holds the lead's sparse-checkout patterns and PROFILE
the repository profile as plain data.  CODE names the runner's code by
content hash.  DEADLINE is how many seconds the runner may take once it
starts.  READY is the host's readiness file as the laptop saw it, and
READY-FILE where the host keeps it.  MEASURE-EGRESS is the laptop panel's
`+agent-shell-pstack-arena-measure-egress', which the host's panel takes."
  id panel brief rubric timeout judge-timeout prefix base
  prerequisite sparse profile repository code deadline ready-file ready
  measure-egress (shape 'arena) members delegate)

(cl-defstruct (+agent-shell-arena-remote-delegate-job
               (:constructor +agent-shell-arena-remote--make-delegate-job)
               (:copier nil))
  "How a delegate job runs: SANDBOX says whether it is confined, and
IDENTITY is the laptop user's `+agent-shell-arena-git-user', who authors
and commits its work."
  sandbox identity)

(cl-defstruct (+agent-shell-arena-remote-cursor
               (:constructor +agent-shell-arena-remote--make-cursor)
               (:copier nil))
  "A place in a delegate's transcript: its EPOCH, SEGMENT and byte OFFSET.
SEQ is the last record before it, or 0."
  epoch segment offset (seq 0))

(cl-defstruct (+agent-shell-arena-remote-transcript
               (:constructor +agent-shell-arena-remote--make-transcript)
               (:copier nil))
  "What a status says of its delegate's transcript: its EPOCH, the
FIRST and LAST segments the host keeps, and the newest record's SEQ."
  epoch first last seq)

(cl-defstruct (+agent-shell-arena-remote-commit
               (:constructor +agent-shell-arena-remote--make-commit)
               (:copier nil))
  "A commit a delegate's branch holds: its OID and SUBJECT."
  oid subject)

(cl-defstruct (+agent-shell-arena-remote-outcome
               (:constructor +agent-shell-arena-remote--make-outcome)
               (:copier nil))
  "What a delegate left on its host, as its report gives it.
BRANCH is its full ref and TIP the commit it points at.  COMMITS lists
`+agent-shell-arena-remote-commit's from the base, oldest first.
WORKSPACE is its worktree on the host.  DIRTY-PATHS names at most ten
of its DIRTY-COUNT uncommitted paths.  FINALIZATION is `none', `empty',
`committed' or `blocked', saying what came of a sandboxed delegate's
one commit, with MESSAGE-ERROR or RETAINED-REASON saying why it was
blocked."
  branch tip commits workspace sandbox dirty-paths (dirty-count 0)
  (finalization 'none) message-error retained-reason)

(defconst +agent-shell-arena-remote-capability-registry
  '((linux-namespaces
     "linux_namespaces"
     :proof "unshare --user --map-root-user --net true succeeds as the \
runner's user"
     :setup "allow unprivileged user namespaces (kernel.unprivileged_userns_clone=1, \
and on Ubuntu 24.04 or newer an AppArmor profile for bwrap or \
kernel.apparmor_restrict_unprivileged_userns=0)")
    (bwrap
     "bwrap"
     :proof "bwrap --unshare-user --unshare-pid --unshare-ipc --unshare-net \
--die-with-parent --ro-bind / / --dev /dev --proc /proc true succeeds"
     :setup "install bubblewrap and record it as ready.json's bwrap tool")
    (slirp4netns
     "slirp4netns"
     :proof "a command in a bwrap network namespace reaches the network \
through slirp4netns --configure"
     :setup "install slirp4netns and record it as ready.json's slirp4netns \
tool")
    (systemd-user
     "systemd_user"
     :proof "systemd-run --user --collect --wait true and systemd-run --user \
--scope --collect true both succeed from a non-interactive ssh session"
     :setup "run a systemd user manager that outlives logins (loginctl \
enable-linger) and reaches ssh sessions through XDG_RUNTIME_DIR")
    (git
     "git"
     :proof "git runs bundle verify, fetch from a bundle, worktree add and \
sparse-checkout in a scratch repository"
     :setup "install git")
    (node-adapters
     "node_adapters"
     :proof "node, and claude-agent-acp and codex-acp in adapters_bin, each \
answer --version"
     :setup "install Node and the ACP adapters from their lock into \
adapters_bin, and record them as ready.json's node, claude_agent_acp and \
codex_acp tools")
    (delegate
     "delegate"
     :proof "setup's delegate smoke check passes: a fixture delegate commits \
on a branch of a scratch checkout and its scope stops"
     :setup "rerun the host's setup with delegate support, or keep delegates \
here with :delegate (:host local)")
    (egress-tshark
     "egress_tshark"
     :proof "tshark captures a sandbox's traffic inside its user namespace"
     :setup "install tshark"))
  "What a host's readiness file may say its setup proved, in order.
Each entry is (NAME KEY :proof PROOF :setup SETUP): KEY names it in
ready.json's capabilities object, PROOF is the check that proves it and
SETUP what to do on a host that lacks it.  A capability's value is the
version of that check that passed, at least 1, or 0 when it ran and
failed; a key that is absent was never checked.")

(cl-defstruct (+agent-shell-arena-remote-capabilities
               (:constructor +agent-shell-arena-remote--make-capabilities)
               (:copier nil))
  "What a host's setup proved it can do, one slot per entry of
`+agent-shell-arena-remote-capability-registry': each the version of the
check that passed, 0 when it failed, or nil when setup never checked."
  linux-namespaces bwrap slirp4netns systemd-user git node-adapters delegate
  egress-tshark)

(cl-defstruct (+agent-shell-arena-remote-sparse
               (:constructor +agent-shell-arena-remote--make-sparse)
               (:copier nil))
  "Sparse-checkout patterns, and whether they are in cone mode."
  cone patterns)

(cl-defstruct (+agent-shell-arena-remote-status
               (:constructor +agent-shell-arena-remote--make-status)
               (:copier nil))
  "What a runner reports while it runs.
PHASE is queued until the host's repository is free, then the panel's
phase.  RESULTS says the results are written.  HEARTBEAT is when the
runner last wrote it, and DEADLINE when it gives up.  LOGINS says how
each agent's host login stood before any member started.  SHAPE is
the job's, and a delegate's TRANSCRIPT says what of its transcript the
host keeps."
  id phase pid heartbeat deadline arena members results error logins
  shape transcript)

(cl-defstruct (+agent-shell-arena-remote-member
               (:constructor +agent-shell-arena-remote--make-member)
               (:copier nil))
  "A member as a status or report gives it.
A status gives only ROLE, AGENT, KIND, STATE, CAPTURE, ERROR, why it
settled as an error, and FAILURE-KIND, the kind of the provider failure
that ended its last turn.  A report adds the member's TEXT, the COMMIT
that holds its result, its diff STAT, and why its capture failed,
FAILURE."
  role agent kind state capture text error commit stat failure failure-kind)

(cl-defstruct (+agent-shell-arena-remote-report
               (:constructor +agent-shell-arena-remote--make-report)
               (:copier nil))
  "A runner's results.  ARENA is the host's arena id, whose refs the
results bundle holds; REJECTION says why the arena never ran.  READY is
the host's readiness file as the runner read it, naming what ran.  A
delegate's DELEGATE is its `+agent-shell-arena-remote-outcome'."
  id arena base rejection members ready shape delegate)

(cl-defstruct (+agent-shell-arena-remote-tool
               (:constructor +agent-shell-arena-remote--make-tool)
               (:copier nil))
  "A program a host's setup installed: its PATH and VERSION."
  path version)

(cl-defstruct (+agent-shell-arena-remote-ready
               (:constructor +agent-shell-arena-remote--make-ready)
               (:copier nil))
  "A host's readiness file, written once its setup passed a smoke check.
EMACS is the Emacs a runner runs on.  ADAPTERS-BIN is the folder holding
the ACP adapters' launchers, and the other slots the tools the Linux
sandbox and the adapters need, each a `+agent-shell-arena-remote-tool'.
CAPABILITIES says what setup proved, a
`+agent-shell-arena-remote-capabilities'."
  written-at emacs adapters-bin claude-agent-acp codex-acp node bwrap unshare
  slirp4netns capabilities)

(cl-defstruct (+agent-shell-arena-remote-host
               (:constructor +agent-shell-arena-remote--make-host)
               (:copier nil))
  "Where a remote arena runs.
KIND is registered in `+agent-shell-arena-remote-host-kinds'; NAME is
its backend's host name or local state folder."
  kind name)

(cl-defstruct (+agent-shell-arena-remote
               (:constructor +agent-shell-arena-remote--make)
               (:copier nil))
  "The laptop's record of an arena running on a host.
ID is the laptop's arena id, which is also the job's, and PANEL the
panel id.  SESSION is the lead's session, by which a restored lead gets
the follow-up.  PHASE moves as `+agent-shell-arena-remote--transitions'
allows.  CANCEL-STATE is `none', `pending' until the host has the marker, then
`sent'.  HEARTBEAT is the last one seen and SEEN when it changed.
EXTENDED is when the workspace's stop time was last moved past.  ARENA
is the host's arena id and EMACS the host's Emacs.

SHAPE is the job's.  CURSOR is where a delegate's transcript continues,
a `+agent-shell-arena-remote-cursor', advanced once the page before it
is in the laptop's cache.

The slots from DIRECTORY to LOGINS are not saved."
  id panel session host phase (cancel-state 'none) submitted fetched delivered
  deadline extended heartbeat seen (restarts 0) arena emacs error
  directory timer (failures 0) runner-phase ready logins (shape 'arena) cursor
  polled)

(defconst +agent-shell-arena-remote--agent-regexp
  "\\`[a-z0-9][a-z0-9-]*\\'"
  "Agent identifiers a record may name.")

(defconst +agent-shell-arena-remote--kinds
  `((job +agent-shell-arena-remote-job +agent-shell-arena-remote--make-job
         (id "id" id) (panel "panel" id)
         (shape "shape" (enum arena delegate)) (brief "brief" text)
         (rubric "rubric" (strings)) (timeout "timeout" natnum)
         (judge-timeout "judge_timeout" (opt natnum))
         (members "members" (list request-member))
         (delegate "delegate" (opt (record delegate-job)))
         (prefix "prefix" text) (base "base" sha)
         (prerequisite "prerequisite" (opt sha))
         (sparse "sparse" (opt (record sparse)))
         (profile "profile" (opt profile)) (repository "repository" text)
         (code "code" sha) (deadline "deadline" natnum)
         (ready-file "ready_file" (opt text))
         (ready "ready" (opt (record ready)))
         (measure-egress "measure_egress" (opt boolean)))
    (request-member +agent-shell-arena-remote-request-member
                    +agent-shell-arena-remote--make-request-member
                    (role "role" id) (agent "agent" agent)
                    (kind "kind" (enum candidate judge delegate))
                    (focus "focus" (opt text)))
    (delegate-job +agent-shell-arena-remote-delegate-job
                  +agent-shell-arena-remote--make-delegate-job
                  (sandbox "sandbox" boolean)
                  (identity "identity" (record identity)))
    (identity +agent-shell-arena-git-user +agent-shell-arena-git--user
              (name "name" text) (email "email" text))
    (sparse +agent-shell-arena-remote-sparse
            +agent-shell-arena-remote--make-sparse
            (cone "cone" boolean) (patterns "patterns" (strings)))
    (status +agent-shell-arena-remote-status
            +agent-shell-arena-remote--make-status
            (id "id" id)
            (phase "phase"
                   (enum queued preparing candidates capturing judging
                         delegating finalizing finished))
            (pid "pid" (opt natnum)) (heartbeat "heartbeat" number)
            (deadline "deadline" (opt number)) (arena "arena" (opt id))
            (members "members" (list member-state))
            (results "results" boolean) (error "error" (opt text))
            (logins "logins" (list login))
            (shape "shape" (opt (enum arena delegate)))
            (transcript "transcript" (opt (record transcript))))
    (transcript +agent-shell-arena-remote-transcript
                +agent-shell-arena-remote--make-transcript
                (epoch "epoch" id) (first "first" natnum)
                (last "last" natnum) (seq "seq" natnum))
    (cursor +agent-shell-arena-remote-cursor
            +agent-shell-arena-remote--make-cursor
            (epoch "epoch" id) (segment "segment" natnum)
            (offset "offset" natnum) (seq "seq" natnum))
    (login +agent-shell-sandbox-login-result
           +agent-shell-sandbox-login-result-create
           (agent "agent" agent) (refreshed "refreshed" boolean)
           (remaining "remaining" (opt number)) (error "error" (opt text)))
    (member-state +agent-shell-arena-remote-member
                  +agent-shell-arena-remote--make-member
                  (role "role" id) (agent "agent" agent)
                  (kind "kind" (enum candidate judge delegate))
                  (state "state" (enum pending starting waiting running done
                                       error timeout))
                  (capture "capture" (opt (enum stopping stopped capturing
                                                captured failed)))
                  (error "error" (opt text))
                  (failure-kind "failure_kind" (opt failure-kind)))
    (report +agent-shell-arena-remote-report
            +agent-shell-arena-remote--make-report
            (id "id" id) (arena "arena" (opt id)) (base "base" sha)
            (rejection "rejection" (opt text))
            (members "members" (list member-report))
            (ready "ready" (opt (record ready)))
            (shape "shape" (opt (enum arena delegate)))
            (delegate "delegate" (opt (record outcome))))
    (outcome +agent-shell-arena-remote-outcome
             +agent-shell-arena-remote--make-outcome
             (branch "branch" text) (tip "tip" (opt sha))
             (commits "commits" (list commit)) (workspace "workspace" text)
             (sandbox "sandbox" boolean) (dirty-paths "dirty_paths" (strings))
             (dirty-count "dirty_count" natnum)
             (finalization "finalization" (enum none empty committed blocked))
             (message-error "message_error" (opt text))
             (retained-reason "retained_reason" (opt text)))
    (commit +agent-shell-arena-remote-commit
            +agent-shell-arena-remote--make-commit
            (oid "oid" sha) (subject "subject" text))
    (member-report +agent-shell-arena-remote-member
                   +agent-shell-arena-remote--make-member
                   (role "role" id) (agent "agent" agent)
                   (kind "kind" (enum candidate judge delegate))
                   (state "state" (enum pending starting waiting running done
                                        error timeout))
                   (capture "capture" (opt (enum stopping stopped capturing
                                               captured failed)))
                   (text "text" (opt text)) (error "error" (opt text))
                   (commit "commit" (opt sha)) (stat "stat" (opt text))
                   (failure "failure" (opt text))
                   (failure-kind "failure_kind" (opt failure-kind)))
    (tool +agent-shell-arena-remote-tool +agent-shell-arena-remote--make-tool
          (path "path" text) (version "version" text))
    (ready +agent-shell-arena-remote-ready
           +agent-shell-arena-remote--make-ready
           (written-at "written_at" (opt text)) (emacs "emacs" (record tool))
           (adapters-bin "adapters_bin" (opt text))
           (claude-agent-acp "claude_agent_acp" (opt (record tool)))
           (codex-acp "codex_acp" (opt (record tool)))
           (node "node" (opt (record tool))) (bwrap "bwrap" (opt (record tool)))
           (unshare "unshare" (opt (record tool)))
           (slirp4netns "slirp4netns" (opt (record tool)))
           (capabilities "capabilities" (opt (record capabilities))))
    (capabilities +agent-shell-arena-remote-capabilities
                  +agent-shell-arena-remote--make-capabilities
                  ,@(mapcar (pcase-lambda (`(,slot ,key . ,_))
                              (list slot key '(opt natnum)))
                            +agent-shell-arena-remote-capability-registry))
    (host +agent-shell-arena-remote-host +agent-shell-arena-remote--make-host
          (kind "kind" host-kind) (name "name" text))
    (record +agent-shell-arena-remote +agent-shell-arena-remote--make
            (id "id" id) (panel "panel" id) (session "session" (opt text))
            (host "host" (record host))
            (phase "phase" (enum preparing submitting remote fetching
                                 finished unreachable))
            (cancel-state "cancel" (enum none pending sent))
            (submitted "submitted" boolean) (fetched "fetched" boolean)
            (delivered "delivered" boolean) (deadline "deadline" (opt number))
            (extended "extended" (opt number)) (heartbeat "heartbeat" (opt number))
            (seen "seen" (opt number)) (restarts "restarts" natnum)
            (arena "arena" (opt id)) (emacs "emacs" (opt text))
            (error "error" (opt text))
            (shape "shape" (opt (enum arena delegate)))
            (cursor "cursor" (opt (record cursor)))))
  "Each record kind: its struct, constructor and fields.
A field is (SLOT KEY TYPE).  TYPE is `text', any string; `id', a name
safe in a path and a ref; `sha', a full commit id; `agent', an agent
identifier read as a symbol; `host-kind', a registered host kind;
`failure-kind', one of `+agent-shell-drive-failure-kinds', an unknown
one read as `provider_error';
`natnum', `number' or `boolean'; (enum
SYMBOL...); (strings); (list KIND) or (record KIND) of another kind;
`profile', a repository profile as plain data; or (opt TYPE), which may be
absent or null.")

(defconst +agent-shell-arena-remote--top-kinds
  '((job t) (status t) (report t) (record t) (ready nil 1))
  "Kinds stored as a file of their own, each (KIND LABELLED VERSION).
Every such file has a version, up to VERSION or else
`+agent-shell-arena-remote-wire-version'; a LABELLED one also names its
kind.  The host's setup writes ready.json in a format of its own.")

(defun +agent-shell-arena-remote--kind (kind)
  "Return the registry entry of record KIND."
  (or (assq kind +agent-shell-arena-remote--kinds)
      (error "Unknown record kind %s" kind)))

(defun +agent-shell-arena-remote--bad (path expected value)
  "Signal that the field at PATH holds VALUE, not EXPECTED."
  (signal '+agent-shell-arena-remote-wire
          (list (format "%s must be %s, not %s" path expected
                        (if (eq value :null) "null"
                          (truncate-string-to-width
                           (format "%S" value) 60 nil nil "..."))))))

(defun +agent-shell-arena-remote--absent-p (value)
  "Return whether a parsed JSON VALUE is absent or null."
  (or (null value) (eq value :null)))

(defun +agent-shell-arena-remote--decode-value (type value path)
  "Return VALUE, parsed from JSON, as TYPE; PATH names it in errors."
  (let ((bad (lambda (expected)
               (+agent-shell-arena-remote--bad path expected value))))
    (pcase type
      (`(opt ,inner)
       (unless (+agent-shell-arena-remote--absent-p value)
         (+agent-shell-arena-remote--decode-value inner value path)))
      ('text (if (stringp value) value (funcall bad "a string")))
      ('id (if (and (stringp value)
                    (string-match-p +agent-shell-arena-git--name-regexp value))
               value
             (funcall bad "a name of letters, digits, _ and -")))
      ('sha (if (and (stringp value)
                     (string-match-p
                      "\\`\\(?:[0-9a-f]\\{40\\}\\|[0-9a-f]\\{64\\}\\)\\'" value))
                value
              (funcall bad "a full hex object id")))
      ('host-kind
       (+agent-shell-arena-remote--decode-value
        (cons 'enum (+agent-shell-arena-git-host-kinds)) value path))
      ('failure-kind
       (if (stringp value) (+agent-shell-drive-failure-kind value)
         (funcall bad "a failure kind")))
      ('agent (if (and (stringp value)
                       (string-match-p +agent-shell-arena-remote--agent-regexp
                                       value))
                  (intern value)
                (funcall bad "an agent identifier")))
      ('natnum (if (natnump value) value (funcall bad "a whole number")))
      ('number (if (numberp value) value (funcall bad "a number")))
      ('boolean (pcase value
                  ('t t) (:false nil) (_ (funcall bad "true or false"))))
      (`(enum . ,symbols)
       (or (and (stringp value) (car (memq (intern value) symbols)))
           (funcall bad (format "one of %s"
                                (mapconcat #'symbol-name symbols ", ")))))
      ('(strings)
       (if (and (listp value) (seq-every-p #'stringp value)) value
         (funcall bad "a list of strings")))
      (`(list ,kind)
       (if (listp value)
           (seq-map-indexed
            (lambda (element index)
              (+agent-shell-arena-remote--decode
               kind element (format "%s[%d]" path index)))
            value)
         (funcall bad "a list")))
      (`(record ,kind) (+agent-shell-arena-remote--decode kind value path))
      ('profile (+agent-shell-arena-remote--decode-profile value path))
      (_ (error "Unknown field type %S" type)))))

(defun +agent-shell-arena-remote--upgrade (kind json)
  "Return parsed JSON of top-level KIND in the newest wire layout.
A version-1 job lists `candidates' and a `judge' agent; they become
`members' of an arena.  Nothing else reads the old layout."
  (if (and (eq kind 'job) (eql (alist-get 'version json) 1))
      (let ((candidates (alist-get 'candidates json))
            (judge (alist-get 'judge json)))
        (unless (listp candidates)
          (+agent-shell-arena-remote--bad "job.candidates" "a list"
                                          candidates))
        (append
         `((shape . "arena")
           (members
            . ,(append
                (mapcar (lambda (candidate)
                          (if (and (listp candidate) (consp (car-safe candidate)))
                              (cons '(kind . "candidate") candidate)
                            candidate))
                        candidates)
                (list `((role . "judge") (agent . ,judge)
                        (kind . "judge"))))))
         (seq-remove (lambda (field)
                       (memq (car-safe field) '(candidates judge shape members)))
                     json)))
    json))

(defun +agent-shell-arena-remote--check-job (job path)
  "Signal unless JOB's members and fields fit its shape; PATH names it."
  (let* ((members (+agent-shell-arena-remote-job-members job))
         (kinds (mapcar #'+agent-shell-arena-remote-request-member-kind members))
         (roles (mapcar #'+agent-shell-arena-remote-request-member-role members))
         (bad (lambda (field expected value)
                (+agent-shell-arena-remote--bad (concat path field) expected
                                                value))))
    (when-let* ((duplicate (seq-find (lambda (role) (member role (cdr (member role roles))))
                                     roles)))
      (funcall bad ".members" "members with unique roles" duplicate))
    (pcase (+agent-shell-arena-remote-job-shape job)
      ('delegate
       (unless (equal kinds '(delegate))
         (funcall bad ".members" "one member of kind delegate" kinds))
       (unless (+agent-shell-arena-remote-job-delegate job)
         (funcall bad ".delegate" "present for a delegate" nil))
       (when-let* ((identity (+agent-shell-arena-remote-delegate-job-identity
                              (+agent-shell-arena-remote-job-delegate job)))
                   (problem (+agent-shell-arena-git-identity-error
                             (+agent-shell-arena-git-user-name identity)
                             (+agent-shell-arena-git-user-email identity))))
         (funcall bad ".delegate.identity" "a valid git identity" problem)))
      ('arena
       (when (+agent-shell-arena-remote-job-delegate job)
         (funcall bad ".delegate" "absent for an arena" "present"))
       (unless (and (= 1 (cl-count 'judge kinds)) (memq 'candidate kinds)
                    (not (memq 'delegate kinds)))
         (funcall bad ".members" "candidates and one judge" kinds))
       (unless (+agent-shell-arena-remote-job-judge-timeout job)
         (funcall bad ".judge_timeout" "present for an arena" nil))))
    job))

(defun +agent-shell-arena-remote--decode (kind json &optional path)
  "Return the record of KIND that parsed JSON holds, or signal why not.
PATH names JSON in errors.  A top-level kind checks its version and
kind, and older versions are upgraded first; fields it does not know
are ignored.  A job's members must fit its shape."
  (pcase-let* ((`(,_ ,_ ,constructor . ,fields)
                (+agent-shell-arena-remote--kind kind))
               (path (or path (symbol-name kind))))
    (unless (and (listp json) (consp (car-safe json)))
      (+agent-shell-arena-remote--bad path "an object" json))
    (when-let* ((top (assq kind +agent-shell-arena-remote--top-kinds)))
      (let ((version (alist-get 'version json))
            (written (alist-get 'kind json))
            (newest (or (nth 2 top) +agent-shell-arena-remote-wire-version)))
        (unless (or (not (nth 1 top)) (equal written (symbol-name kind)))
          (+agent-shell-arena-remote--bad (concat path ".kind")
                                          (format "%S" (symbol-name kind))
                                          written))
        (unless (natnump version)
          (+agent-shell-arena-remote--bad (concat path ".version")
                                          "a whole number" version))
        (unless (<= 1 version newest)
          (signal '+agent-shell-arena-remote-wire
                  (list (format "%s has version %d, but this Emacs reads up \
to %d; update it" path version newest))))
      (setq json (+agent-shell-arena-remote--upgrade kind json))))
    (let ((record
           (apply constructor
                  (mapcan (pcase-lambda (`(,slot ,key ,type))
                            (let ((value (alist-get (intern key) json))
                                  (field (format "%s.%s" path key)))
                              (when (and (+agent-shell-arena-remote--absent-p
                                          value)
                                         (not (memq (car-safe type)
                                                    '(opt strings list)))
                                         (not (equal type '(strings)))
                                         (not (eq type 'boolean)))
                                (+agent-shell-arena-remote--bad field "present"
                                                                value))
                              (list (intern (format ":%s" slot))
                                    (+agent-shell-arena-remote--decode-value
                                     type value field))))
                          fields))))
      (if (eq kind 'job) (+agent-shell-arena-remote--check-job record path)
        record))))

(defun +agent-shell-arena-remote--encode-value (type value)
  "Return VALUE of TYPE as `json-serialize' takes it."
  (pcase type
    (`(opt ,inner) (if (null value) :null
                   (+agent-shell-arena-remote--encode-value inner value)))
    ((or 'text 'id 'sha 'natnum 'number) value)
    ((or 'agent 'host-kind 'failure-kind `(enum . ,_)) (symbol-name value))
    ('boolean (if value t :false))
    ('(strings) (vconcat value))
    (`(list ,kind) (vconcat (mapcar (lambda (element)
                                      (+agent-shell-arena-remote--encode
                                       kind element))
                                    value)))
    (`(record ,kind) (+agent-shell-arena-remote--encode kind value))
    ('profile (+agent-shell-arena-remote--encode-profile value))
    (_ (error "Unknown field type %S" type))))

(defun +agent-shell-arena-remote--encode (kind record)
  "Return RECORD of KIND as `json-serialize' takes it."
  (pcase-let ((`(,_ ,type ,_ . ,fields) (+agent-shell-arena-remote--kind kind)))
    (append
     (when-let* ((top (assq kind +agent-shell-arena-remote--top-kinds)))
       `((version . ,(or (nth 2 top) +agent-shell-arena-remote-wire-version))
         ,@(when (nth 1 top) `((kind . ,(symbol-name kind))))))
     (mapcar (pcase-lambda (`(,slot ,key ,field-type))
               (cons (intern key)
                     (+agent-shell-arena-remote--encode-value
                      field-type (cl-struct-slot-value type slot record))))
             fields))))

(defun +agent-shell-arena-remote-to-json (kind record)
  "Return RECORD of KIND as a JSON string."
  (json-serialize (+agent-shell-arena-remote--encode kind record)))

(defun +agent-shell-arena-remote-from-json (kind string)
  "Return the record of KIND in JSON STRING, or signal why there is none."
  (+agent-shell-arena-remote--decode
   kind
   (condition-case err
       (json-parse-string string :object-type 'alist :array-type 'list
                          :null-object :null :false-object :false)
     (json-error
      (signal '+agent-shell-arena-remote-wire
              (list (format "%s is not valid JSON: %s" kind
                            (error-message-string err))))))))

(defun +agent-shell-arena-remote--write-json (file kind record)
  "Write RECORD of KIND to FILE through a temporary file and a rename."
  (let ((temporary (make-temp-file
                    (expand-file-name (concat "." (file-name-nondirectory file)
                                              "-")
                                      (file-name-directory file)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (concat (+agent-shell-arena-remote-to-json kind record)
                            "\n")
                    nil temporary nil 'silent))
    (rename-file temporary file t)))

(defun +agent-shell-arena-remote--read-json (file kind)
  "Return the record of KIND in FILE, or signal why there is none."
  (+agent-shell-arena-remote-from-json
   kind (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents file))
          (buffer-string))))

;;;; Profiles as plain data

(defconst +agent-shell-arena-remote--placeholders
  '(("@ROOT@" . +agent-shell-sandbox-root)
    ("@TMP@" . +agent-shell-sandbox-tmp)
    ("@CONFIG@" . +agent-shell-sandbox-config))
  "What a stand-in sandbox's folders say, and the host sandbox's accessor.")

(defconst +agent-shell-arena-remote--profile-lists
  '(:shared :private :copy :connect :env)
  "Profile keys whose plain value is a list of strings.")

(defun +agent-shell-arena-remote--stand-in ()
  "Return a sandbox whose folders are placeholders the host fills in."
  (+agent-shell-sandbox--make :root "@ROOT@" :tmp "@TMP@" :config "@CONFIG@"))

(defun +agent-shell-arena-remote--resolve (items sandbox)
  "Return ITEMS with each function called on SANDBOX and its result spliced."
  (mapcan (lambda (item)
            (let ((value (if (functionp item) (funcall item sandbox) item)))
              (if (and (listp value)
                       (not (+agent-shell-sandbox-pattern-p value)))
                  (copy-sequence value)
                (list value))))
          items))

(defun +agent-shell-arena-remote-plain-profile (profile)
  "Return checked PROFILE as plain data a host can take.
Functions are called with a stand-in sandbox whose root, temp and config
folders are @ROOT@, @TMP@ and @CONFIG@, which the host replaces with its
own sandbox's.  The host keys are left out."
  (let ((sandbox (+agent-shell-arena-remote--stand-in))
        (plain nil))
    (cl-loop
     for (key value) on profile by #'cddr
     do (pcase key
          ((or :host :host-directory))
          ((or :shared :private :copy)
           (setq plain (plist-put plain key value)))
          (:files
           (setq plain
                 (plist-put
                  plain key
                  (mapcar (pcase-lambda (`(,path . ,function))
                            (let ((text (funcall function sandbox)))
                              (unless (stringp text)
                                (user-error "Arena profile :files function \
for %s returned %S, not a string" path text))
                              (cons path text)))
                          value))))
          ((or :write :read :connect :env)
           (setq plain (plist-put
                        plain key
                        (+agent-shell-arena-remote--resolve value sandbox))))))
    plain))

(defun +agent-shell-arena-remote--encode-profile (profile)
  "Return plain PROFILE as a JSON object."
  (let ((allowance (lambda (value)
                     (if (+agent-shell-sandbox-pattern-p value)
                         `((regexp . ,(cdr value)))
                       value))))
    (cl-loop
     for (key value) on profile by #'cddr
     collect (cons (intern (substring (symbol-name key) 1))
                   (pcase key
                     (:files (vconcat (mapcar (pcase-lambda (`(,path . ,text))
                                                (vector path text))
                                              value)))
                     ((or :write :read) (vconcat (mapcar allowance value)))
                     (_ (vconcat value)))))))

(defun +agent-shell-arena-remote--decode-profile (json path)
  "Return the plain profile JSON holds; PATH names it in errors."
  (unless (and (listp json) (or (null json) (consp (car-safe json))))
    (+agent-shell-arena-remote--bad path "an object" json))
  (let ((profile nil))
    (pcase-dolist (`(,name . ,value) json)
      (let* ((key (intern (format ":%s" name)))
             (field (format "%s.%s" path name))
             (strings (lambda ()
                        (if (and (listp value) (seq-every-p #'stringp value))
                            value
                          (+agent-shell-arena-remote--bad
                           field "a list of strings" value)))))
        (setq profile
              (plist-put
               profile key
               (pcase key
                 ((pred (lambda (key)
                          (memq key +agent-shell-arena-remote--profile-lists)))
                  (funcall strings))
                 (:files
                  (mapcar (lambda (entry)
                            (pcase entry
                              (`(,(and (pred stringp) path)
                                 ,(and (pred stringp) text))
                               (cons path text))
                              (_ (+agent-shell-arena-remote--bad
                                  field "a list of [path, text] pairs" entry))))
                          (if (listp value) value
                            (+agent-shell-arena-remote--bad field "a list"
                                                            value))))
                 ((or :write :read)
                  (mapcar (lambda (entry)
                            (pcase entry
                              ((pred stringp) entry)
                              (`((regexp . ,(and (pred stringp) re)))
                               (cons 'regexp re))
                              (_ (+agent-shell-arena-remote--bad
                                  field "paths or {\"regexp\": RE} objects"
                                  entry))))
                          (if (listp value) value
                            (+agent-shell-arena-remote--bad field "a list"
                                                            value))))
                 (_ (+agent-shell-arena-remote--bad
                     path "a profile without unknown keys" name)))))))
    profile))

(defun +agent-shell-arena-remote--fill (string sandbox &optional quote)
  "Return STRING with each placeholder replaced by SANDBOX's folder.
With QUOTE, the folders are quoted for a sandbox regex."
  (dolist (placeholder +agent-shell-arena-remote--placeholders string)
    (let ((folder (funcall (cdr placeholder) sandbox)))
      (setq string (string-replace
                    (car placeholder)
                    (if quote (+agent-shell-sandbox-regexp-quote folder) folder)
                    string)))))

(defun +agent-shell-arena-remote--placeholder-p (string)
  "Return whether STRING holds a placeholder."
  (seq-some (lambda (placeholder) (string-search (car placeholder) string))
            +agent-shell-arena-remote--placeholders))

(defun +agent-shell-arena-remote-host-profile (plain)
  "Return PLAIN, a profile from a job, as a profile for the host's sandbox.
A value holding a placeholder becomes a function of the sandbox."
  (let ((later (lambda (value)
                 (cond
                  ((and (+agent-shell-sandbox-pattern-p value)
                        (+agent-shell-arena-remote--placeholder-p (cdr value)))
                   (lambda (sandbox)
                     (cons 'regexp (+agent-shell-arena-remote--fill
                                    (cdr value) sandbox t))))
                  ((and (stringp value)
                        (+agent-shell-arena-remote--placeholder-p value))
                   (lambda (sandbox)
                     (+agent-shell-arena-remote--fill value sandbox)))
                  (t value)))))
    (cl-loop
     for (key value) on plain by #'cddr
     append (list key
                  (pcase key
                    ((or :shared :private :copy) value)
                    (:files
                     (mapcar (pcase-lambda (`(,path . ,text))
                               (cons path (lambda (sandbox)
                                            (+agent-shell-arena-remote--fill
                                             text sandbox))))
                             value))
                    ;; A profile's :env is strings or one function.
                    (:env
                     (if (seq-some #'+agent-shell-arena-remote--placeholder-p
                                   value)
                         (lambda (sandbox)
                           (mapcar (lambda (entry)
                                     (+agent-shell-arena-remote--fill
                                      entry sandbox))
                                   value))
                       value))
                    (_ (mapcar later value)))))))

;;;; Hosts

(defun +agent-shell-arena-remote-parse-host (value)
  "Return the host a profile's :host VALUE names, or signal `user-error'."
  (unless (+agent-shell-arena-git-host-p value)
    (user-error "Arena host must be %s, not %S"
                (+agent-shell-arena-git-host-description) value))
  (+agent-shell-arena-remote--make-host
   :kind (car value)
   :name (if (plist-get (alist-get (car value)
                                   +agent-shell-arena-remote-host-kinds) :local)
             (file-name-as-directory (expand-file-name (cdr value)))
           (cdr value))))

(defun +agent-shell-arena-remote--host-key (host)
  "Return the key that groups calls to HOST."
  (format "%s:%s" (+agent-shell-arena-remote-host-kind host)
          (+agent-shell-arena-remote-host-name host)))

(defun +agent-shell-arena-remote--host-option (host option)
  "Return OPTION from HOST's registered kind."
  (plist-get (alist-get (+agent-shell-arena-remote-host-kind host)
                        +agent-shell-arena-remote-host-kinds)
             option))

(defun +agent-shell-arena-remote--coder-label (name)
  "Return the message label for Coder workspace NAME."
  (format "Coder workspace %s" name))

(defun +agent-shell-arena-remote--ssh-label (name)
  "Return the message label for SSH host NAME."
  (format "SSH host %s" name))

(defun +agent-shell-arena-remote--local-label (name)
  "Return the message label for local state directory NAME."
  (format "local host %s" (abbreviate-file-name (directory-file-name name))))

(defun +agent-shell-arena-remote--host-label (host)
  "Return HOST as a reader would name it."
  (funcall (+agent-shell-arena-remote--host-option host :label)
           (+agent-shell-arena-remote-host-name host)))

(defun +agent-shell-arena-remote--ssh-exec (name args)
  "Return the local SSH command running ARGS on host NAME."
  (append (list "ssh") +agent-shell-arena-remote-ssh-options
          (list name "--" (+agent-shell-arena-remote--shell-command args))))

(defun +agent-shell-arena-remote--coder-exec (name args)
  "Return the local SSH command running ARGS on Coder workspace NAME."
  (+agent-shell-arena-remote--ssh-exec (concat "coder." name) args))

(defun +agent-shell-arena-remote--local-exec (_name args)
  "Return the local shell command running ARGS."
  (cons "/bin/sh" (cdr args)))

(defun +agent-shell-arena-remote--command (host script args)
  "Return (DIRECTORY . COMMAND) running sh SCRIPT with ARGS on HOST.
The script runs in HOST's state folder."
  (let* ((name (+agent-shell-arena-remote-host-name host))
         (directory (if (+agent-shell-arena-remote--host-option host :local)
                        (progn (make-directory name t) name)
                      (expand-file-name "~/"))))
    (cons directory
          (funcall (+agent-shell-arena-remote--host-option host :exec)
                   name (append (list "sh" "-c" script "sh") args)))))

(defun +agent-shell-arena-remote--shell-command (args)
  "Return the shell command running ARGS in a remote host's state folder."
  (let ((root (shell-quote-argument +agent-shell-arena-remote-host-root)))
    (format "mkdir -p %s && cd %s && exec %s" root root
            (mapconcat #'shell-quote-argument args " "))))

(defun +agent-shell-arena-remote-remote-command (script args)
  "Return the shell command an SSH host runs for SCRIPT with ARGS."
  (+agent-shell-arena-remote--shell-command
   (append (list "sh" "-c" script "sh") args)))

;;;; Transport

(defun +agent-shell-arena-remote--spawn (directory command input output timeout
                                                   done)
  "Run COMMAND in DIRECTORY, then call DONE with (STATUS STDOUT STDERR).
INPUT, when non-nil, is a file for standard input, and OUTPUT a file for
standard output, which STDOUT then lacks.  STATUS is the exit status, or
`timeout' when the command ran longer than TIMEOUT seconds and was killed."
  (let* ((default-directory (file-name-as-directory directory))
         (process-environment
          (append (list (concat "PA_REMOTE_IN=" (or input "/dev/null"))
                        (concat "PA_REMOTE_OUT=" (or output "/dev/null")))
                  process-environment))
         (out (generate-new-buffer " *pstack-arena-remote*" t))
         (err (generate-new-buffer " *pstack-arena-remote-stderr*" t))
         (stderr (make-pipe-process :name "pstack-arena-remote-stderr"
                                    :buffer err :noquery t :sentinel #'ignore))
         (timer nil)
         (finished nil)
         (finish (lambda (status)
                   (unless finished
                     (setq finished t)
                     (when timer (cancel-timer timer))
                     (dotimes (_ 50)
                       (when (process-live-p stderr)
                         (accept-process-output stderr 0.01)))
                     (let ((stdout (with-current-buffer out (buffer-string)))
                           (stderr-text (with-current-buffer err
                                          (string-trim (buffer-string)))))
                       (delete-process stderr)
                       (kill-buffer out)
                       (kill-buffer err)
                       (funcall done status stdout stderr-text)))))
         (process
          (make-process
           :name "pstack-arena-remote" :buffer out :stderr stderr
           :command (append (list "/bin/sh" "-c"
                                  (concat "exec \"$@\" <\"$PA_REMOTE_IN\""
                                          (when output " >\"$PA_REMOTE_OUT\""))
                                  "sh")
                            command)
           :coding 'utf-8-unix :connection-type 'pipe :noquery t
           :sentinel (lambda (process _event)
                       (unless (process-live-p process)
                         (funcall finish (process-exit-status process)))))))
    (setq timer (run-at-time timeout nil
                             (lambda ()
                               (when (process-live-p process)
                                 (funcall finish 'timeout)
                                 (delete-process process)))))
    process))

(cl-defstruct (+agent-shell-arena-remote--call
               (:constructor +agent-shell-arena-remote--make-call)
               (:copier nil))
  "A call waiting to run on a host."
  host script args input output timeout callback)

(defvar +agent-shell-arena-remote--lines (make-hash-table :test #'equal)
  "Map a host key to (BUSY . WAITING): whether a call to it is in flight,
and the calls waiting, oldest first.")

(cl-defun +agent-shell-arena-remote--call (host script args
                                                &key input output (timeout 120)
                                                callback)
  "Run sh SCRIPT with ARGS on HOST, after the calls to HOST already queued.
One call to a host is in flight at a time, so polls never overlap and a
submission never races a cancel.  INPUT and OUTPUT are local files for
the command's standard input and output.  CALLBACK gets the output and
nil, or nil and a message, when the command exits nonzero or runs longer
than TIMEOUT seconds."
  (let* ((key (+agent-shell-arena-remote--host-key host))
         (line (or (gethash key +agent-shell-arena-remote--lines)
                   (puthash key (list nil) +agent-shell-arena-remote--lines))))
    (setcdr line (append (cdr line)
                         (list (+agent-shell-arena-remote--make-call
                                :host host :script script :args args
                                :input input :output output :timeout timeout
                                :callback callback))))
    (+agent-shell-arena-remote--pump key)))

(defun +agent-shell-arena-remote--pump (key)
  "Start the oldest call waiting for the host KEY names, unless one runs."
  (let ((line (gethash key +agent-shell-arena-remote--lines)))
    (when (and line (not (car line)) (cdr line))
      (let* ((call (pop (cdr line)))
             (host (+agent-shell-arena-remote--call-host call))
             (command (+agent-shell-arena-remote--command
                       host (+agent-shell-arena-remote--call-script call)
                       (+agent-shell-arena-remote--call-args call)))
             (label (+agent-shell-arena-remote--host-label host))
             (done
              (lambda (status stdout stderr)
                (setcar line nil)
                (unwind-protect
                    (funcall (+agent-shell-arena-remote--call-callback call)
                             (and (eql status 0) stdout)
                             (cond
                              ((eql status 0) nil)
                              ((eq status 'timeout)
                               (format "%s did not answer within %d seconds"
                                       label
                                       (+agent-shell-arena-remote--call-timeout
                                        call)))
                              ((string-empty-p stderr)
                               (format "%s: exit status %s" label status))
                              (t (format "%s: %s" label stderr))))
                  (+agent-shell-arena-remote--pump key)))))
        (setcar line t)
        (condition-case err
            (funcall +agent-shell-arena-remote-spawn-function
                     (car command) (cdr command)
                     (+agent-shell-arena-remote--call-input call)
                     (+agent-shell-arena-remote--call-output call)
                     (+agent-shell-arena-remote--call-timeout call) done)
          (error (run-at-time 0 nil done 127 "" (error-message-string err))))))))

(defun +agent-shell-arena-remote--delay (failures)
  "Return the seconds to wait before the next poll after FAILURES in a row."
  (min +agent-shell-arena-remote-max-interval
       (* +agent-shell-arena-remote-poll-interval (expt 2 (min failures 10)))))

(defun +agent-shell-arena-remote--local (directory command callback)
  "Run local COMMAND in DIRECTORY; call CALLBACK with output and nil, or nil
and a message."
  (+agent-shell-arena-remote--spawn
   directory command nil nil 600
   (lambda (status stdout stderr)
     (funcall callback (and (eql status 0) stdout)
              (unless (eql status 0)
                (format "%s: %s" (car command)
                        (if (string-empty-p stderr)
                            (format "exit status %s" status)
                          stderr)))))))

(defun +agent-shell-arena-remote--coder (args timeout callback)
  "Run the Coder CLI with ARGS here, for at most TIMEOUT seconds.
Call CALLBACK with its output and nil, or nil and a message."
  (funcall +agent-shell-arena-remote-spawn-function
           (expand-file-name "~/") (cons +agent-shell-arena-remote-coder args)
           nil nil timeout
           (lambda (status stdout stderr)
             (if (eql status 0) (funcall callback stdout nil)
               (funcall callback nil
                        (format "coder %s failed: %s" (string-join args " ")
                                (cond ((eq status 'timeout)
                                       (format "no answer within %s seconds"
                                               timeout))
                                      ((string-empty-p stderr)
                                       (format "exit status %s" status))
                                      (t stderr))))))))

(defun +agent-shell-arena-remote--coder-extend (name seconds callback)
  "Extend Coder workspace NAME by SECONDS, then call CALLBACK with an error."
  (+agent-shell-arena-remote--coder
   (list "schedule" "extend" name (format "%dm" (ceiling seconds 60)))
   60 (lambda (_output error) (funcall callback error))))

(defun +agent-shell-arena-remote--extend (host seconds callback)
  "Move HOST's stop time to at least SECONDS from now, then call CALLBACK.
CALLBACK gets nil, or a message saying why the extension failed."
  (if-let* ((extend (+agent-shell-arena-remote--host-option host :extend)))
      (funcall extend (+agent-shell-arena-remote-host-name host) seconds callback)
    (run-at-time 0 nil callback nil)))

(defun +agent-shell-arena-remote--workspace (output name)
  "Return (STATUS . AGENT) for workspace NAME in coder list -o json OUTPUT.
STATUS is its latest build's status, such as \"running\" or \"stopped\",
and AGENT its first agent's status, such as \"connected\".  Return nil
when OUTPUT lists no such workspace."
  (when-let* ((workspace
               (seq-find (lambda (workspace)
                           (member name (list (alist-get 'name workspace)
                                              (format "%s/%s"
                                                      (alist-get 'owner_name
                                                                 workspace)
                                                      (alist-get 'name
                                                                 workspace)))))
                         (json-parse-string output :object-type 'alist
                                            :array-type 'list
                                            :null-object nil))))
    (let ((build (alist-get 'latest_build workspace)))
      (cons (alist-get 'status build)
            (seq-some (lambda (resource)
                        (seq-some (lambda (agent) (alist-get 'status agent))
                                  (alist-get 'agents resource)))
                      (alist-get 'resources build))))))

;;;; Host scripts

(defconst +agent-shell-arena-remote--repo
  "repo=$1; case $repo in \"~/\"*) repo=$HOME/${repo#\"~/\"};; esac\n"
  "Script lines setting repo to the checkout named by the first argument.")

(defconst +agent-shell-arena-remote--probe-script
  (concat
   +agent-shell-arena-remote--repo
   "top=$(git -C \"$repo\" rev-parse --show-toplevel 2>/dev/null) || top=
tip=
if [ -n \"$top\" ]; then
  tip=$(git -C \"$top\" rev-parse --verify -q 'refs/remotes/origin/HEAD^{commit}' \
2>/dev/null) || tip=$(git -C \"$top\" rev-parse --verify -q 'HEAD^{commit}' \
2>/dev/null) || tip=
fi
code=0; [ -d \"code/$2\" ] && code=1
ready=$3; case $ready in \"~/\"*) ready=$HOME/${ready#\"~/\"};; esac
boot=$(awk '$1 == \"btime\" {print $2}' /proc/stat 2>/dev/null) || boot=
printf 'pstack-arena-probe 1\\ntoplevel %s\\ntip %s\\ncode %s\\nready %s\\nboot %s\\n' \
\"$top\" \"$tip\" \"$code\" \"$ready\" \"$boot\"
cat \"$ready\" 2>/dev/null || true
")
  "Report the checkout's top level, its upstream tip, whether the code with
hash $2 is there, the readiness file $3, by its absolute name, when the
host booted, as /proc/stat says, and the readiness file's contents.")

(defconst +agent-shell-arena-remote--launch
  "hash=$(cat \"$job/code-hash\")
code=$PWD/code/$hash
set -- \"$emacs\" -Q --batch -L \"$code\" -l \"$code/+agent-shell-arena-remote.el\" \
-f +agent-shell-arena-remote-runner-batch \"$PWD/$job\"
case $launcher in
  systemd)
    systemd-run --user --quiet --collect --unit \"pstack-arena-$id-$(date +%s)\" \
--property=MemoryMax=50% --property=OOMPolicy=continue \
--working-directory \"$PWD\" --setenv=PATH=\"$PATH\" \
--setenv=PSTACK_ARENA_LAUNCHER=systemd \"$@\" >\"$job/launch.log\" 2>&1 ||
      { echo \"pstack-arena-submit failed $(tail -n 1 \"$job/launch.log\")\"; exit 0; } ;;
  *) PSTACK_ARENA_LAUNCHER=nohup nohup \"$@\" >\"$job/runner.log\" 2>&1 </dev/null & ;;
esac
touch \"$job/started\"
echo \"pstack-arena-submit started\"
"
  "Script lines starting the runner for $job with $emacs by $launcher.
Under systemd the runner's unit, which holds its candidates too, may use
half the host's memory.  The kernel reclaims page cache at that limit
and kills the largest process past it, a candidate's build rather than
the runner, which goes on to report that candidate as failed.")

(defconst +agent-shell-arena-remote--submit-script
  (concat
   "id=$1; emacs=$3; launcher=$4; shift
"
   +agent-shell-arena-remote--repo
   "job=jobs/$id
mkdir -p \"$job\" code
if [ -f \"$job/started\" ]; then cat >/dev/null; echo \"pstack-arena-submit started\"; exit 0; fi
tar -xf - -C \"$job\" || { echo \"pstack-arena-submit failed the upload did not unpack\"; exit 0; }
hash=$(cat \"$job/code-hash\")
if [ ! -d \"code/$hash\" ]; then
  [ -f \"$job/code.tar\" ] || { echo \"pstack-arena-submit code\"; exit 0; }
  tmp=$(mktemp -d \"code/.$hash.XXXXXX\") && tar -xf \"$job/code.tar\" -C \"$tmp\" &&
    { mv \"$tmp\" \"code/$hash\" 2>/dev/null || rm -rf \"$tmp\"; }
  [ -d \"code/$hash\" ] || { echo \"pstack-arena-submit failed the runner code did not unpack\"; exit 0; }
fi
rm -f \"$job/code.tar\"
g() { git -C \"$repo\" -c core.hooksPath=/dev/null \"$@\"; }
prereq=$(cat \"$job/prerequisite\" 2>/dev/null)
bundle=$PWD/$job/base.bundle
if ! g bundle verify -q \"$bundle\" >/dev/null 2>&1; then
  [ -n \"$prereq\" ] && g fetch -q origin \"$prereq\" >/dev/null 2>&1
  if ! g bundle verify -q \"$bundle\" >/dev/null 2>\"$job/verify.log\"; then
    echo \"pstack-arena-submit missing $prereq\"; exit 0
  fi
fi
g fetch -q --no-tags --no-write-fetch-head \"$bundle\" \
\"+refs/pstack-arena/$id/base:refs/pstack-arena/$id/base\" 2>\"$job/fetch.log\" ||
  { echo \"pstack-arena-submit failed $(tail -n 1 \"$job/fetch.log\")\"; exit 0; }
"
   +agent-shell-arena-remote--launch)
  "Unpack the upload on standard input as job $1 for checkout $2, fetch its
base, and start the runner with Emacs $3 by launcher $4.  A job already
started answers as started, so a resent submission is harmless.")

(defconst +agent-shell-arena-remote--resume-script
  (concat
   "id=$1; emacs=$2; launcher=$3; job=jobs/$id
[ -f \"$job/started\" ] || { echo \"pstack-arena-submit failed no such job\"; exit 0; }
pid=$(cat \"$job/pid\" 2>/dev/null) && [ -n \"$pid\" ] && kill -0 \"$pid\" 2>/dev/null &&
  { echo \"pstack-arena-submit started\"; exit 0; }
"
   +agent-shell-arena-remote--launch)
  "Start job $1's runner again with Emacs $2 by launcher $3 unless it runs.")

(defconst +agent-shell-arena-remote--poll-script
  "job=jobs/$1
[ -f \"$job/started\" ] || { echo \"pstack-arena-poll none\"; exit 0; }
[ \"$2\" = cancel ] && printf '%s\\n' \"$3\" >\"$job/cancel\"
hash=$(cat \"$job/code-hash\" 2>/dev/null)
if [ \"${4:--}\" != - ] && [ -n \"$5\" ] && [ -n \"$hash\" ] && [ -d \"code/$hash\" ] &&
  out=$(\"$5\" -Q --batch -L \"code/$hash\" -l \"code/$hash/+agent-shell-arena-remote.el\" \
-f +agent-shell-arena-remote-poll-batch \"$PWD/$job\" \"$4\" 2>/dev/null); then
  printf '%s\\n' \"$out\"; exit 0
fi
alive=0; pid=$(cat \"$job/pid\" 2>/dev/null) && [ -n \"$pid\" ] && kill -0 \"$pid\" 2>/dev/null && alive=1
cancel=0; [ -f \"$job/cancel\" ] && cancel=1
echo \"pstack-arena-poll alive=$alive cancel=$cancel\"
cat \"$job/status.json\" 2>/dev/null || true
"
  "Leave a cancel marker holding reason $3 when $2 is cancel, then report
whether job $1's runner lives, whether it has a marker, and its status.
With a transcript cursor $4 other than -, a short batch Emacs $5 from the
job's code answers instead with the version-2 envelope, which adds the
transcript's next page; when it cannot, the plain answer follows.")

(defconst +agent-shell-arena-remote--fetch-script
  "cd \"jobs/$1\" || exit 1
if [ -f results.files ]; then exec tar -cf - -T results.files; fi
if [ -f results.bundle ]; then exec tar -cf - report.json results.bundle; fi
exec tar -cf - report.json
"
  "Write job $1's report and results bundle as a tar stream.
A measured arena's runner lists them in results.files with the egress
records.")

(defconst +agent-shell-arena-remote--clean-script
  "job=jobs/$1
[ -d \"$job\" ] || { echo \"pstack-arena-clean done\"; exit 0; }
alive() { pid=$(cat \"$job/pid\" 2>/dev/null) && [ -n \"$pid\" ] && kill -0 \"$pid\" 2>/dev/null; }
# A finished runner takes a few seconds to exit.
if alive && grep -q '\"phase\":\"finished\"' \"$job/status.json\" 2>/dev/null; then
  i=0; while alive && [ $i -lt 20 ]; do sleep 1; i=$((i+1)); done
fi
alive && { echo \"pstack-arena-clean running\"; exit 0; }
hash=$(cat \"$job/code-hash\" 2>/dev/null)
if [ -n \"$hash\" ] && [ -d \"code/$hash\" ] && [ -f \"$job/job.json\" ]; then
  \"$2\" -Q --batch -L \"code/$hash\" -l \"code/$hash/+agent-shell-arena-remote.el\" \
-f +agent-shell-arena-remote-runner-clean-batch \"$PWD/$job\" >\"$job/clean.log\" 2>&1 ||
    { echo \"pstack-arena-clean failed $(tail -n 1 \"$job/clean.log\")\"; exit 0; }
fi
rm -rf \"$job\"
echo \"pstack-arena-clean done\"
"
  "Remove job $1's arena, base ref and folder with Emacs $2, unless it runs.")

(defun +agent-shell-arena-remote--answer (output prefix)
  "Return the words after PREFIX on OUTPUT's last line starting with it."
  (when output
    (when-let* ((line (seq-find (lambda (line) (string-prefix-p prefix line))
                                (reverse (split-string output "\n" t)))))
      (string-trim (substring line (length prefix))))))

(defun +agent-shell-arena-remote--launcher (host)
  "Return how HOST starts a runner that outlives the call: systemd or nohup."
  (if (+agent-shell-arena-remote--host-option host :local) "nohup" "systemd"))

;;;; Laptop phases

(defconst +agent-shell-arena-remote--transitions
  '((preparing submitting finished)
    (submitting remote finished)
    (remote fetching unreachable finished)
    (fetching finished remote unreachable)
    (unreachable remote)
    (finished))
  "The phases a remote arena may move to from each phase on the laptop.
It prepares the base, submits the job, waits while the host runs it,
fetches the results and finishes.  A host that stops answering makes it
unreachable until the lead re-attaches it.")

(defvar +agent-shell-arena-remote--arenas (make-hash-table :test #'equal)
  "Map an arena id to the laptop's record of a remote arena.")

(defun +agent-shell-arena-remote--panel (remote)
  "Return REMOTE's panel, or nil."
  (+agent-shell-pstack-panel--find (+agent-shell-arena-remote-panel remote)))

(defun +agent-shell-arena-remote--record-file (directory)
  "Return the file holding the record of the remote arena in DIRECTORY."
  (expand-file-name "remote.json" directory))

(defun +agent-shell-arena-remote--save (remote)
  "Write REMOTE to its arena folder, if that folder still exists."
  (let ((directory (+agent-shell-arena-remote-directory remote)))
    (when (and directory (file-directory-p directory))
      (+agent-shell-arena-remote--write-json
       (+agent-shell-arena-remote--record-file directory) 'record remote))))

(defun +agent-shell-arena-remote--move (remote phase)
  "Move REMOTE to PHASE, save it, and show it in its panel.
Signal when the move is not one `+agent-shell-arena-remote--transitions'
allows.  The panel's own `finished' is left to
`+agent-shell-pstack-panel--finish'."
  (let ((from (+agent-shell-arena-remote-phase remote)))
    (unless (memq phase (cdr (assq from +agent-shell-arena-remote--transitions)))
      (error "Remote arena %s cannot move from %s to %s"
             (+agent-shell-arena-remote-id remote) from phase))
    (setf (+agent-shell-arena-remote-phase remote) phase)
    (+agent-shell-arena-remote--save remote)
    (when-let* ((panel (+agent-shell-arena-remote--panel remote))
                ((not (eq phase 'finished))))
      (setf (+agent-shell-pstack-panel-phase panel) phase))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-arena-remote-describe (remote &optional detail)
  "Return where REMOTE runs and what ran it, once its report says.
With DETAIL, add its host's phase, last heartbeat and last error."
  (concat
   (+agent-shell-arena-remote--host-label (+agent-shell-arena-remote-host
                                           remote))
   (let ((ran (+agent-shell-arena-remote--ready-text
               (+agent-shell-arena-remote-ready remote))))
     (unless (string-empty-p ran) (format " (%s)" ran)))
   (let ((logins (+agent-shell-arena-remote--logins-text
                  (+agent-shell-arena-remote-logins remote))))
     (unless (string-empty-p logins) (format ", %s" logins)))
   (when detail
     (concat
      (when-let* ((phase (+agent-shell-arena-remote-runner-phase remote)))
        (format ", %s there" phase))
      (when-let* ((seen (+agent-shell-arena-remote-seen remote)))
        (format ", heard from %s" (format-time-string "%H:%M:%S" seen)))
      (when-let* ((error (+agent-shell-arena-remote-error remote)))
        (format ", last error: %s" error))))))

(defun +agent-shell-arena-remote-recorded-p (manifest)
  "Return whether MANIFEST's arena ran on a host."
  (file-exists-p (+agent-shell-arena-remote--record-file
                  (+agent-shell-arena-git-manifest-directory manifest))))

;;;; Base and code

(defun +agent-shell-arena-remote--base-ref (manifest)
  "Return the ref that holds MANIFEST's base and keeps it alive."
  (+agent-shell-arena-git-ref manifest "base"))

(defun +agent-shell-arena-remote--git (manifest args callback &optional input)
  "Run git ARGS in MANIFEST's repository; call CALLBACK with trimmed
output and nil, or nil and a message."
  (+agent-shell-arena-git-command
   manifest args
   (lambda (output error)
     (funcall callback (and output (string-trim output)) error))
   input))

(defun +agent-shell-arena-remote--upstream (manifest callback)
  "Call CALLBACK with the commit MANIFEST's repository tracks upstream, or nil."
  (+agent-shell-arena-remote--git
   manifest '("rev-parse" "--verify" "-q" "refs/remotes/origin/HEAD^{commit}")
   (lambda (commit _)
     (if commit (funcall callback commit)
       (+agent-shell-arena-remote--git
        manifest '("rev-parse" "--verify" "-q" "@{upstream}^{commit}")
        (lambda (commit _) (funcall callback commit)))))))

(defun +agent-shell-arena-remote-prerequisites (manifest tip callback)
  "Call CALLBACK with the commits a bundle of MANIFEST's base may require.
They come in the order to try them.  First the merge base with TIP, the
host's upstream tip, when this repository has TIP; then the merge base
with this repository's own upstream, which the host can fetch from
origin.  With neither, the list is (nil): a bundle of the whole history."
  (let ((base (+agent-shell-arena-git-manifest-base manifest)))
    (cl-labels
        ((merge-base (commit next)
           (if (null commit) (funcall next nil)
             (+agent-shell-arena-remote--git
              manifest (list "merge-base" base commit)
              (lambda (output _) (funcall next output)))))
         (finish (found)
           (funcall callback (or (delete-dups (delq nil found)) (list nil)))))
      (+agent-shell-arena-remote--git
       manifest (list "cat-file" "-e" (concat (or tip "") "^{commit}"))
       (lambda (_ missing)
         (merge-base
          (and tip (not (string-empty-p tip)) (not missing) tip)
          (lambda (with-tip)
            (+agent-shell-arena-remote--upstream
             manifest
             (lambda (upstream)
               (merge-base upstream
                           (lambda (with-upstream)
                             (finish (list with-tip with-upstream)))))))))))))

(defun +agent-shell-arena-remote--lfs-paths (manifest prerequisite callback)
  "Call CALLBACK with the Git LFS paths MANIFEST's base changes after
PREREQUISITE, or after nothing when it is nil, and an error or nil."
  (let ((diff (lambda (from)
                (+agent-shell-arena-remote--git
                 manifest
                 (list "diff" "--name-only" "-z" from
                       (+agent-shell-arena-git-manifest-base manifest)
                       "--" ":(attr:filter=lfs)")
                 (lambda (output error)
                   (funcall callback (and output (split-string output "\0" t))
                            error))))))
    (if prerequisite (funcall diff prerequisite)
      (+agent-shell-arena-remote--git
       manifest '("hash-object" "-t" "tree" "--stdin")
       (lambda (empty error)
         (if error (funcall callback nil error) (funcall diff empty)))
       ""))))

(defun +agent-shell-arena-remote-bundle (manifest prerequisite file callback)
  "Bundle MANIFEST's base ref against PREREQUISITE into FILE.
PREREQUISITE nil bundles the whole history.  A base that changes Git LFS
files is refused, since a bundle carries only their pointers.  Call
CALLBACK with FILE and nil, or nil and a message."
  (+agent-shell-arena-remote--lfs-paths
   manifest prerequisite
   (lambda (lfs error)
     (cond
      (error (funcall callback nil error))
      (lfs (funcall callback nil
                    (format "The base changes Git LFS files the host lacks \
the contents of, which a bundle cannot carry: %s.  Push them first, or run \
the arena locally." (string-join lfs ", "))))
      (t (ignore-errors (delete-file file))
         (+agent-shell-arena-remote--git
          manifest
          (append (list "bundle" "create" "-q" file
                        (+agent-shell-arena-remote--base-ref manifest))
                  (when prerequisite (list (concat "^" prerequisite))))
          (lambda (_ error)
            (funcall callback (unless error file) error))))))))

(defun +agent-shell-arena-remote--code-files ()
  "Return the runner's code as an alist of file name to true name, sorted."
  (sort
   (append
    (mapcar (lambda (feature)
              (let ((file (or (locate-library (format "%s.el" feature) t)
                              (error "No source for %s" feature))))
                (cons (file-name-nondirectory file) (file-truename file))))
            +agent-shell-arena-remote-libraries)
    (mapcar (lambda (name)
              (cons name (file-truename
                          (or (locate-file name load-path)
                              (error "No %s on `load-path'" name)))))
            +agent-shell-arena-remote-files)
    (mapcan (lambda (package)
              (let ((directory
                     (file-name-directory
                      (or (locate-library (format "%s.el" package) t)
                          (error "No source for package %s" package)))))
                (mapcar (lambda (file)
                          (cons (file-name-nondirectory file)
                                (file-truename file)))
                        (seq-remove
                         (lambda (file)
                           (string-match-p "-\\(?:autoloads\\|pkg\\)\\.el\\'"
                                           file))
                         (directory-files directory t "\\.el\\'")))))
            +agent-shell-arena-remote-packages))
   :key #'car))

(defun +agent-shell-arena-remote--code-hash (files)
  "Return the SHA-256 of FILES' names and contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (pcase-dolist (`(,name . ,file) files)
      (insert name 0)
      (insert-file-contents-literally file nil nil nil)
      (goto-char (point-max))
      (insert 0))
    (secure-hash 'sha256 (current-buffer))))

(defun +agent-shell-arena-remote-code (callback)
  "Call CALLBACK with (HASH . TARBALL) for the runner's code, or nil and why.
The tarball is built once per hash, under the arena directory's code/."
  (condition-case err
      (let* ((files (+agent-shell-arena-remote--code-files))
             (hash (+agent-shell-arena-remote--code-hash files))
             (store (expand-file-name "code/" +agent-shell-pstack-arena-directory))
             (tarball (expand-file-name (concat hash ".tar") store)))
        (if (file-exists-p tarball)
            (run-at-time 0 nil callback (cons hash tarball) nil)
          (make-directory store t)
          (let ((staging (make-temp-file (expand-file-name ".code-" store) t))
                (partial (concat tarball ".partial")))
            (pcase-dolist (`(,name . ,file) files)
              (copy-file file (expand-file-name name staging) t))
            (+agent-shell-arena-remote--local
             staging (list "tar" "-cf" partial ".")
             (lambda (_ error)
               (delete-directory staging t)
               (if error (funcall callback nil error)
                 (rename-file partial tarball t)
                 (funcall callback (cons hash tarball) nil)))))))
    (error (run-at-time 0 nil callback nil (error-message-string err)))))

;;;; Submitting

(defun +agent-shell-arena-remote--session (lead)
  "Return the session id of LEAD's shell, or nil."
  (when (buffer-live-p lead)
    (ignore-errors (agent-shell-session-id :shell-buffer lead))))

(defun +agent-shell-arena-remote--job (panel base profile manifest)
  "Return the job that runs arena or delegate PANEL over BASE with PROFILE.
MANIFEST is the laptop's arena; its prerequisite and code come later.
A delegate's job carries its frozen policy's sandbox and the laptop
user's identity, and its deadline is its timeout."
  (let ((sparse (+agent-shell-arena-git-sparse base))
        (delegate (+agent-shell-pstack-panel--delegate-p panel)))
    (+agent-shell-arena-remote--make-job
     :id (+agent-shell-arena-git-manifest-id manifest)
     :panel (+agent-shell-pstack-panel-id panel)
     :shape (+agent-shell-pstack-panel-shape panel)
     :brief (+agent-shell-pstack-panel-brief panel)
     :rubric (+agent-shell-pstack-panel-rubric panel)
     :timeout (+agent-shell-pstack-panel-timeout panel)
     :judge-timeout (+agent-shell-pstack-panel-judge-timeout panel)
     :members
     (mapcar (lambda (member)
               (+agent-shell-arena-remote--make-request-member
                :role (+agent-shell-pstack-panel-member-role member)
                :agent (+agent-shell-pstack-panel-member-agent member)
                :kind (+agent-shell-pstack-panel-member-kind member)
                :focus (+agent-shell-pstack-panel-member-focus member)))
             (+agent-shell-pstack-panel-members panel))
     :delegate (when delegate
                 (+agent-shell-arena-remote--make-delegate-job
                  :sandbox (plist-get (+agent-shell-pstack-panel-delegate-options
                                       panel)
                                      :sandbox)
                  :identity (+agent-shell-pstack-panel-identity panel)))
     :prefix (+agent-shell-arena-git-base-prefix base)
     :base (+agent-shell-arena-git-base-commit base)
     :sparse (when sparse
               (+agent-shell-arena-remote--make-sparse
                :cone (plist-get sparse :cone)
                :patterns (plist-get sparse :patterns)))
     :profile (+agent-shell-arena-remote-plain-profile profile)
     :repository (plist-get profile :host-directory)
     :code (make-string 64 ?0)
     :deadline (if delegate (+agent-shell-pstack-panel-timeout panel)
                 (+agent-shell-pstack-panel-arena-budget
                  (+agent-shell-pstack-panel-timeout panel)
                  (+agent-shell-pstack-panel-judge-timeout panel)))
     :measure-egress (and (not delegate)
                          (+agent-shell-pstack-panel-measure-egress panel)))))

(defun +agent-shell-arena-remote--job-file (remote)
  "Return the laptop's copy of REMOTE's job."
  (expand-file-name "job.json" (+agent-shell-arena-remote-directory remote)))

(defun +agent-shell-arena-remote-launch (panel base profile)
  "Run arena or delegate PANEL over BASE on the host that PROFILE names.
PANEL is preparing.  A delegate's host is its frozen policy's, and its
laptop manifest holds a delegate record without a workspace.  Keep the
base alive under a ref, write the laptop's records, and submit the job."
  (let* ((delegate (+agent-shell-pstack-panel--delegate-p panel))
         (host (+agent-shell-arena-remote-parse-host
                (if delegate
                    (plist-get (+agent-shell-pstack-panel-delegate-options
                                panel)
                               :host)
                  (plist-get profile :host))))
         (manifest (+agent-shell-arena-git-create
                    base (+agent-shell-pstack-panel-id panel)
                    (when delegate
                      (+agent-shell-arena-git--delegate
                       :isolation 'worktree
                       :sandbox (plist-get
                                 (+agent-shell-pstack-panel-delegate-options
                                  panel)
                                 :sandbox)))))
         (remote (+agent-shell-arena-remote--make
                  :id (+agent-shell-arena-git-manifest-id manifest)
                  :panel (+agent-shell-pstack-panel-id panel)
                  :session (+agent-shell-arena-remote--session
                            (+agent-shell-pstack-panel-lead panel))
                  :shape (+agent-shell-pstack-panel-shape panel)
                  :host host :phase 'preparing
                  :directory (+agent-shell-arena-git-manifest-directory
                              manifest))))
    (setf (+agent-shell-pstack-panel-arena panel) manifest)
    (let ((job (+agent-shell-arena-remote--job panel base profile manifest)))
      (setf (+agent-shell-pstack-panel-remote panel) remote)
      (puthash (+agent-shell-arena-remote-id remote) remote
               +agent-shell-arena-remote--arenas)
      (+agent-shell-arena-remote--write-json
       (+agent-shell-arena-remote--job-file remote) 'job job)
      (+agent-shell-arena-remote--save remote)
      (+agent-shell-arena-remote--git
       manifest (list "update-ref" (+agent-shell-arena-remote--base-ref manifest)
                      (+agent-shell-arena-git-manifest-base manifest))
       (lambda (_ error)
         (if error (+agent-shell-arena-remote--reject remote error)
           (+agent-shell-arena-git-record
            manifest "base" (+agent-shell-arena-git-manifest-base manifest)
            "base")
           (when (eq (+agent-shell-arena-remote-phase remote) 'preparing)
             (+agent-shell-arena-remote--move remote 'submitting)
             (+agent-shell-arena-remote--submit remote job))))))))

(defun +agent-shell-arena-remote--reject (remote reason)
  "End REMOTE's arena, which never ran on its host, because of REASON.
Its panel gets the rejection follow-up and its laptop arena is removed."
  (cl-incf (+agent-shell-arena-remote-failures remote))
  (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
    (cancel-timer timer))
  (remhash (+agent-shell-arena-remote-id remote)
           +agent-shell-arena-remote--arenas)
  (setf (+agent-shell-arena-remote-phase remote) 'finished)
  (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
    (unless (eq (+agent-shell-pstack-panel-phase panel) 'finished)
      (+agent-shell-pstack-panel--fail-preparation
       panel (format "The arena could not start on %s: %s"
                     (+agent-shell-arena-remote--host-label
                      (+agent-shell-arena-remote-host remote))
                     reason)))))

(defun +agent-shell-arena-remote--probe (remote job callback)
  "Ask REMOTE's host about JOB's checkout; call CALLBACK with a plist.
The plist holds :toplevel, :tip, :code, :ready-file, :boot, the time the
host booted or nil, and :ready, or CALLBACK gets nil and a message."
  (+agent-shell-arena-remote--call
   (+agent-shell-arena-remote-host remote)
   +agent-shell-arena-remote--probe-script
   (list (+agent-shell-arena-remote-job-repository job)
         (+agent-shell-arena-remote-job-code job)
         (+agent-shell-arena-remote--ready-file
          (+agent-shell-arena-remote-host remote)))
   :timeout 120
   :callback
   (lambda (output error)
     (if error (funcall callback nil error)
       (let* ((start (and output (string-search "pstack-arena-probe 1\n" output)))
              (lines (and start (split-string (substring output start) "\n")))
              (field (lambda (name)
                       (when-let* ((line (seq-find
                                          (lambda (line)
                                            (string-prefix-p (concat name " ")
                                                             line))
                                          (seq-take lines 6))))
                         (string-trim (substring line (1+ (length name)))))))
              (ready (string-trim (string-join (nthcdr 6 lines) "\n"))))
         (if (null start)
             (funcall callback nil (format "unexpected answer: %s" output))
           (condition-case err
               (funcall callback
                        (list :toplevel (funcall field "toplevel")
                              :tip (funcall field "tip")
                              :code (equal (funcall field "code") "1")
                              :ready-file (funcall field "ready")
                              :boot (let ((boot (funcall field "boot")))
                                      (and boot (string-match-p "\\`[0-9]+\\'"
                                                                boot)
                                           (string-to-number boot)))
                              :ready (unless (string-empty-p ready)
                                       (+agent-shell-arena-remote-from-json
                                        'ready ready)))
                        nil)
             (+agent-shell-arena-remote-wire
              (funcall callback nil (format "its readiness file is \
malformed: %s" (cadr err)))))))))))

(defun +agent-shell-arena-remote--ready-file (host)
  "Return where HOST's setup writes its readiness file."
  (if (+agent-shell-arena-remote--host-option host :local)
      (expand-file-name "ready.json" (+agent-shell-arena-remote-host-name host))
    +agent-shell-arena-remote-ready-file))

(defun +agent-shell-arena-remote--version (ready tool)
  "Return the version READY gives for TOOL, a slot such as `emacs', or nil."
  (when-let* ((entry (and ready (cl-struct-slot-value
                                 '+agent-shell-arena-remote-ready tool ready))))
    (+agent-shell-arena-remote-tool-version entry)))

(defun +agent-shell-arena-remote--ready-text (ready)
  "Return what READY says ran, such as \"Emacs 31.1, codex-acp 2.2.2\"."
  (string-join
   (seq-keep (pcase-lambda (`(,tool . ,name))
               (when-let* ((version (+agent-shell-arena-remote--version
                                     ready tool)))
                 (format "%s %s" name version)))
             '((emacs . "Emacs") (claude-agent-acp . "claude-agent-acp")
               (codex-acp . "codex-acp")))
   ", "))

(defun +agent-shell-arena-remote--logins-text (logins)
  "Return how LOGINS stood, such as \"logins: claude-code refreshed, 8.0 h
left\"."
  (if (null logins) ""
    (concat
     "logins: "
     (mapconcat
      (lambda (login)
        (let ((remaining (+agent-shell-sandbox-login-result-remaining login)))
          (concat
           (symbol-name (+agent-shell-sandbox-login-result-agent login))
           (when (+agent-shell-sandbox-login-result-refreshed login)
             " refreshed")
           (cond ((+agent-shell-sandbox-login-result-error login) " refused")
                 (remaining (format ", %.1f h left" (/ remaining 3600.0)))
                 (t ", none")))))
      logins "; "))))

(defun +agent-shell-arena-remote--written (ready)
  "Return when READY says it was written, or nil when it does not say."
  (when-let* ((written (+agent-shell-arena-remote-ready-written-at ready)))
    (ignore-errors (float-time (encode-time (iso8601-parse written))))))

(defun +agent-shell-arena-remote--capability (ready name)
  "Return the value READY gives capability NAME: a version, 0, or nil."
  (when-let* ((capabilities (and ready (+agent-shell-arena-remote-ready-capabilities
                                        ready))))
    (cl-struct-slot-value '+agent-shell-arena-remote-capabilities name
                          capabilities)))

(defun +agent-shell-arena-remote--delegate-ready-p (ready)
  "Return whether READY says its host's setup can run delegates."
  (when-let* ((version (+agent-shell-arena-remote--capability ready 'delegate)))
    (>= version 1)))

(defun +agent-shell-arena-remote--linux-sandbox-p (host)
  "Return whether HOST's runner confines members with the Linux sandbox.
A local host's runner is this Emacs's system; any other runs on Linux."
  (if (+agent-shell-arena-remote--host-option host :local)
      (eq +agent-shell-sandbox-backend 'bwrap)
    t))

(defun +agent-shell-arena-remote--needs (host job)
  "Return the capabilities JOB needs on HOST, as registry names.
Every job needs git and the adapters, and a systemd launcher its user
manager, which also holds each delegate's scope.  A delegate needs the
delegate capability, and sandboxed members on Linux need namespaces,
bwrap and slirp4netns: every arena member, and a delegate when its
policy sandboxes it."
  (let ((delegate (+agent-shell-arena-remote-job-delegate job)))
    (append
     '(git node-adapters)
     (when (equal (+agent-shell-arena-remote--launcher host) "systemd")
       '(systemd-user))
     (when (eq (+agent-shell-arena-remote-job-shape job) 'delegate)
       '(delegate))
     (when (and (+agent-shell-arena-remote--linux-sandbox-p host)
                (or (not delegate)
                    (+agent-shell-arena-remote-delegate-job-sandbox delegate)))
       '(linux-namespaces bwrap slirp4netns)))))

(defun +agent-shell-arena-remote--wants (host job)
  "Return the capabilities JOB uses on HOST without needing them.
A measured job's Linux members capture egress with tshark; without it
their records are unavailable, and the job runs regardless."
  (when (and (+agent-shell-arena-remote-job-measure-egress job)
             (+agent-shell-arena-remote--linux-sandbox-p host))
    '(egress-tshark)))

(defun +agent-shell-arena-remote--lacking (ready names)
  "Return the capabilities of NAMES that READY does not prove."
  (seq-remove (lambda (name)
                (let ((version (+agent-shell-arena-remote--capability
                                ready name)))
                  (and version (>= version 1))))
              names))

(defun +agent-shell-arena-remote--capability-key (name)
  "Return capability NAME's key in ready.json."
  (nth 1 (assq name +agent-shell-arena-remote-capability-registry)))

(defun +agent-shell-arena-remote--capability-error (probe host job)
  "Return why PROBE's readiness file cannot run JOB on HOST, or nil.
Name each capability JOB needs that the file does not prove and what
sets it up.  One the file never declares means its setup predates it."
  (let* ((ready (plist-get probe :ready))
         (lacking (+agent-shell-arena-remote--lacking
                   ready (+agent-shell-arena-remote--needs host job)))
         (undeclared (seq-filter (lambda (name)
                                   (null (+agent-shell-arena-remote--capability
                                          ready name)))
                                 lacking)))
    (when lacking
      (concat
       (format "its readiness file at %s does not prove the capabilities \
this job needs: %s.  "
               (plist-get probe :ready-file)
               (mapconcat #'+agent-shell-arena-remote--capability-key lacking
                          ", "))
       (mapconcat (lambda (name)
                    (format "%s: %s."
                            (+agent-shell-arena-remote--capability-key name)
                            (plist-get (nthcdr 2 (assq name
                                                       +agent-shell-arena-remote-capability-registry))
                                       :setup)))
                  lacking "  ")
       (when undeclared
         (format "  The file does not declare %s at all, so the host's \
setup predates %s; rerun it."
                 (mapconcat #'+agent-shell-arena-remote--capability-key
                            undeclared ", ")
                 (if (cdr undeclared) "them" "it")))))))

(defun +agent-shell-arena-remote--capability-notes (probe host job)
  "Return what JOB on HOST goes without, as PROBE's readiness file says.
That names the capabilities it uses but does not need; nil when none lack."
  (when-let* ((lacking (+agent-shell-arena-remote--lacking
                        (plist-get probe :ready)
                        (+agent-shell-arena-remote--wants host job))))
    (format "its readiness file does not prove %s, so egress records will \
be unavailable"
            (mapconcat #'+agent-shell-arena-remote--capability-key lacking
                       ", "))))

(defun +agent-shell-arena-remote--ready-error (probe repository)
  "Return why PROBE says the host cannot run a job for REPOSITORY, or nil.
A readiness file written before the host last booted is stale: setup
removes it on every boot before it writes a fresh one."
  (let* ((ready (plist-get probe :ready))
         (written (and ready (+agent-shell-arena-remote--written ready)))
         (boot (plist-get probe :boot)))
    (cond
     ((null ready)
      (format "it has no readiness file at %s, so its setup has not passed \
its smoke check" (plist-get probe :ready-file)))
     ((and written boot (< written boot))
      (format "its readiness file at %s was written at %s, before it booted \
at %s, so its setup has not finished since"
              (plist-get probe :ready-file)
              (format-time-string "%F %T %Z" written)
              (format-time-string "%F %T %Z" boot)))
     ((< (string-to-number (+agent-shell-arena-remote--version ready 'emacs))
         +agent-shell-arena-remote-minimum-emacs)
      (format "its Emacs is %s, older than %d"
              (+agent-shell-arena-remote--version ready 'emacs)
              +agent-shell-arena-remote-minimum-emacs))
     ((string-empty-p (or (plist-get probe :toplevel) ""))
      (format "it has no git checkout at %s" repository)))))

(defun +agent-shell-arena-remote--coder-wake (name callback)
  "Check or start Coder workspace NAME, reporting wake events to CALLBACK.
NEXT continuations retain whether this submission already started it."
  (let ((started nil))
    (cl-labels
        ((report (event &optional detail next)
           (funcall callback event detail (or next #'status)))
         (status ()
           (+agent-shell-arena-remote--coder
            '("list" "-o" "json") 60
            (lambda (output error)
              (pcase (and output (condition-case nil
                                     (list (+agent-shell-arena-remote--workspace
                                            output name))
                                   (json-error nil)))
                ('nil (report 'retry (or error "coder list is not JSON")))
                ('(nil) (report 'error
                                (format "coder list shows no workspace %s" name)))
                (`((,"running" . ,(or "connected" 'nil))) (report 'ready))
                (`((,"running" . ,agent))
                 (report 'wait (format "running with its agent %s" agent)))
                (`((,(or "stopped" "canceled") . ,_))
                 (if started (report 'wait "still stopped")
                   (report 'starting nil #'start)))
                (`((,(and state (or "pending" "starting" "stopping"
                                    "canceling")) . ,_))
                 (report 'wait state))
                (`((,state . ,_))
                 (report 'error (format "its status is %s" state)))))))
         (start ()
           (setq started t)
           (+agent-shell-arena-remote--coder
            (list "start" "--yes" name) 900
            (lambda (_output error)
              (if error (report 'error error) (report 'check))))))
      (status))))

(defun +agent-shell-arena-remote--await (remote job callback)
  "Call CALLBACK with a probe of REMOTE's host once it can run JOB.
A stopped Coder workspace is started first.  A host that was not running,
or booted less than `+agent-shell-arena-remote-wake-timeout' seconds ago,
is checked every `+agent-shell-arena-remote-wake-interval' seconds until
it runs, answers and has a readiness file written since it booted.
Otherwise, or once that timeout has passed, REMOTE is rejected with what
was last wrong.  A cancel stops the wait."
  (let* ((host (+agent-shell-arena-remote-host remote))
         (wake (+agent-shell-arena-remote--host-option host :wake))
         (deadline (+ (float-time) +agent-shell-arena-remote-wake-timeout))
         (woke nil))
    (cl-labels
        ((reject (reason)
           (+agent-shell-arena-remote--reject
            remote (if woke
                       (format "%s, but it was not ready within %s seconds: \
%s" woke +agent-shell-arena-remote-wake-timeout reason)
                     reason)))
         (retry (reason)
           (if (> (float-time) deadline) (reject reason)
             (setf (+agent-shell-arena-remote-timer remote)
                   (run-at-time +agent-shell-arena-remote-wake-interval nil
                                #'check))))
         (wait (reason)
           (unless woke (setq woke (format "it was %s" reason)))
           (retry (format "it is %s" reason)))
         (check ()
           (cond
            ((not (eq (+agent-shell-arena-remote-phase remote) 'submitting)))
            ((not (eq (+agent-shell-arena-remote-cancel-state remote) 'none))
             (+agent-shell-arena-remote--reject
              remote (+agent-shell-arena-remote--cancel-reason remote)))
            (wake (funcall wake))
            (t (probe))))
         (awakened (event detail next)
           (setq wake next)
           (pcase event
             ('ready (probe))
             ('retry (if woke (retry detail) (probe)))
             ('wait (wait detail))
             ('starting
              (setq woke "it was stopped, so Emacs started it")
              (message "pstack arena %s: starting %s"
                       (+agent-shell-arena-remote-id remote)
                       (+agent-shell-arena-remote--host-label host))
              (funcall next))
             ('check (check))
             ('error (reject detail))))
         (probe ()
           (+agent-shell-arena-remote--probe
            remote job
            (lambda (probe error)
              (let ((problem (or error
                                 (+agent-shell-arena-remote--ready-error
                                  probe
                                  (+agent-shell-arena-remote-job-repository
                                   job))))
                    (boot (plist-get probe :boot)))
                (cond
                 ((not problem)
                  (if-let* ((refusal (+agent-shell-arena-remote--capability-error
                                      probe host job)))
                      (+agent-shell-arena-remote--reject remote refusal)
                    (when-let* ((note (+agent-shell-arena-remote--capability-notes
                                       probe host job)))
                      (message "pstack arena %s: %s, %s"
                               (+agent-shell-arena-remote-id remote)
                               (+agent-shell-arena-remote--host-label host)
                               note))
                    (funcall callback probe)))
                 ((or woke
                      (and boot
                           (< (- (float-time) boot)
                              +agent-shell-arena-remote-wake-timeout)))
                  (retry problem))
                 (t (reject problem))))))))
      (when wake
        (let ((function wake))
          (setq wake (lambda ()
                       (funcall function
                                (+agent-shell-arena-remote-host-name host)
                                #'awakened)))))
      (check))))

(defun +agent-shell-arena-remote--submit (remote job)
  "Submit JOB, REMOTE's arena, to its host.
Wait until the host is ready, then try each prerequisite in turn until
the host takes a bundle against one."
  (+agent-shell-arena-remote--await
   remote job
   (lambda (probe)
     (setf (+agent-shell-arena-remote-emacs remote)
           (+agent-shell-arena-remote-tool-path
            (+agent-shell-arena-remote-ready-emacs (plist-get probe :ready)))
           (+agent-shell-arena-remote-job-ready-file job)
           (plist-get probe :ready-file)
           (+agent-shell-arena-remote-job-ready job) (plist-get probe :ready))
     (+agent-shell-arena-remote--save remote)
     (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
       (+agent-shell-arena-remote-prerequisites
        (+agent-shell-pstack-panel-arena panel) (plist-get probe :tip)
        (lambda (ladder)
          (+agent-shell-arena-remote--offer remote job ladder nil
                                            (plist-get probe :code))))))))

(defun +agent-shell-arena-remote--offer (remote job ladder tried has-code
                                                &optional attempts)
  "Send JOB with a bundle against the first of LADDER to REMOTE's host.
TRIED lists the prerequisites the host already lacked, and HAS-CODE says
whether the host has the runner's code.  ATTEMPTS counts submissions
lost to a transport error, which are resent, since the host takes a job
it already started as started."
  (let* ((panel (+agent-shell-arena-remote--panel remote))
         (manifest (and panel (+agent-shell-pstack-panel-arena panel)))
         (directory (+agent-shell-arena-remote-directory remote))
         (prerequisite (car ladder))
         (label (+agent-shell-arena-remote--host-label
                 (+agent-shell-arena-remote-host remote)))
         (fail (lambda (reason)
                 (+agent-shell-arena-remote--reject remote reason))))
    (cond
     ((not (eq (+agent-shell-arena-remote-phase remote) 'submitting)))
     ((null ladder)
      (funcall fail (format "%s lacks every commit the base bundle could \
build on (%s) and could not fetch them from origin" label
                            (string-join (delq nil (reverse tried)) ", "))))
     (t
      (+agent-shell-arena-remote-bundle
       manifest prerequisite (expand-file-name "base.bundle" directory)
       (lambda (bundle error)
         (if error (funcall fail error)
           (+agent-shell-arena-remote-code
            (lambda (code error)
              (if error (funcall fail error)
                (setf (+agent-shell-arena-remote-job-prerequisite job)
                      prerequisite
                      (+agent-shell-arena-remote-job-code job) (car code))
                (+agent-shell-arena-remote--upload
                 remote job bundle (unless has-code (cdr code))
                 (lambda (upload error)
                   (if error (funcall fail error)
                     (+agent-shell-arena-remote--extend
                      (+agent-shell-arena-remote-host remote)
                      (+ (+agent-shell-arena-remote-job-deadline job)
                         +agent-shell-arena-remote-slack)
                      (lambda (error)
                        (if error (funcall fail error)
                          (setf (+agent-shell-arena-remote-extended remote)
                                (+ (float-time)
                                   (+agent-shell-arena-remote-job-deadline job)
                                   +agent-shell-arena-remote-slack))
                          (+agent-shell-arena-remote--send
                           remote job upload
                           (lambda (answer error)
                             (pcase answer
                               ('started
                                (+agent-shell-arena-remote--submitted remote))
                               ('missing
                                (+agent-shell-arena-remote--offer
                                 remote job (cdr ladder)
                                 (cons prerequisite tried) has-code))
                               ('code
                                (+agent-shell-arena-remote--offer
                                 remote job ladder tried nil))
                               ('lost
                                (if (< (or attempts 0) 3)
                                    (run-at-time
                                     (+agent-shell-arena-remote--delay
                                      (or attempts 0))
                                     nil
                                     #'+agent-shell-arena-remote--offer
                                     remote job ladder tried has-code
                                     (1+ (or attempts 0)))
                                  (funcall fail error)))
                               (_ (funcall fail error)))))))))))))))))))))

(defun +agent-shell-arena-remote--upload (remote job bundle tarball callback)
  "Pack JOB, BUNDLE and, when non-nil, the code TARBALL for REMOTE's host.
A cancel asked for already goes in as the marker.  Call CALLBACK with
the upload's file and nil, or nil and a message."
  (condition-case err
      (let* ((directory (+agent-shell-arena-remote-directory remote))
             (staging (expand-file-name "upload/" directory))
             (upload (expand-file-name "upload.tar" directory))
             (write (lambda (name text)
                      (let ((coding-system-for-write 'utf-8-unix))
                        (write-region text nil (expand-file-name name staging)
                                      nil 'silent)))))
        (when (file-directory-p staging) (delete-directory staging t))
        (make-directory staging t)
        (+agent-shell-arena-remote--write-json
         (+agent-shell-arena-remote--job-file remote) 'job job)
        (copy-file (+agent-shell-arena-remote--job-file remote)
                   (expand-file-name "job.json" staging))
        (copy-file bundle (expand-file-name "base.bundle" staging))
        (funcall write "prerequisite"
                 (or (+agent-shell-arena-remote-job-prerequisite job) ""))
        (funcall write "code-hash" (+agent-shell-arena-remote-job-code job))
        (when tarball (copy-file tarball (expand-file-name "code.tar" staging)))
        (unless (eq (+agent-shell-arena-remote-cancel-state remote) 'none)
          (funcall write "cancel" "The arena was cancelled before it started.\n"))
        (+agent-shell-arena-remote--local
         staging (list "tar" "-cf" upload ".")
         (lambda (_ error)
           (delete-directory staging t)
           (funcall callback (unless error upload) error))))
    (error (run-at-time 0 nil callback nil (error-message-string err)))))

(defun +agent-shell-arena-remote--send (remote job upload callback)
  "Send UPLOAD, JOB's files, to REMOTE's host and start its runner.
Call CALLBACK with `started', `missing' when the host lacks the
prerequisite, `code' when it lacks the code, `lost' when the call failed,
or nil; and a message."
  (+agent-shell-arena-remote--call
   (+agent-shell-arena-remote-host remote)
   +agent-shell-arena-remote--submit-script
   (list (+agent-shell-arena-remote-id remote)
         (+agent-shell-arena-remote-job-repository job)
         (+agent-shell-arena-remote-emacs remote)
         (+agent-shell-arena-remote--launcher
          (+agent-shell-arena-remote-host remote)))
   :input upload :timeout 900
   :callback
   (lambda (output error)
     (let ((answer (+agent-shell-arena-remote--answer
                    output "pstack-arena-submit ")))
       (cond
        (error (funcall callback 'lost error))
        ((equal answer "started") (funcall callback 'started nil))
        ((equal answer "code") (funcall callback 'code nil))
        ((and answer (string-prefix-p "missing" answer))
         (funcall callback 'missing nil))
        ((and answer (string-prefix-p "failed " answer))
         (funcall callback nil (substring answer (length "failed "))))
        (t (funcall callback nil (format "unexpected answer: %s" output))))))))

(defun +agent-shell-arena-remote--submitted (remote)
  "Note that REMOTE's host took its job, and start polling."
  (setf (+agent-shell-arena-remote-submitted remote) t
        (+agent-shell-arena-remote-seen remote) (float-time))
  (+agent-shell-arena-remote--move remote 'remote)
  (+agent-shell-arena-remote--schedule remote 0))

;;;; Polling

(defun +agent-shell-arena-remote--schedule (remote delay)
  "Poll REMOTE's host after DELAY seconds, replacing any poll planned."
  (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
    (cancel-timer timer))
  (setf (+agent-shell-arena-remote-timer remote)
        (run-at-time delay nil #'+agent-shell-arena-remote--poll
                     (+agent-shell-arena-remote-id remote))))

(defun +agent-shell-arena-remote--poll (id)
  "Read the status of the remote arena with ID, sending a pending cancel."
  (when-let* ((remote (gethash id +agent-shell-arena-remote--arenas))
              ((eq (+agent-shell-arena-remote-phase remote) 'remote)))
    (setf (+agent-shell-arena-remote-timer remote) nil)
    (+agent-shell-arena-remote--call
     (+agent-shell-arena-remote-host remote)
     +agent-shell-arena-remote--poll-script
     (list id (if (eq (+agent-shell-arena-remote-cancel-state remote) 'pending)
                  "cancel" "-")
           (or (+agent-shell-arena-remote--cancel-reason remote) "")
           (+agent-shell-arena-remote--cursor-argument remote)
           (or (+agent-shell-arena-remote-emacs remote) ""))
     :timeout 120
     :callback (lambda (output error)
                 (+agent-shell-arena-remote--polled remote output error)))))

(defvar +agent-shell-arena-remote--cancel-reasons (make-hash-table :test #'equal)
  "Map an arena id to why its cancel was asked for.")

(defun +agent-shell-arena-remote--cancel-reason (remote)
  "Return why REMOTE's cancel was asked for, or nil."
  (gethash (+agent-shell-arena-remote-id remote)
           +agent-shell-arena-remote--cancel-reasons))

(defun +agent-shell-arena-remote-parse-poll (output)
  "Return (:alive BOOL :cancel BOOL :status STATUS :tail TAIL) from OUTPUT.
Return `none' when the host has no such job; signal when OUTPUT is
malformed.  STATUS is nil before the runner first writes one.  A
version-2 answer is one JSON envelope, whose TAIL is a delegate's
transcript page as parsed JSON, or nil."
  (if-let* ((start (and output (string-match "^pstack-arena-poll 2\n" output))))
      (let ((envelope (condition-case err
                          (json-parse-string (substring output (match-end 0))
                                             :object-type 'alist
                                             :array-type 'list
                                             :null-object :null
                                             :false-object :false)
                        (json-error
                         (signal '+agent-shell-arena-remote-wire
                                 (list (format "poll envelope is not JSON: %s"
                                               (error-message-string err))))))))
        (unless (and (listp envelope) (consp (car-safe envelope)))
          (signal '+agent-shell-arena-remote-wire
                  (list "poll envelope is not an object")))
        (list :alive (eq t (alist-get 'alive envelope))
              :cancel (eq t (alist-get 'cancel envelope))
              :status (let ((status (alist-get 'status envelope)))
                        (unless (+agent-shell-arena-remote--absent-p status)
                          (+agent-shell-arena-remote--decode 'status status)))
              :tail (let ((tail (alist-get 'tail envelope)))
                      (unless (+agent-shell-arena-remote--absent-p tail)
                        tail))))
    (+agent-shell-arena-remote--parse-poll-1 output)))

(defun +agent-shell-arena-remote--parse-poll-1 (output)
  "Parse a version-1 poll's OUTPUT as `+agent-shell-arena-remote-parse-poll'."
  (let ((start (and output (string-match "^pstack-arena-poll \\(.*\\)$" output))))
    (unless start
      (signal '+agent-shell-arena-remote-wire
              (list (format "unexpected poll answer: %s" output))))
    (let* ((line (match-string 1 output))
           (rest (string-trim (substring output (match-end 0))))
           (words (split-string line)))
      (if (equal words '("none")) 'none
        (list :alive (member "alive=1" words) :cancel (member "cancel=1" words)
              :status (unless (string-empty-p rest)
                        (+agent-shell-arena-remote-from-json 'status rest)))))))

(defun +agent-shell-arena-remote--polled (remote output error)
  "Act on the poll of REMOTE that gave OUTPUT or ERROR."
  (when (eq (+agent-shell-arena-remote-phase remote) 'remote)
    (let ((poll (and (not error)
                     (condition-case err
                         (+agent-shell-arena-remote-parse-poll output)
                       (+agent-shell-arena-remote-wire
                        (setq error (cadr err)) nil)))))
      (cond
       (error (+agent-shell-arena-remote--trouble remote error))
       ((eq poll 'none)
        (+agent-shell-arena-remote--trouble remote "the host has no such job"))
       (t
        (let ((status (plist-get poll :status))
              (now (float-time))
              (more nil))
          (setf (+agent-shell-arena-remote-failures remote) 0
                (+agent-shell-arena-remote-error remote) nil
                (+agent-shell-arena-remote-polled remote) now)
          (when-let* ((tail (plist-get poll :tail)))
            (condition-case err
                (setq more (and (not (alist-get 'error tail))
                                (+agent-shell-arena-remote--cache-transcript
                                 remote tail)))
              (error (message "pstack delegate %s: transcript page refused: %s"
                              (+agent-shell-arena-remote-id remote)
                              (error-message-string err)))))
          (when (and (plist-get poll :cancel)
                     (eq (+agent-shell-arena-remote-cancel-state remote) 'pending))
            (setf (+agent-shell-arena-remote-cancel-state remote) 'sent))
          (when status
            (unless (eql (+agent-shell-arena-remote-status-heartbeat status)
                         (+agent-shell-arena-remote-heartbeat remote))
              (setf (+agent-shell-arena-remote-heartbeat remote)
                    (+agent-shell-arena-remote-status-heartbeat status)
                    (+agent-shell-arena-remote-seen remote) now))
            (setf (+agent-shell-arena-remote-arena remote)
                  (+agent-shell-arena-remote-status-arena status)
                  (+agent-shell-arena-remote-deadline remote)
                  (+agent-shell-arena-remote-status-deadline status))
            (+agent-shell-arena-remote--mirror remote status)
            (+agent-shell-arena-remote--keep-awake remote status))
          (+agent-shell-arena-remote--save remote)
          (cond
           ((and status
                 (eq (+agent-shell-arena-remote-status-phase status) 'finished))
            (+agent-shell-arena-remote--move remote 'fetching)
            (+agent-shell-arena-remote--fetch remote))
           ((and (not (plist-get poll :alive))
                 (> (- now (or (+agent-shell-arena-remote-seen remote) now))
                    +agent-shell-arena-remote-stale-after)
                 (< (+agent-shell-arena-remote-restarts remote)
                    +agent-shell-arena-remote-restarts))
            (+agent-shell-arena-remote--resume remote))
           ((+agent-shell-arena-remote--overdue-p remote)
            (+agent-shell-arena-remote--unreachable remote))
           (t (+agent-shell-arena-remote--schedule
               remote (+agent-shell-arena-remote--interval remote more))))))))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-arena-remote--interval (remote more)
  "Return the seconds until REMOTE's next poll.
MORE says its transcript has another page waiting, fetched as soon as
the host's queue allows; a shown transcript polls every
`+agent-shell-arena-remote-transcript-interval' seconds."
  (cond (more 0)
        ((+agent-shell-arena-remote--transcript-visible-p remote)
         (min +agent-shell-arena-remote-transcript-interval
              +agent-shell-arena-remote-poll-interval))
        (t +agent-shell-arena-remote-poll-interval)))

(defun +agent-shell-arena-remote--overdue-p (remote)
  "Return whether REMOTE's host has gone too long without a heartbeat."
  (> (- (float-time) (or (+agent-shell-arena-remote-seen remote) (float-time)))
     +agent-shell-arena-remote-unreachable-after))

(defun +agent-shell-arena-remote--trouble (remote error)
  "Note that a call about REMOTE failed with ERROR, then poll again later.
Once its host has been silent too long, tell the lead."
  (cl-incf (+agent-shell-arena-remote-failures remote))
  (setf (+agent-shell-arena-remote-error remote) error)
  (cond
   ((not (memq (+agent-shell-arena-remote-phase remote) '(remote fetching)))
    (message "pstack arena %s: %s" (+agent-shell-arena-remote-id remote) error))
   ((+agent-shell-arena-remote--overdue-p remote)
    (+agent-shell-arena-remote--unreachable remote))
   (t
    (when (eq (+agent-shell-arena-remote-phase remote) 'fetching)
      (+agent-shell-arena-remote--move remote 'remote))
    (+agent-shell-arena-remote--schedule
     remote (+agent-shell-arena-remote--delay
             (+agent-shell-arena-remote-failures remote)))))
  (+agent-shell-pstack-panels--refresh))

(defun +agent-shell-arena-remote--keep-awake (remote status)
  "Extend REMOTE's workspace when STATUS's deadline passes its stop time."
  (when-let* ((deadline (+agent-shell-arena-remote-status-deadline status))
              ((> (+ deadline +agent-shell-arena-remote-slack)
                  (or (+agent-shell-arena-remote-extended remote) 0))))
    (let ((until (+ deadline +agent-shell-arena-remote-slack)))
      (setf (+agent-shell-arena-remote-extended remote) until)
      (+agent-shell-arena-remote--extend
       (+agent-shell-arena-remote-host remote) (- until (float-time))
       (lambda (error)
         (when error
           (setf (+agent-shell-arena-remote-extended remote) nil)
           (message "pstack arena %s: %s" (+agent-shell-arena-remote-id remote)
                    error)))))))

(defun +agent-shell-arena-remote--failure (state)
  "Return the provider failure member STATE names a kind of, or nil."
  (when-let* ((kind (+agent-shell-arena-remote-member-failure-kind state)))
    (list :kind kind)))

(defun +agent-shell-arena-remote--mirror (remote status)
  "Show STATUS's member states in REMOTE's panel.
Members never settle through the local path; they only take the states
the runner reports."
  (setf (+agent-shell-arena-remote-runner-phase remote)
        (+agent-shell-arena-remote-status-phase status)
        (+agent-shell-arena-remote-logins remote)
        (+agent-shell-arena-remote-status-logins status))
  (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
    (dolist (state (+agent-shell-arena-remote-status-members status))
      (when-let* ((member (+agent-shell-pstack-panel--member
                           panel (+agent-shell-arena-remote-member-role state))))
        (setf (+agent-shell-pstack-panel-member-state member)
              (+agent-shell-arena-remote-member-state state)
              (+agent-shell-pstack-panel-member-capture member)
              (+agent-shell-arena-remote-member-capture state)
              (+agent-shell-pstack-panel-member-error member)
              (+agent-shell-arena-remote-member-error state)
              (+agent-shell-pstack-panel-member-failure member)
              (+agent-shell-arena-remote--failure state))))
    (let ((file (expand-file-name "status.json"
                                  (+agent-shell-arena-remote-directory remote))))
      (+agent-shell-arena-remote--write-json file 'status status))))

(defun +agent-shell-arena-remote--resume (remote)
  "Start REMOTE's runner again; it captures what its candidates left."
  (cl-incf (+agent-shell-arena-remote-restarts remote))
  (setf (+agent-shell-arena-remote-seen remote) (float-time))
  (+agent-shell-arena-remote--save remote)
  (+agent-shell-arena-remote--call
   (+agent-shell-arena-remote-host remote)
   +agent-shell-arena-remote--resume-script
   (list (+agent-shell-arena-remote-id remote)
         (or (+agent-shell-arena-remote-emacs remote) "emacs")
         (+agent-shell-arena-remote--launcher
          (+agent-shell-arena-remote-host remote)))
   :timeout 120
   :callback (lambda (_output error)
               (if error (+agent-shell-arena-remote--trouble remote error)
                 (+agent-shell-arena-remote--schedule
                  remote +agent-shell-arena-remote-poll-interval)))))

(defun +agent-shell-arena-remote--unreachable (remote)
  "Tell REMOTE's lead that its host stopped answering, and stop polling."
  (when-let* ((timer (+agent-shell-arena-remote-timer remote)))
    (cancel-timer timer))
  (+agent-shell-arena-remote--move remote 'unreachable)
  (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
    (setf (+agent-shell-pstack-panel-followup panel)
          (+agent-shell-arena-remote--unreachable-text panel remote)
          (+agent-shell-pstack-panel-delivered panel) nil)
    (+agent-shell-pstack-panel--deliver-to (+agent-shell-pstack-panel-lead panel))
    (+agent-shell-pstack-panels--refresh)))

(defun +agent-shell-arena-remote--unreachable-text (panel remote)
  "Return the follow-up telling PANEL's lead that REMOTE's host is silent."
  (format "pstack panel %s (arena) on %s is unreachable: no heartbeat since \
%s%s.  Its candidates may still be running there, and its results may still \
arrive.  Emacs stopped polling.  To re-attach later, run M-x \
+agent-shell-pstack-arena-reattach and choose %s, or evaluate \
(+agent-shell-pstack-arena-reattach \"%s\")."
          (+agent-shell-pstack-panel-id panel)
          (+agent-shell-arena-remote--host-label
           (+agent-shell-arena-remote-host remote))
          (format-time-string "%F %T" (+agent-shell-arena-remote-seen remote))
          (if-let* ((error (+agent-shell-arena-remote-error remote)))
              (format " (last error: %s)" error)
            "")
          (+agent-shell-arena-remote-id remote)
          (+agent-shell-arena-remote-id remote)))

;;;; Fetching

(defun +agent-shell-arena-remote--fetch (remote)
  "Fetch REMOTE's report and results bundle from its host."
  (let ((tarball (expand-file-name "results.tar"
                                   (+agent-shell-arena-remote-directory remote))))
    (+agent-shell-arena-remote--call
     (+agent-shell-arena-remote-host remote)
     +agent-shell-arena-remote--fetch-script
     (list (+agent-shell-arena-remote-id remote))
     :output tarball :timeout 900
     :callback (lambda (_ error)
                 (if error (+agent-shell-arena-remote--trouble remote error)
                   (+agent-shell-arena-remote--receive remote tarball))))))

(defun +agent-shell-arena-remote--receive (remote tarball)
  "Unpack TARBALL, REMOTE's results, into the lead's repository and finish.
Each result lands in refs/pstack-arena/ID/ROLE, checked against the
report, and its diff is taken here as a local arena's is."
  (let* ((directory (+agent-shell-arena-remote-directory remote))
         (results (expand-file-name "results/" directory))
         (fail (lambda (error)
                 (+agent-shell-arena-remote--trouble remote error))))
    (when (file-directory-p results) (delete-directory results t))
    (make-directory results t)
    (+agent-shell-arena-remote--local
     results (list "tar" "-xf" tarball)
     (lambda (_ error)
       (if error (funcall fail error)
         (condition-case err
             (let ((report (+agent-shell-arena-remote--read-json
                            (expand-file-name "report.json" results) 'report)))
               (unless (equal (+agent-shell-arena-remote-report-id report)
                              (+agent-shell-arena-remote-id remote))
                 (signal '+agent-shell-arena-remote-wire
                         (list "the report is for another arena")))
               (+agent-shell-arena-remote--receive-egress remote results)
               (when (eq (+agent-shell-arena-remote-shape remote) 'delegate)
                 (+agent-shell-arena-remote--reconcile-transcript remote results))
               (funcall
                (if (eq (+agent-shell-arena-remote-report-shape report) 'delegate)
                    #'+agent-shell-arena-remote--land-delegate
                  #'+agent-shell-arena-remote--land)
                remote report (expand-file-name "results.bundle" results)
                (lambda (results error)
                  (if error (funcall fail error)
                    (+agent-shell-arena-remote--finish remote report results)))))
           ((error +agent-shell-arena-remote-wire)
            (funcall fail (error-message-string err)))))))))

(defun +agent-shell-arena-remote--receive-egress (remote results)
  "Give REMOTE's measured panel the egress records unpacked in RESULTS.
Each is checked and saved under the laptop's own panel folder; a missing
or invalid one is saved as unavailable, and never stops the results."
  (when-let* ((panel (+agent-shell-arena-remote--panel remote))
              ((+agent-shell-pstack-panel-measure-egress panel)))
    (require '+agent-shell-egress)
    (with-demoted-errors "pstack arena: egress records failed: %S"
      (dolist (member (+agent-shell-pstack-panel--egress-members panel))
        (let ((name (concat (+agent-shell-pstack-panel-member-role member)
                            ".egress.json"))
              (id (+agent-shell-pstack-panel-id panel)))
          (setf (+agent-shell-pstack-panel-member-egress-record member)
                (+agent-shell-egress-import
                 (expand-file-name (format "reports/%s/%s" id name) results)
                 (+agent-shell-pstack-panel--egress-identity panel member)
                 (+agent-shell-pstack-panel--egress-file panel member))))))))

(defun +agent-shell-arena-remote--land (remote report bundle callback)
  "Fetch REPORT's results from BUNDLE into REMOTE's laptop refs.
Call CALLBACK with an alist of role to result plist and nil, or nil and
a message."
  (let* ((panel (+agent-shell-arena-remote--panel remote))
         (manifest (+agent-shell-pstack-panel-arena panel))
         (captured (seq-filter #'+agent-shell-arena-remote-member-commit
                               (+agent-shell-arena-remote-report-members
                                report)))
         (arena (+agent-shell-arena-remote-report-arena report)))
    (cl-labels
        ((fetch (done)
           (if (null captured) (funcall done)
             (+agent-shell-arena-remote--git
              manifest (list "bundle" "verify" "-q" bundle)
              (lambda (_ error)
                (if error (funcall callback nil error)
                  (+agent-shell-arena-remote--git
                   manifest
                   (append
                    (list "fetch" "-q" "--no-tags" "--no-write-fetch-head"
                          bundle)
                    (mapcar
                     (lambda (member)
                       (let ((role (+agent-shell-arena-remote-member-role
                                    member)))
                         (format "+refs/pstack-arena/%s/%s:%s" arena role
                                 (+agent-shell-arena-git-ref manifest role))))
                     captured))
                   (lambda (_ error)
                     (if error (funcall callback nil error)
                       (funcall done)))))))))
         (compare (members found)
           (if (null members) (funcall callback (nreverse found) nil)
             (let* ((member (car members))
                    (role (+agent-shell-arena-remote-member-role member))
                    (commit (+agent-shell-arena-remote-member-commit member)))
               (if (not commit)
                   (compare (cdr members) found)
                 (+agent-shell-arena-remote--git
                  manifest (list "rev-parse" "--verify" "-q"
                                 (+agent-shell-arena-git-ref manifest role))
                  (lambda (landed _)
                    (if (not (equal landed commit))
                        (funcall callback nil
                                 (format "%s's result landed as %s, not the \
reported %s" role landed commit))
                      (+agent-shell-arena-git-record
                       manifest role commit
                       (symbol-name (+agent-shell-arena-remote-member-state
                                     member)))
                      (+agent-shell-arena-git-diff
                       manifest role
                       (lambda (diff error)
                         (if error (funcall callback nil error)
                           (compare (cdr members)
                                    (cons (cons role
                                                (append
                                                 (list :commit commit
                                                       :ref
                                                       (+agent-shell-arena-git-ref
                                                        manifest role))
                                                 diff))
                                          found)))))))))))))
      (if (and captured (not arena))
          (funcall callback nil "the report names results but no host arena")
        (fetch (lambda () (compare captured nil)))))))

(defun +agent-shell-arena-remote--land-delegate (remote report bundle callback)
  "Fetch REPORT's delegate branch from BUNDLE into REMOTE's laptop repository.
The branch lands first under the job's own refs/pstack-arena/ID/delegate,
is checked against the report, the panel and the base, then becomes
refs/heads/pstack/delegate/PANEL by compare-and-swap.  A branch already
there is accepted only when it holds the same commit; otherwise it is
left alone, the fetched ref stays for recovery and the result says so.
Its commits are listed here, from the verified tip.  Call CALLBACK with
an alist of the delegate's role to its result plist and nil, or nil and
a message for a failure worth retrying."
  (let* ((panel (+agent-shell-arena-remote--panel remote))
         (manifest (+agent-shell-pstack-panel-arena panel))
         (outcome (+agent-shell-arena-remote-report-delegate report))
         (member (seq-find (lambda (member)
                             (eq 'delegate (+agent-shell-arena-remote-member-kind
                                            member)))
                           (+agent-shell-arena-remote-report-members report)))
         (role (and member (+agent-shell-arena-remote-member-role member)))
         (base (+agent-shell-arena-git-manifest-base manifest))
         (branch (concat "refs/heads/"
                         (+agent-shell-arena-git-delegate-branch manifest)))
         (temporary (+agent-shell-arena-git-ref manifest "delegate"))
         (tip (and outcome (+agent-shell-arena-remote-outcome-tip outcome)))
         (git (lambda (args done)
                (+agent-shell-arena-remote--git
                 manifest args
                 (lambda (output error)
                   (if error (funcall callback nil error)
                     (funcall done output)))))))
    (cl-labels
        ((result (&rest plist)
           (funcall callback
                    (list (cons role (append (list :outcome outcome) plist)))
                    nil))
         (listed (published)
           (+agent-shell-arena-git-delegate-log
            manifest tip
            (lambda (commits error)
              (if error (funcall callback nil error)
                (let ((delegate (+agent-shell-arena-git-manifest-delegate
                                 manifest)))
                  (setf (+agent-shell-arena-git-delegate-commits delegate)
                        commits)
                  (+agent-shell-arena-git-write-manifest manifest))
                (apply #'result :tip tip :published published
                       (unless published
                         (list :collision
                               (format "%s already exists here at another \
commit, so Emacs left it alone.  The delegate's branch is at %s, which \
%s holds."
                                       branch tip temporary))))))))
         (publish ()
           (let ((existing (+agent-shell-arena-git--read-in
                            (+agent-shell-arena-git-manifest-git-dir manifest)
                            (+agent-shell-arena-git-manifest-toplevel manifest)
                            "rev-parse" "--verify" "-q" branch)))
             (cond
              ((equal existing tip) (drop-temporary))
              (existing (listed nil))
              (t (funcall git (list "update-ref" branch tip "")
                          (lambda (_) (drop-temporary)))))))
         (drop-temporary ()
           (funcall git (list "update-ref" "-d" temporary tip)
                    (lambda (_) (listed t)))))
      (cond
       ((null role)
        (funcall callback nil "the delegate's report names no delegate"))
       ((null tip) (result))
       ((not (equal (+agent-shell-arena-remote-outcome-branch outcome) branch))
        (funcall callback nil (format "the report names branch %s, not %s"
                                      (+agent-shell-arena-remote-outcome-branch
                                       outcome)
                                      branch)))
       (t
        (funcall
         git (list "bundle" "verify" "-q" bundle)
         (lambda (_)
           (funcall
            git (list "fetch" "-q" "--no-tags" "--no-write-fetch-head" bundle
                      (format "+%s:%s" branch temporary))
            (lambda (_)
              (+agent-shell-arena-git-record manifest "delegate" tip "fetched")
              (funcall
               git (list "rev-parse" "--verify" "-q" temporary)
               (lambda (landed)
                 (if (not (equal landed tip))
                     (funcall callback nil (format "the delegate's branch landed \
as %s, not the reported %s" landed tip))
                   (+agent-shell-arena-remote--git
                    manifest (list "merge-base" "--is-ancestor" base tip)
                    (lambda (_ error)
                      (if error
                          (funcall callback nil (format "the delegate's branch \
tip %s does not descend from its base %s" tip base))
                        (publish))))))))))))))))

(defun +agent-shell-arena-remote--finish (remote report results)
  "Give REMOTE's panel REPORT's members and RESULTS, then finish it.
The follow-up is composed as a local arena's is."
  (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
    (setf (+agent-shell-pstack-panel-rejection panel)
          (+agent-shell-arena-remote-report-rejection report))
    (dolist (reported (+agent-shell-arena-remote-report-members report))
      (when-let* ((role (+agent-shell-arena-remote-member-role reported))
                  (member (+agent-shell-pstack-panel--member panel role)))
        (setf (+agent-shell-pstack-panel-member-state member)
              (+agent-shell-arena-remote-member-state reported)
              (+agent-shell-pstack-panel-member-text member)
              (+agent-shell-arena-remote-member-text reported)
              (+agent-shell-pstack-panel-member-error member)
              (+agent-shell-arena-remote-member-error reported)
              (+agent-shell-pstack-panel-member-failure member)
              (+agent-shell-arena-remote--failure reported)
              (+agent-shell-pstack-panel-member-capture member)
              (+agent-shell-arena-remote-member-capture reported)
              (+agent-shell-pstack-panel-member-result member)
              (or (alist-get role results nil nil #'equal)
                  (when-let* ((failure (+agent-shell-arena-remote-member-failure
                                        reported)))
                    (list :error failure))))
        (+agent-shell-pstack-panel--write panel member)))
    (setf (+agent-shell-arena-remote-fetched remote) t
          (+agent-shell-arena-remote-ready remote)
          (+agent-shell-arena-remote-report-ready report))
    (unless (eq (+agent-shell-arena-remote-phase remote) 'finished)
      (+agent-shell-arena-remote--move remote 'finished))
    (+agent-shell-pstack-panel--finish panel)
    (+agent-shell-arena-remote--sync)))

;;;; Delegate transcripts

(defvar-local +agent-shell-arena-remote--transcript-id nil
  "Id of the remote delegate this transcript buffer shows.")

(defvar-local +agent-shell-arena-remote--transcript-shown nil
  "Sequence number of the last record this buffer shows, or nil to redraw.")

(cl-defstruct (+agent-shell-arena-remote--recorder
               (:constructor +agent-shell-arena-remote--make-recorder)
               (:copier nil))
  "A delegate's transcript as its runner writes it, in DIRECTORY.
EPOCH names the stream, SEGMENT the one written, SIZE its bytes, FIRST
the oldest kept, SEQ the last record's number.  PENDING holds agent text
not yet written, newest first."
  directory epoch segment (size 0) first (seq 0) pending)

(defconst +agent-shell-arena-remote--transcript-kinds '("assistant" "tool"
                                                        "notice")
  "Kinds of record a transcript holds.")

(defun +agent-shell-arena-remote--segment-file (directory segment)
  "Return transcript SEGMENT's file in DIRECTORY."
  (expand-file-name (format "%08d.jsonl" segment) directory))

(defun +agent-shell-arena-remote--segments (directory)
  "Return the segment numbers in transcript DIRECTORY, oldest first."
  (sort (mapcar (lambda (file) (string-to-number (file-name-base file)))
                (and (file-directory-p directory)
                     (directory-files directory nil "\\`[0-9]\\{8\\}\\.jsonl\\'")))
        #'<))

(defun +agent-shell-arena-remote--read-bytes (file &optional start end)
  "Return FILE's bytes from START to END as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil start end)
    (buffer-string)))

(defun +agent-shell-arena-remote--recorder-open (directory)
  "Return the recorder of the transcript in DIRECTORY, resuming one there.
An incomplete last line, left by a runner that died mid-write, goes, and
numbering resumes after the last whole record."
  (with-file-modes #o700 (make-directory directory t))
  (let* ((epoch-file (expand-file-name "epoch" directory))
         (epoch (or (ignore-errors
                      (string-trim (+agent-shell-arena-remote--read-bytes
                                    epoch-file)))
                    (let ((epoch (format "%08x%08x" (random #x100000000)
                                         (random #x100000000))))
                      (with-file-modes #o600
                        (write-region epoch nil epoch-file nil 'silent))
                      epoch)))
         (segments (+agent-shell-arena-remote--segments directory))
         (recorder (+agent-shell-arena-remote--make-recorder
                    :directory directory :epoch epoch
                    :segment (or (car (last segments)) 1)
                    :first (or (car segments) 1))))
    (dolist (segment (reverse segments))
      (let* ((file (+agent-shell-arena-remote--segment-file directory segment))
             (bytes (+agent-shell-arena-remote--read-bytes file))
             (end (let ((newline (string-search "\n" (reverse bytes))))
                    (if newline (- (length bytes) newline) 0))))
        (when (< end (length bytes))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (substring bytes 0 end) nil file nil 'silent)))
        (when (= segment (+agent-shell-arena-remote--recorder-segment recorder))
          (setf (+agent-shell-arena-remote--recorder-size recorder) end))
        (when (and (zerop (+agent-shell-arena-remote--recorder-seq recorder))
                   (> end 0))
          (let ((lines (split-string (decode-coding-string
                                      (substring bytes 0 end) 'utf-8)
                                     "\n" t)))
            (setf (+agent-shell-arena-remote--recorder-seq recorder)
                  (or (ignore-errors
                        (alist-get 'seq (json-parse-string
                                         (car (last lines))
                                         :object-type 'alist)))
                      0))))))
    recorder))

(defun +agent-shell-arena-remote--record-lines (recorder kind text)
  "Return RECORDER's next records of KIND for TEXT as encoded lines.
TEXT is split between characters so no line passes
`+agent-shell-arena-remote-transcript-record-bytes'."
  (let ((encode (lambda (seq text)
                  (encode-coding-string
                   (concat (json-serialize `((seq . ,seq)
                                             (time . ,(float-time))
                                             (kind . ,kind) (text . ,text)))
                           "\n")
                   'utf-8-unix))))
    (cl-labels ((lines (text)
                  (let ((line (funcall encode
                                       (1+ (+agent-shell-arena-remote--recorder-seq
                                            recorder))
                                       text)))
                    (if (or (<= (length line)
                                +agent-shell-arena-remote-transcript-record-bytes)
                            (< (length text) 2))
                        (progn (cl-incf (+agent-shell-arena-remote--recorder-seq
                                         recorder))
                               (list line))
                      (let ((half (/ (length text) 2)))
                        (append (lines (substring text 0 half))
                                (lines (substring text half))))))))
      (lines text))))

(defun +agent-shell-arena-remote--record (recorder kind text)
  "Append a record of KIND holding TEXT to RECORDER's transcript.
A line never spans segments: one that would overflow the current
segment starts the next, and the oldest go once more than
`+agent-shell-arena-remote-transcript-segments' remain."
  (let ((directory (+agent-shell-arena-remote--recorder-directory recorder)))
    (dolist (line (+agent-shell-arena-remote--record-lines recorder kind text))
      (when (and (> (+agent-shell-arena-remote--recorder-size recorder) 0)
                 (> (+ (+agent-shell-arena-remote--recorder-size recorder)
                       (length line))
                    +agent-shell-arena-remote-transcript-segment-bytes))
        (cl-incf (+agent-shell-arena-remote--recorder-segment recorder))
        (setf (+agent-shell-arena-remote--recorder-size recorder) 0))
      (let ((coding-system-for-write 'no-conversion))
        (with-file-modes #o600
          (write-region line nil
                        (+agent-shell-arena-remote--segment-file
                         directory
                         (+agent-shell-arena-remote--recorder-segment recorder))
                        t 'silent)))
      (cl-incf (+agent-shell-arena-remote--recorder-size recorder)
               (length line)))
    (let ((keep (- (+agent-shell-arena-remote--recorder-segment recorder)
                   (1- +agent-shell-arena-remote-transcript-segments))))
      (when (> keep (+agent-shell-arena-remote--recorder-first recorder))
        (setf (+agent-shell-arena-remote--recorder-first recorder) keep)
        (dolist (segment (+agent-shell-arena-remote--segments directory))
          (when (< segment keep)
            (ignore-errors
              (delete-file (+agent-shell-arena-remote--segment-file
                            directory segment)))))))))

(defun +agent-shell-arena-remote--flush-transcript (recorder)
  "Write the agent text RECORDER holds back as one record."
  (when-let* ((pending (+agent-shell-arena-remote--recorder-pending recorder)))
    (setf (+agent-shell-arena-remote--recorder-pending recorder) nil)
    (+agent-shell-arena-remote--record recorder "assistant"
                                       (apply #'concat (reverse pending)))))

(defun +agent-shell-arena-remote--record-transcript (runner event)
  "Append the public projection of delegate EVENT to RUNNER's transcript.
Agent text chunks gather into assistant records, tool calls become tool
records of their title and status, and lifecycle events notices.  Auth,
permission payloads, environment, tool arguments, hidden reasoning and
the final text, which the report holds, never go in.  The runner is the
transcript's only writer."
  (when-let* ((recorder (+agent-shell-arena-remote--runner-transcript runner)))
    (let ((text (plist-get event :text)))
      (pcase (plist-get event :kind)
        ('progress
         (pcase (plist-get event :part)
           ('assistant
            (push text (+agent-shell-arena-remote--recorder-pending recorder))
            (when (> (apply #'+ (mapcar #'string-bytes
                                        (+agent-shell-arena-remote--recorder-pending
                                         recorder)))
                     4096)
              (+agent-shell-arena-remote--flush-transcript recorder)))
           ('tool
            (+agent-shell-arena-remote--flush-transcript recorder)
            (+agent-shell-arena-remote--record
             recorder "tool"
             (concat text (when-let* ((status (plist-get event :status)))
                            (format " (%s)" status)))))))
        (kind
         (when-let* ((notice
                      (pcase kind
                        ('init-finished "The delegate's agent started.")
                        ('input-submitted "Emacs sent the brief.")
                        ('turn-complete
                         (format "The turn ended: %s."
                                 (or (plist-get event :stop-reason) "done")))
                        ('error (format "The agent reported an error: %s"
                                        (or (plist-get event :message)
                                            "no message")))
                        ('clean-up "The delegate's shell ended."))))
           (+agent-shell-arena-remote--flush-transcript recorder)
           (+agent-shell-arena-remote--record recorder "notice" notice)))))))

(defun +agent-shell-arena-remote--runner-transcript-state (runner)
  "Return what RUNNER's status says of its transcript, or nil."
  (when-let* ((recorder (+agent-shell-arena-remote--runner-transcript runner)))
    (+agent-shell-arena-remote--make-transcript
     :epoch (+agent-shell-arena-remote--recorder-epoch recorder)
     :first (+agent-shell-arena-remote--recorder-first recorder)
     :last (+agent-shell-arena-remote--recorder-segment recorder)
     :seq (+agent-shell-arena-remote--recorder-seq recorder))))

(defun +agent-shell-arena-remote--page-records (directory segments segment
                                                          offset)
  "Read whole records from SEGMENT at byte OFFSET on, in transcript DIRECTORY.
SEGMENTS are those it keeps.  Return (RECORDS NEXT-SEGMENT NEXT-OFFSET
MORE BAD): at most `+agent-shell-arena-remote-transcript-page-bytes' of
records, where reading continues, whether more follows, and whether a
malformed line was skipped."
  (let ((budget +agent-shell-arena-remote-transcript-page-bytes)
        records more bad)
    (catch 'full
      (while (memq segment segments)
        (let* ((file (+agent-shell-arena-remote--segment-file directory segment))
               (size (or (file-attribute-size (file-attributes file)) 0))
               (cut (< (+ offset budget) size))
               (bytes (if (< offset size)
                          (+agent-shell-arena-remote--read-bytes
                           file offset (min size (+ offset budget)))
                        ""))
               (start 0))
          (while (let ((newline (string-search "\n" bytes start)))
                   (when (and newline (<= (- (1+ newline) start) budget))
                     (let ((line (decode-coding-string
                                  (substring bytes start newline) 'utf-8)))
                       (condition-case nil
                           (push (json-parse-string line :object-type 'alist)
                                 records)
                         (json-error (setq bad t))))
                     (cl-decf budget (- (1+ newline) start))
                     (setq start (1+ newline))))
            nil)
          (setq offset (+ offset start))
          (cond
           ;; Past a whole record the page had no room for, or a line the
           ;; runner is still writing.
           ((< offset size) (setq more cut) (throw 'full nil))
           ((memq (1+ segment) segments)
            (if (<= budget 0) (progn (setq more t) (throw 'full nil))
              (setq segment (1+ segment) offset 0)))
           (t (throw 'full nil))))))
    (list (nreverse records) segment offset more bad)))

(defun +agent-shell-arena-remote--poll-page (directory cursor)
  "Return the version-2 poll envelope for job DIRECTORY on the host.
CURSOR is nil or (EPOCH SEGMENT OFFSET SEQ).  The envelope holds whether
the runner lives, whether a cancel marker is there, the status as the
runner wrote it, and a transcript tail: at most
`+agent-shell-arena-remote-transcript-page-bytes' of whole records from
CURSOR, the cursor after them, and whether there is a gap before them or
more after.  A cursor the host no longer keeps resumes at the oldest
record it does, with a gap.  Reading the tail never stops the rest, and
nothing here writes the runner's files."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (file (lambda (name) (expand-file-name name directory)))
         (pid (ignore-errors
                (string-to-number (+agent-shell-arena-remote--read-bytes
                                   (funcall file "pid")))))
         (status (ignore-errors
                   (json-parse-string
                    (decode-coding-string
                     (+agent-shell-arena-remote--read-bytes
                      (funcall file "status.json"))
                     'utf-8)
                    :object-type 'alist :array-type 'array
                    :null-object :null :false-object :false)))
         (tail
          (condition-case err
              (let* ((transcript (funcall file "transcript/"))
                     (epoch (ignore-errors
                              (string-trim (+agent-shell-arena-remote--read-bytes
                                            (expand-file-name "epoch"
                                                              transcript)))))
                     (segments (+agent-shell-arena-remote--segments transcript))
                     (first (car segments)))
                (when (and epoch first)
                  (pcase-let*
                      ((same (and cursor (equal (nth 0 cursor) epoch)
                                  (memq (nth 1 cursor) segments)))
                       (gap (if cursor (not same) (> first 1)))
                       (from (if same (list (nth 1 cursor) (nth 2 cursor))
                               (list first 0)))
                       (`(,records ,segment ,offset ,more ,bad)
                        (+agent-shell-arena-remote--page-records
                         transcript segments (car from) (cadr from))))
                    `((epoch . ,epoch) (gap . ,(if (or gap bad) t :false))
                      (from . ((segment . ,(car from))
                               (offset . ,(cadr from))))
                      (next . ((segment . ,segment) (offset . ,offset)))
                      (records . ,(vconcat records))
                      (more . ,(if more t :false))))))
            (error `((error . ,(error-message-string err)))))))
    `((version . 2)
      (alive . ,(if (and (natnump pid) (> pid 0) (process-attributes pid)) t
                  :false))
      (cancel . ,(if (file-exists-p (funcall file "cancel")) t :false))
      (status . ,(or status :null))
      (tail . ,(or tail :null)))))

(defun +agent-shell-arena-remote-poll-batch ()
  "Print the poll envelope of the job folder and cursor on the command line."
  (let* ((directory (pop command-line-args-left))
         (cursor (let ((words (split-string (or (pop command-line-args-left) "")
                                            ":")))
                   (when (= 4 (length words))
                     (list (nth 0 words) (string-to-number (nth 1 words))
                           (string-to-number (nth 2 words))
                           (string-to-number (nth 3 words)))))))
    ;; `princ' would encode json-serialize's UTF-8 bytes a second time.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "pstack-arena-poll 2\n"
              (json-serialize (+agent-shell-arena-remote--poll-page
                               directory cursor))
              "\n")
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil "/dev/stdout" nil 'silent)))
    (kill-emacs 0)))

(defun +agent-shell-arena-remote--cursor-argument (remote)
  "Return the poll script's transcript argument for REMOTE.
A delegate asks for its tail from its cursor, or from the start; an
arena has none."
  (if (not (eq (+agent-shell-arena-remote-shape remote) 'delegate)) "-"
    (if-let* ((cursor (+agent-shell-arena-remote-cursor remote)))
        (format "%s:%d:%d:%d" (+agent-shell-arena-remote-cursor-epoch cursor)
                (+agent-shell-arena-remote-cursor-segment cursor)
                (+agent-shell-arena-remote-cursor-offset cursor)
                (+agent-shell-arena-remote-cursor-seq cursor))
      "start")))

(defun +agent-shell-arena-remote--cache-file (remote)
  "Return the laptop's cache of REMOTE's transcript, beside its reports.
Cleaning the delegate's job up leaves it, as it leaves the reports."
  (let ((directory (expand-file-name (+agent-shell-arena-remote-panel remote)
                                     +agent-shell-pstack-panel-directory)))
    (make-directory directory t)
    (expand-file-name "transcript.jsonl" directory)))

(defun +agent-shell-arena-remote--check-record (record)
  "Return transcript RECORD, an alist, or signal that it is malformed."
  (unless (and (listp record) (consp (car-safe record))
               (natnump (alist-get 'seq record))
               (numberp (alist-get 'time record))
               (member (alist-get 'kind record)
                       +agent-shell-arena-remote--transcript-kinds)
               (stringp (alist-get 'text record)))
    (signal '+agent-shell-arena-remote-wire
            (list (format "malformed transcript record: %s"
                          (truncate-string-to-width (format "%S" record) 60
                                                    nil nil "...")))))
  record)

(defun +agent-shell-arena-remote--cache-transcript (remote page)
  "Validate REMOTE's transcript PAGE and add it to the laptop's cache.
PAGE is a poll's parsed tail.  Records the cache has, by sequence number,
are skipped, so a page replayed after a restart adds nothing twice; a
gap, or records dropped to keep the cache within
`+agent-shell-arena-remote-transcript-cache-bytes', leaves a notice.  The
cache is written before REMOTE's cursor moves past the page.  Shown
transcript buffers then gain the new records.  Return whether more
follows on the host."
  (let* ((cursor (+agent-shell-arena-remote-cursor remote))
         (epoch (alist-get 'epoch page))
         (next (alist-get 'next page))
         (records (alist-get 'records page))
         (same (and cursor (equal epoch (+agent-shell-arena-remote-cursor-epoch
                                         cursor))))
         (last (if same (+agent-shell-arena-remote-cursor-seq cursor) 0)))
    (unless (and (stringp epoch)
                 (string-match-p +agent-shell-arena-git--name-regexp epoch)
                 (natnump (alist-get 'segment next))
                 (natnump (alist-get 'offset next))
                 (listp records))
      (signal '+agent-shell-arena-remote-wire
              (list "malformed transcript page")))
    (let* ((records (mapcar #'+agent-shell-arena-remote--check-record records))
           (seqs (mapcar (lambda (record) (alist-get 'seq record)) records)))
      (unless (equal seqs (sort (seq-uniq seqs) #'<))
        (signal '+agent-shell-arena-remote-wire
                (list "transcript records out of order")))
      (let ((fresh (seq-filter (lambda (record) (> (alist-get 'seq record) last))
                               records))
            (gap (or (eq t (alist-get 'gap page))
                     (and cursor (not same)))))
        (+agent-shell-arena-remote--append-cache
         remote
         (append (when gap
                   (list `((seq . 0) (time . ,(float-time)) (kind . "notice")
                           (text . "Part of the transcript is missing here: \
the host kept only its newest part, or its runner started a new one."))))
                 fresh))
        (setf (+agent-shell-arena-remote-cursor remote)
              (+agent-shell-arena-remote--make-cursor
               :epoch epoch :segment (alist-get 'segment next)
               :offset (alist-get 'offset next)
               :seq (if fresh (alist-get 'seq (car (last fresh))) last)))
        (+agent-shell-arena-remote--save remote)
        (+agent-shell-arena-remote--show-transcript remote)
        (eq t (alist-get 'more page))))))

(defun +agent-shell-arena-remote--append-cache (remote records)
  "Append RECORDS to REMOTE's transcript cache, keeping it within bounds.
Past `+agent-shell-arena-remote-transcript-cache-bytes', whole oldest
records go, and a notice says so."
  (when records
    (let ((file (+agent-shell-arena-remote--cache-file remote))
          (coding-system-for-write 'utf-8-unix))
      (with-file-modes #o600
        (write-region (mapconcat (lambda (record)
                                   (concat (json-serialize record) "\n"))
                                 records "")
                      nil file t 'silent))
      (when (> (or (file-attribute-size (file-attributes file)) 0)
               +agent-shell-arena-remote-transcript-cache-bytes)
        (let* ((bytes (+agent-shell-arena-remote--read-bytes file))
               (keep (- (length bytes)
                        (* 3 (/ +agent-shell-arena-remote-transcript-cache-bytes
                                4))))
               (start (1+ (or (string-search "\n" bytes keep) (1- (length bytes))))))
          (let ((coding-system-for-write 'no-conversion))
            (write-region
             (concat (encode-coding-string
                      (concat (json-serialize
                               `((seq . 0) (time . ,(float-time))
                                 (kind . "notice")
                                 (text . "Older transcript was dropped to keep \
it within its size bound.")))
                              "\n")
                      'utf-8-unix)
                     (substring bytes start))
             nil file nil 'silent))
          (when-let* ((buffer (+agent-shell-arena-remote--transcript-live remote)))
            (with-current-buffer buffer
              (setq +agent-shell-arena-remote--transcript-shown nil))))))))

(defun +agent-shell-arena-remote--cached-records (remote)
  "Return the records in REMOTE's transcript cache, oldest first."
  (let ((file (+agent-shell-arena-remote--cache-file remote)))
    (when (file-readable-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (seq-keep (lambda (line)
                    (ignore-errors (json-parse-string line :object-type 'alist)))
                  (split-string (buffer-string) "\n" t))))))

(defun +agent-shell-arena-remote--reconcile-transcript (remote results)
  "Cache what REMOTE's fetched transcript segments in RESULTS hold beyond
the cache.  A missing or broken transcript never stops the results."
  (with-demoted-errors "pstack delegate: transcript catch-up failed: %S"
    (let* ((directory (expand-file-name "transcript/" results))
           (epoch (ignore-errors
                    (string-trim (+agent-shell-arena-remote--read-bytes
                                  (expand-file-name "epoch" directory)))))
           (segments (+agent-shell-arena-remote--segments directory)))
      (when (and epoch segments)
        (let ((cursor (+agent-shell-arena-remote-cursor remote))
              (records nil))
          (dolist (segment segments)
            (dolist (line (split-string
                           (decode-coding-string
                            (+agent-shell-arena-remote--read-bytes
                             (+agent-shell-arena-remote--segment-file
                              directory segment))
                            'utf-8)
                           "\n" t))
              (when-let* ((record (ignore-errors
                                    (json-parse-string line
                                                       :object-type 'alist))))
                (push record records))))
          (+agent-shell-arena-remote--cache-transcript
           remote
           `((epoch . ,epoch)
             (gap . ,(if (and cursor
                              (equal epoch (+agent-shell-arena-remote-cursor-epoch
                                            cursor))
                              (> (car segments)
                                 (+agent-shell-arena-remote-cursor-segment
                                  cursor)))
                         t :false))
             (next . ((segment . ,(car (last segments))) (offset . 0)))
             (records . ,(nreverse records)))))))))

;;;; The transcript buffer

(defvar-keymap +agent-shell-arena-remote-transcript-mode-map
  :doc "Keymap for `+agent-shell-arena-remote-transcript-mode'."
  "g" #'+agent-shell-arena-remote-transcript-refresh)

(define-derived-mode +agent-shell-arena-remote-transcript-mode special-mode
  "pstack transcript"
  "Read a remote delegate's transcript, as its host recorded it.
The text is the host's, shown literally; nothing here sends it anything.
\\<+agent-shell-arena-remote-transcript-mode-map>\\[+agent-shell-arena-remote-transcript-refresh] polls its host now, and \\[quit-window] closes the window."
  (setq-local truncate-lines nil
              buffer-undo-list t
              header-line-format
              '(:eval (+agent-shell-arena-remote--transcript-header))))

(defun +agent-shell-arena-remote--transcript-name (remote)
  "Return the name of REMOTE's transcript buffer."
  (format "*pstack delegate %s transcript*" (+agent-shell-arena-remote-panel remote)))

(defun +agent-shell-arena-remote--transcript-live (remote)
  "Return REMOTE's transcript buffer when it exists, or nil."
  (get-buffer (+agent-shell-arena-remote--transcript-name remote)))

(defun +agent-shell-arena-remote--transcript-visible-p (remote)
  "Return whether a window shows REMOTE's transcript."
  (when-let* ((buffer (+agent-shell-arena-remote--transcript-live remote)))
    (get-buffer-window buffer t)))

(defun +agent-shell-arena-remote--transcript-header ()
  "Return the header line of the current transcript buffer."
  (when-let* ((remote (gethash +agent-shell-arena-remote--transcript-id
                               +agent-shell-arena-remote--arenas)))
    (let* ((panel (+agent-shell-arena-remote--panel remote))
           (member (and panel (car (+agent-shell-pstack-panel-members panel))))
           (file (+agent-shell-arena-remote--cache-file remote)))
      (format "%s | %s | %s | read-only | last poll %s | %d KiB kept"
              (+agent-shell-arena-remote--host-label
               (+agent-shell-arena-remote-host remote))
              (if member
                  (format "%s (%s)" (+agent-shell-pstack-panel-member-role member)
                          (+agent-shell-pstack-panel-member-agent member))
                "delegate")
              (if member (+agent-shell-pstack-panel-member-state member)
                (+agent-shell-arena-remote-phase remote))
              (if-let* ((polled (+agent-shell-arena-remote-polled remote)))
                  (format-time-string "%H:%M:%S" polled)
                "never")
              (/ (or (file-attribute-size (file-attributes file)) 0) 1024)))))

(defun +agent-shell-arena-remote--render-record (record)
  "Return RECORD as the transcript buffer shows it, literally."
  (let ((lines (split-string (alist-get 'text record) "\n")))
    (concat (format-time-string "%H:%M:%S" (alist-get 'time record)) "  "
            (format "%-9s  " (alist-get 'kind record))
            (string-join lines "\n                     ")
            "\n")))

(defun +agent-shell-arena-remote--show-transcript (remote)
  "Add REMOTE's new cached records to its transcript buffer, if it exists.
Windows at its end follow the new text; others keep their place."
  (when-let* ((buffer (+agent-shell-arena-remote--transcript-live remote)))
    (with-current-buffer buffer
      (let* ((records (+agent-shell-arena-remote--cached-records remote))
             (shown +agent-shell-arena-remote--transcript-shown)
             (windows (get-buffer-window-list buffer nil t))
             (following (seq-filter (lambda (window)
                                      (>= (window-point window) (point-max)))
                                    windows))
             (at-end (>= (point) (point-max)))
             (inhibit-read-only t))
        (save-excursion
          (if shown
              (setq records (seq-filter (lambda (record)
                                          (> (alist-get 'seq record) shown))
                                        records))
            (erase-buffer))
          (goto-char (point-max))
          (dolist (record records)
            (unless (and shown (zerop (alist-get 'seq record)))
              (insert (+agent-shell-arena-remote--render-record record))))
          (setq +agent-shell-arena-remote--transcript-shown
                (or (car (last (delq 0 (mapcar (lambda (record)
                                                 (alist-get 'seq record))
                                               records))))
                    shown 0)))
        (when at-end (goto-char (point-max)))
        (dolist (window following)
          (set-window-point window (point-max)))
        (force-mode-line-update)))))

(defun +agent-shell-arena-remote-transcript-buffer (remote)
  "Return REMOTE's read-only transcript buffer, creating it if needed.
It shows the laptop's bounded cache of the delegate's transcript
literally, in `+agent-shell-arena-remote-transcript-mode', with its host,
member and last poll in the header.  It starts no agent, shell, process
or timer: polling stays with REMOTE's host queue, which polls sooner
while the buffer is shown, and a finished delegate's transcript reads
from the cache."
  (let ((buffer (get-buffer-create
                 (+agent-shell-arena-remote--transcript-name remote))))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-shell-arena-remote-transcript-mode)
        (+agent-shell-arena-remote-transcript-mode))
      (setq +agent-shell-arena-remote--transcript-id
            (+agent-shell-arena-remote-id remote)))
    (+agent-shell-arena-remote--show-transcript remote)
    (when (and (eq (+agent-shell-arena-remote-phase remote) 'remote)
               (null (+agent-shell-arena-remote-timer remote)))
      (+agent-shell-arena-remote--schedule remote 0))
    buffer))

(defun +agent-shell-arena-remote-transcript-refresh ()
  "Poll the host of the current transcript buffer's delegate now."
  (interactive)
  (if-let* ((remote (gethash +agent-shell-arena-remote--transcript-id
                             +agent-shell-arena-remote--arenas)))
      (if (eq (+agent-shell-arena-remote-phase remote) 'remote)
          (+agent-shell-arena-remote--schedule remote 0)
        (+agent-shell-arena-remote--show-transcript remote)
        (message "The delegate is %s; its transcript is all here"
                 (+agent-shell-arena-remote-phase remote)))
    (user-error "This transcript's delegate is no longer attached")))

;;;; Cancel, abandon, re-attach and clean


(defvar +agent-shell-arena-remote--quitting nil
  "Non-nil while Emacs quits, which never cancels a remote arena.")

(defun +agent-shell-arena-remote-cancel (panel reason)
  "Cancel remote arena PANEL because of REASON.
Before the host has the job, the arena stops as a local one would.
After, a marker tells its runner, which captures what its candidates
made and skips the judge; a marker that could not be sent is retried."
  (let ((remote (+agent-shell-pstack-panel-remote panel)))
    (pcase (+agent-shell-arena-remote-phase remote)
      ('preparing
       (setf (+agent-shell-pstack-panel-remote panel) nil)
       (+agent-shell-pstack-panel-cancel panel reason)
       (setf (+agent-shell-pstack-panel-remote panel) remote))
      ((or 'submitting 'remote)
       (when (eq (+agent-shell-arena-remote-cancel-state remote) 'none)
         (puthash (+agent-shell-arena-remote-id remote) reason
                  +agent-shell-arena-remote--cancel-reasons)
         (setf (+agent-shell-arena-remote-cancel-state remote) 'pending)
         (+agent-shell-arena-remote--save remote)
         (when (eq (+agent-shell-arena-remote-phase remote) 'remote)
           (+agent-shell-arena-remote--schedule remote 0))
         (+agent-shell-pstack-panels--refresh))))))

(defun +agent-shell-arena-remote-abandon (panel)
  "Cancel remote arena PANEL, whose lead's shell ended, unless Emacs quits.
Quitting leaves the arena running; its runner's deadline bounds it."
  (unless +agent-shell-arena-remote--quitting
    (+agent-shell-arena-remote-cancel panel "The lead's shell ended.")))

(defun +agent-shell-arena-remote--note-quitting ()
  "Note that Emacs is quitting."
  (setq +agent-shell-arena-remote--quitting t))

(defun +agent-shell-arena-remote--sync ()
  "Save each remote arena whose panel's delivery changed."
  (maphash (lambda (_ remote)
             (when-let* ((panel (+agent-shell-arena-remote--panel remote))
                         ((not (eq (and (+agent-shell-pstack-panel-delivered panel)
                                        t)
                                   (+agent-shell-arena-remote-delivered remote)))))
               (setf (+agent-shell-arena-remote-delivered remote)
                     (and (+agent-shell-pstack-panel-delivered panel) t))
               (+agent-shell-arena-remote--save remote)))
           +agent-shell-arena-remote--arenas))

(defun +agent-shell-arena-remote--restore (directory)
  "Rebuild the panel of the remote arena recorded in DIRECTORY and resume it.
Return the record, or nil when there is nothing left to do."
  (let* ((remote (+agent-shell-arena-remote--read-json
                  (+agent-shell-arena-remote--record-file directory) 'record))
         (id (+agent-shell-arena-remote-id remote)))
    (setf (+agent-shell-arena-remote-directory remote)
          (file-name-as-directory directory))
    (unless (or (gethash id +agent-shell-arena-remote--arenas)
                (and (eq (+agent-shell-arena-remote-phase remote) 'finished)
                     (+agent-shell-arena-remote-delivered remote)))
      (let* ((job (+agent-shell-arena-remote--read-json
                   (+agent-shell-arena-remote--job-file remote) 'job))
             (manifest (+agent-shell-arena-git-read-manifest directory))
             (panel (+agent-shell-arena-remote--rebuild remote job manifest))
             (status-file (expand-file-name "status.json" directory)))
        (puthash id remote +agent-shell-arena-remote--arenas)
        (puthash (+agent-shell-pstack-panel-id panel) panel
                 +agent-shell-pstack-panel--panels)
        (when (file-exists-p status-file)
          (ignore-errors
            (+agent-shell-arena-remote--mirror
             remote (+agent-shell-arena-remote--read-json status-file 'status))))
        (pcase (+agent-shell-arena-remote-phase remote)
          ('preparing
           (+agent-shell-arena-remote--reject
            remote "Emacs restarted before it reached the host"))
          ('submitting (+agent-shell-arena-remote--submit remote job))
          ('remote (+agent-shell-arena-remote--schedule remote 0))
          ('fetching (+agent-shell-arena-remote--fetch remote))
          ('finished
           (setf (+agent-shell-pstack-panel-phase panel) 'fetching)
           (+agent-shell-arena-remote--receive
            remote (expand-file-name "results.tar" directory)))
          ('unreachable
           (setf (+agent-shell-pstack-panel-followup panel)
                 (+agent-shell-arena-remote--unreachable-text panel remote)
                 (+agent-shell-pstack-panel-delivered panel)
                 (+agent-shell-arena-remote-delivered remote))))
        remote))))

(defun +agent-shell-arena-remote--job-options (job)
  "Return the delegate policy JOB's runner panel works under, or nil.
Its routing keys are cleared, so the panel can never submit itself."
  (when-let* ((delegate (+agent-shell-arena-remote-job-delegate job)))
    (list :host nil :isolation 'worktree
          :sandbox (+agent-shell-arena-remote-delegate-job-sandbox delegate))))

(defun +agent-shell-arena-remote--rebuild (remote job manifest)
  "Return a panel for REMOTE running JOB, whose laptop arena is MANIFEST."
  (let* ((toplevel (+agent-shell-arena-git-manifest-toplevel manifest))
         (cwd (expand-file-name (+agent-shell-arena-remote-job-prefix job)
                                toplevel))
         (delegate (+agent-shell-arena-remote-job-delegate job)))
    (+agent-shell-pstack-panel--make
     :id (+agent-shell-arena-remote-job-panel job)
     :shape (+agent-shell-arena-remote-job-shape job)
     :delegate-options
     (when delegate
       (list :host (+agent-shell-arena-remote-host remote)
             :isolation 'worktree
             :sandbox (+agent-shell-arena-remote-delegate-job-sandbox delegate)))
     :identity (and delegate
                    (+agent-shell-arena-remote-delegate-job-identity delegate))
     :brief (+agent-shell-arena-remote-job-brief job)
     :timeout (+agent-shell-arena-remote-job-timeout job)
     :rubric (+agent-shell-arena-remote-job-rubric job)
     :judge-timeout (+agent-shell-arena-remote-job-judge-timeout job)
     :phase (+agent-shell-arena-remote-phase remote)
     :cwd (if (file-directory-p cwd) cwd toplevel)
     :arena manifest :remote remote
     :measure-egress (+agent-shell-arena-remote-job-measure-egress job)
     :members (+agent-shell-arena-remote--members job))))

(defun +agent-shell-arena-remote--members (job)
  "Return fresh panel members for JOB's members, of their kinds."
  (mapcar (lambda (member)
            (+agent-shell-pstack-panel--make-member
             :role (+agent-shell-arena-remote-request-member-role member)
             :agent (+agent-shell-arena-remote-request-member-agent member)
             :focus (+agent-shell-arena-remote-request-member-focus member)
             :kind (+agent-shell-arena-remote-request-member-kind member)
             :state 'pending))
          (+agent-shell-arena-remote-job-members job)))

;;;###autoload
(defun +agent-shell-arena-remote-reattach-all ()
  "Resume every remote arena on disk that has not delivered its results."
  (interactive)
  (let ((root +agent-shell-pstack-arena-directory))
    (when (file-directory-p root)
      (dolist (directory (directory-files root t "\\`[^.]"))
        (when (file-exists-p (+agent-shell-arena-remote--record-file directory))
          (condition-case err
              (+agent-shell-arena-remote--restore directory)
            (error (message "pstack arena %s: could not re-attach: %s"
                            (file-name-nondirectory directory)
                            (error-message-string err))))))))
  (+agent-shell-arena-remote--attach-leads)
  (+agent-shell-pstack-panels--refresh))

;;;###autoload
(defun +agent-shell-pstack-arena-reattach (id)
  "Resume polling the remote arena ID, as after it was unreachable."
  (interactive
   (list (completing-read
          "Re-attach arena: "
          (seq-keep (lambda (directory)
                      (when (file-exists-p
                             (+agent-shell-arena-remote--record-file directory))
                        (file-name-nondirectory directory)))
                    (and (file-directory-p +agent-shell-pstack-arena-directory)
                         (directory-files +agent-shell-pstack-arena-directory t
                                          "\\`[^.]")))
          nil t)))
  (let ((remote (or (gethash id +agent-shell-arena-remote--arenas)
                    (+agent-shell-arena-remote--restore
                     (expand-file-name id +agent-shell-pstack-arena-directory))
                    (user-error "Arena %s has nothing left to do" id))))
    (when (eq (+agent-shell-arena-remote-phase remote) 'unreachable)
      (when-let* ((panel (+agent-shell-arena-remote--panel remote)))
        (setf (+agent-shell-pstack-panel-followup panel) nil
              (+agent-shell-pstack-panel-delivered panel) nil))
      (setf (+agent-shell-arena-remote-seen remote) (float-time)
            (+agent-shell-arena-remote-failures remote) 0
            (+agent-shell-arena-remote-delivered remote) nil)
      (+agent-shell-arena-remote--move remote 'remote)
      (+agent-shell-arena-remote--schedule remote 0))
    (+agent-shell-arena-remote--attach-leads)
    remote))

(defun +agent-shell-arena-remote--lead-for (session)
  "Return the live agent shell whose session is SESSION, or nil."
  (when session
    (seq-find (lambda (buffer)
                (and (local-variable-p 'agent-shell--state buffer)
                     (equal session
                            (+agent-shell-arena-remote--session buffer))))
              (buffer-list))))

(defun +agent-shell-arena-remote--attach-leads ()
  "Give each remote arena without a live lead the shell of its session.
Deliver its follow-up there once the shell is ready."
  (maphash
   (lambda (_ remote)
     (when-let* ((panel (+agent-shell-arena-remote--panel remote))
                 ((not (buffer-live-p (+agent-shell-pstack-panel-lead panel))))
                 (lead (+agent-shell-arena-remote--lead-for
                        (+agent-shell-arena-remote-session remote))))
       (setf (+agent-shell-pstack-panel-lead panel) lead)
       (+agent-shell-drive-watch
        lead 'pstack-arena-remote (+agent-shell-arena-remote-id remote)
        (lambda (event)
          (when (memq (plist-get event :kind) '(prompt-ready turn-complete))
            (+agent-shell-pstack-panel--deliver-to lead))))
       (+agent-shell-pstack-panel--deliver-to lead)))
   +agent-shell-arena-remote--arenas))

(defvar +agent-shell-arena-remote--attach-timer nil
  "Timer that looks for the leads of remote arenas.")

(defun +agent-shell-arena-remote-clean (manifest callback)
  "Clean MANIFEST's remote arena on its host, then on the laptop.
Call CALLBACK as `+agent-shell-arena-git-clean' does.  An arena whose
runner still runs is left alone."
  (let* ((directory (+agent-shell-arena-git-manifest-directory manifest))
         (remote (or (gethash (+agent-shell-arena-git-manifest-id manifest)
                              +agent-shell-arena-remote--arenas)
                     (let ((remote (+agent-shell-arena-remote--read-json
                                    (+agent-shell-arena-remote--record-file
                                     directory)
                                    'record)))
                       (setf (+agent-shell-arena-remote-directory remote)
                             directory)
                       remote)))
         (local (lambda ()
                  (remhash (+agent-shell-arena-remote-id remote)
                           +agent-shell-arena-remote--arenas)
                  (+agent-shell-arena-git-clean manifest callback))))
    (if (not (+agent-shell-arena-remote-submitted remote))
        (funcall local)
      (+agent-shell-arena-remote--call
       (+agent-shell-arena-remote-host remote)
       +agent-shell-arena-remote--clean-script
       (list (+agent-shell-arena-remote-id remote)
             (or (+agent-shell-arena-remote-emacs remote) "emacs"))
       :timeout 900
       :callback
       (lambda (output error)
         (let ((answer (+agent-shell-arena-remote--answer
                        output "pstack-arena-clean "))
               (label (+agent-shell-arena-remote--host-label
                       (+agent-shell-arena-remote-host remote))))
           (cond
            (error (funcall callback nil error))
            ((equal answer "done") (funcall local))
            ((equal answer "running")
             (funcall callback nil (format "It still runs on %s; cancel it \
first" label)))
            (t (funcall callback nil (format "%s: %s" label
                                             (or answer output)))))))))))

;;;###autoload
(define-minor-mode +agent-shell-arena-remote-mode
  "Resume remote pstack arenas and deliver their results.
On, it re-attaches every remote arena on disk and looks for the leads of
arenas that lack one, by session id."
  :global t
  :group '+agent-shell-arena-remote
  (when +agent-shell-arena-remote--attach-timer
    (cancel-timer +agent-shell-arena-remote--attach-timer)
    (setq +agent-shell-arena-remote--attach-timer nil))
  (if +agent-shell-arena-remote-mode
      (progn
        (add-hook '+agent-shell-pstack-panel-change-hook
                  #'+agent-shell-arena-remote--sync)
        (add-hook 'kill-emacs-hook #'+agent-shell-arena-remote--note-quitting)
        (setq +agent-shell-arena-remote--attach-timer
              (run-at-time 5 5 #'+agent-shell-arena-remote--attach-leads))
        (+agent-shell-arena-remote-reattach-all))
    (remove-hook '+agent-shell-pstack-panel-change-hook
                 #'+agent-shell-arena-remote--sync)
    (remove-hook 'kill-emacs-hook #'+agent-shell-arena-remote--note-quitting)))

;;;; Runner

(cl-defstruct (+agent-shell-arena-remote--runner
               (:constructor +agent-shell-arena-remote--make-runner)
               (:copier nil))
  "A job a batch Emacs runs on the host.
DIRECTORY is the job folder and ROOT the host state folder.  PHASE is
queued until this runner holds the repository LOCK, then the panel's.
PREVIOUS is the status an earlier runner left unfinished.  PANEL holds
the deadline and how each agent's host login stood.  EXPIRED says the
deadline cancelled the panel.  TRANSCRIPT is a delegate's transcript
writer, a `+agent-shell-arena-remote--recorder'.  QUARANTINE says why
its delegate's writers could not be shown to have stopped, which keeps
the repository's lock held."
  job directory root panel (phase 'queued) lock previous done
  cancelled expired (written "") (beat 0) error ready transcript quarantine)

(defvar +agent-shell-arena-remote--runner nil
  "The runner this batch Emacs runs, or nil.")

(defun +agent-shell-arena-remote--runner-file (runner name)
  "Return file NAME in RUNNER's job folder."
  (expand-file-name name (+agent-shell-arena-remote--runner-directory runner)))

(defun +agent-shell-arena-remote--runner-members (runner)
  "Return RUNNER's members as a status gives them."
  (if-let* ((panel (+agent-shell-arena-remote--runner-panel runner)))
      (mapcar (lambda (member)
                (+agent-shell-arena-remote--make-member
                 :role (+agent-shell-pstack-panel-member-role member)
                 :agent (+agent-shell-pstack-panel-member-agent member)
                 :kind (+agent-shell-pstack-panel-member-kind member)
                 :state (+agent-shell-pstack-panel-member-state member)
                 :capture (+agent-shell-pstack-panel-member-capture member)
                 :error (+agent-shell-pstack-panel-member-error member)
                 :failure-kind (plist-get
                                (+agent-shell-pstack-panel-member-failure
                                 member)
                                :kind)))
              (+agent-shell-pstack-panel-members panel))
    (mapcar (lambda (member)
              (+agent-shell-arena-remote--make-member
               :role (+agent-shell-pstack-panel-member-role member)
               :agent (+agent-shell-pstack-panel-member-agent member)
               :kind (+agent-shell-pstack-panel-member-kind member)
               :state 'pending))
            (+agent-shell-arena-remote--members
             (+agent-shell-arena-remote--runner-job runner)))))

(defun +agent-shell-arena-remote--runner-status (runner &optional results)
  "Write RUNNER's status if it changed or a heartbeat is due.
RESULTS says the results are written, which finishes the status."
  (let* ((panel (+agent-shell-arena-remote--runner-panel runner))
         (manifest (and panel (+agent-shell-pstack-panel-arena panel)))
         (job (+agent-shell-arena-remote--runner-job runner))
         (delegate (eq (+agent-shell-arena-remote-job-shape job) 'delegate))
         (phase (cond (results 'finished)
                      ;; Finished only once the results are written.
                      ((null panel)
                       (+agent-shell-arena-remote--runner-phase runner))
                      ((eq (+agent-shell-pstack-panel-phase panel) 'finished)
                       (if delegate 'finalizing 'judging))
                      ((and delegate
                            (eq (+agent-shell-pstack-panel-phase panel)
                                'delegating)
                            (memq (+agent-shell-pstack-panel-member-capture
                                   (car (+agent-shell-pstack-panel-members
                                         panel)))
                                  '(stopped capturing captured failed)))
                       'finalizing)
                      ((memq (+agent-shell-pstack-panel-phase panel)
                             '(running interrupting))
                       'preparing)
                      (t (+agent-shell-pstack-panel-phase panel))))
         (status (+agent-shell-arena-remote--make-status
                  :id (+agent-shell-arena-remote-job-id job)
                  :shape (+agent-shell-arena-remote-job-shape job)
                  :transcript (+agent-shell-arena-remote--runner-transcript-state
                               runner)
                  :phase phase :pid (emacs-pid) :heartbeat 0
                  :deadline (and panel
                                 (+agent-shell-pstack-panel-deadline panel))
                  :arena (and manifest
                              (+agent-shell-arena-git-manifest-id manifest))
                  :members (+agent-shell-arena-remote--runner-members runner)
                  :results (and results t)
                  :error (+agent-shell-arena-remote--runner-error runner)
                  :logins (and panel (+agent-shell-pstack-panel-logins panel))))
         (text (+agent-shell-arena-remote-to-json 'status status))
         (now (float-time)))
    (when (or results
              (and (not (+agent-shell-arena-remote--runner-done runner))
                   (or (not (equal text
                                   (+agent-shell-arena-remote--runner-written
                                    runner)))
                       (>= (- now (+agent-shell-arena-remote--runner-beat runner))
                           +agent-shell-arena-remote-heartbeat))))
      (setf (+agent-shell-arena-remote-status-heartbeat status) now
            (+agent-shell-arena-remote--runner-written runner) text
            (+agent-shell-arena-remote--runner-beat runner) now)
      (+agent-shell-arena-remote--write-json
       (+agent-shell-arena-remote--runner-file runner "status.json")
       'status status))))

(defun +agent-shell-arena-remote--lock-file (root repository)
  "Return the lock folder for REPOSITORY under ROOT."
  (expand-file-name (concat "locks/" (substring (sha1 (file-truename repository))
                                                0 16))
                    root))

(defun +agent-shell-arena-remote--lock-owner (lock)
  "Return the pid holding LOCK, or nil when unknown."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (expand-file-name "owner" lock))
      (car (read-from-string (buffer-string))))))

(defun +agent-shell-arena-remote-acquire (lock id)
  "Take LOCK, a folder, for job ID; return whether it was taken.
A lock whose owner no longer runs is stale and is taken over: it is
renamed aside first, so only one waiter can win it.  A quarantined one,
whose delegate's writers were never shown to stop, never is; see
`+agent-shell-arena-remote--quarantine'."
  (make-directory (file-name-directory (directory-file-name lock)) t)
  (condition-case nil
      (progn (make-directory lock)
             (with-temp-file (expand-file-name "owner" lock)
               (insert (format "%d %S\n" (emacs-pid) id)))
             t)
    (file-already-exists
     (let ((owner (+agent-shell-arena-remote--lock-owner lock)))
       (when (and (natnump owner)
                  (not (file-exists-p (expand-file-name "quarantine" lock)))
                  (/= owner (emacs-pid))
                  (not (process-attributes owner)))
         (let ((aside (format "%s.stale-%d" (directory-file-name lock)
                              (emacs-pid))))
           (when (ignore-errors (rename-file lock aside) t)
             (delete-directory aside t)
             (+agent-shell-arena-remote-acquire lock id))))))))

(defun +agent-shell-arena-remote--release (lock)
  "Give up LOCK if this Emacs holds it."
  (when (and lock (eql (+agent-shell-arena-remote--lock-owner lock) (emacs-pid)))
    (delete-directory lock t)))

(defun +agent-shell-arena-remote--runner-panel-for (runner)
  "Return a panel without a lead for RUNNER's job, registered under its id."
  (let* ((job (+agent-shell-arena-remote--runner-job runner))
         (repository (+agent-shell-arena-remote--repository job))
         (cwd (expand-file-name (+agent-shell-arena-remote-job-prefix job)
                                repository))
         (delegate (+agent-shell-arena-remote-job-delegate job))
         (panel (+agent-shell-pstack-panel--make
                 :id (+agent-shell-arena-remote-job-panel job)
                 :shape (+agent-shell-arena-remote-job-shape job)
                 :delegate-options (+agent-shell-arena-remote--job-options job)
                 :identity (and delegate
                                (+agent-shell-arena-remote-delegate-job-identity
                                 delegate))
                 :brief (+agent-shell-arena-remote-job-brief job)
                 :timeout (+agent-shell-arena-remote-job-timeout job)
                 :rubric (+agent-shell-arena-remote-job-rubric job)
                 :judge-timeout (+agent-shell-arena-remote-job-judge-timeout job)
                 :phase 'preparing
                 :cwd (file-name-as-directory
                       (if (file-directory-p cwd) cwd repository))
                 :members (+agent-shell-arena-remote--members job)
                 :measure-egress
                 (+agent-shell-arena-remote-job-measure-egress job)
                 :runner
                 (when delegate
                   (list :observer
                         (lambda (_member event)
                           (+agent-shell-arena-remote--record-transcript
                            runner event))
                         :lease
                         (lambda (member)
                           (+agent-shell-arena-remote--runner-lease
                            runner member))))
                 :sink (lambda (panel)
                         (+agent-shell-arena-remote--runner-finish
                          runner panel)))))
    (when (and delegate (not (+agent-shell-arena-remote--runner-transcript runner)))
      (let ((recorder (+agent-shell-arena-remote--recorder-open
                       (+agent-shell-arena-remote--runner-file
                        runner "transcript/"))))
        (setf (+agent-shell-arena-remote--runner-transcript runner) recorder)
        (+agent-shell-arena-remote--record
         recorder "notice"
         (if (+agent-shell-arena-remote--runner-previous runner)
             "The runner restarted; it recovers what the delegate left and \
never reruns it."
           "The runner started."))))
    (puthash (+agent-shell-pstack-panel-id panel) panel
             +agent-shell-pstack-panel--panels)
    (setf (+agent-shell-arena-remote--runner-panel runner) panel)))

(defun +agent-shell-arena-remote--lease-file (runner role)
  "Return RUNNER's durable launch record for member ROLE."
  (+agent-shell-arena-remote--runner-file runner (format "leases/%s.json" role)))

(defun +agent-shell-arena-remote--runner-lease (runner member)
  "Return the process lease RUNNER's delegate MEMBER starts under.
Under systemd it is a scope of its own, named for the job and role; a
local host's runner records the adapter's pid and start time instead."
  (let ((role (+agent-shell-pstack-panel-member-role member)))
    (list :file (+agent-shell-arena-remote--lease-file runner role)
          :unit (when (+agent-shell-arena-remote--runner-scoped-p runner)
                  (format "pstack-delegate-%s-%s"
                          (+agent-shell-arena-remote-job-id
                           (+agent-shell-arena-remote--runner-job runner))
                          role)))))

(defun +agent-shell-arena-remote--repository (job)
  "Return the host checkout JOB names, as an absolute folder name."
  (let ((repository (+agent-shell-arena-remote-job-repository job)))
    (file-name-as-directory
     (expand-file-name (if (string-prefix-p "~/" repository)
                           (concat (getenv "HOME") (substring repository 1))
                         repository)))))

(defun +agent-shell-arena-remote--runner-start (runner)
  "Start RUNNER's arena now that it holds the repository's lock.
Its deadline is fixed now.  The panel checks every agent's host login
before any member starts, as a local arena's does."
  (let ((job (+agent-shell-arena-remote--runner-job runner))
        (panel (+agent-shell-arena-remote--runner-panel-for runner))
        (previous (+agent-shell-arena-remote--runner-previous runner)))
    (setf (+agent-shell-arena-remote--runner-phase runner) 'preparing
          (+agent-shell-pstack-panel-deadline panel)
          (+ (float-time) (+agent-shell-arena-remote-job-deadline job)))
    (condition-case err
        (if previous
            (+agent-shell-arena-remote--recover panel previous)
          (let* ((sparse (+agent-shell-arena-remote-job-sparse job))
                 (base (+agent-shell-arena-git-base-at
                        (+agent-shell-arena-remote--repository job)
                        (+agent-shell-arena-remote-job-base job)
                        (+agent-shell-arena-remote-job-prefix job)
                        (when sparse
                          (list :cone (+agent-shell-arena-remote-sparse-cone
                                       sparse)
                                :patterns
                                (+agent-shell-arena-remote-sparse-patterns
                                 sparse))))))
            (if (+agent-shell-pstack-panel--delegate-p panel)
                (progn
                  (when-let* ((refusal (+agent-shell-arena-remote--delegate-refusal
                                        runner)))
                    (user-error "%s" refusal))
                  (+agent-shell-pstack-panel--provision-delegate panel base))
              (+agent-shell-pstack-panel--provision panel base))))
      (error (+agent-shell-pstack-panel--fail-preparation
              panel (error-message-string err))))
    (+agent-shell-arena-remote--runner-status runner)))

(defun +agent-shell-arena-remote--delegate-refusal (runner)
  "Return why RUNNER's host cannot run its delegate, or nil.
Its readiness file must list the delegate capability, a sandboxed
delegate needs a sandbox backend, and under systemd each member runs in
a scope of its own, so systemd-run must be there."
  (let ((job (+agent-shell-arena-remote--runner-job runner)))
    (cond
     ((not (+agent-shell-arena-remote--delegate-ready-p
            (+agent-shell-arena-remote--runner-ready runner)))
      "The host's readiness file does not list the capability \"delegate\":1, so its setup has not proved it can run delegates.")
     ((and (+agent-shell-arena-remote-delegate-job-sandbox
            (+agent-shell-arena-remote-job-delegate job))
           (not +agent-shell-sandbox-backend))
      (format "A sandboxed delegate needs a sandbox, and this host runs on %s."
              system-type))
     ((and (+agent-shell-arena-remote--runner-scoped-p runner)
           (not (executable-find "systemd-run")))
      "The host has no systemd-run, so the delegate could not run in a systemd scope of its own; Emacs never runs it uncontained."))))

(defun +agent-shell-arena-remote--runner-scoped-p (_runner)
  "Return whether this runner starts each delegate in a systemd scope.
A runner started by systemd does, as the launch script says."
  (equal (getenv "PSTACK_ARENA_LAUNCHER") "systemd"))

(defun +agent-shell-arena-remote--report-text (directory role)
  "Return ROLE's report text from the panel reports in DIRECTORY, or nil."
  (let ((file (expand-file-name (concat role ".md") directory)))
    (when (file-readable-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun +agent-shell-arena-remote--recover (panel previous)
  "Report what the runner that left status PREVIOUS made, as PANEL.
Candidates still unsettled were cut off, so their work is captured as
timed out; the judge does not run."
  (let* ((arena (+agent-shell-arena-remote-status-arena previous))
         (manifest
          (and arena
               (ignore-errors
                 (+agent-shell-arena-git-read-manifest
                  (expand-file-name arena +agent-shell-pstack-arena-directory)))))
         (reports (expand-file-name (+agent-shell-pstack-panel-id panel)
                                    +agent-shell-pstack-panel-directory)))
    (if (+agent-shell-pstack-panel--delegate-p panel)
        (+agent-shell-arena-remote--recover-delegate panel previous manifest)
      (+agent-shell-arena-remote--recover-arena panel previous manifest
                                                reports))))

(defun +agent-shell-arena-remote--recover-arena (panel previous manifest
                                                       reports)
  "Recover arena PANEL as `+agent-shell-arena-remote--recover' says.
MANIFEST is its host arena, or nil, and REPORTS its reports folder."
  (if (or (null manifest)
            (eq (+agent-shell-arena-remote-status-phase previous) 'preparing))
        (progn
          (when manifest
            (+agent-shell-arena-git-clean manifest #'ignore))
          (+agent-shell-pstack-panel--fail-preparation
           panel "The runner stopped while it prepared the arena, so no \
candidate ran.  Request the arena again."))
      (setf (+agent-shell-pstack-panel-arena panel) manifest)
      (dolist (member (+agent-shell-pstack-panel-members panel))
        (let* ((role (+agent-shell-pstack-panel-member-role member))
               (old (seq-find (lambda (old)
                                (equal role (+agent-shell-arena-remote-member-role
                                             old)))
                              (+agent-shell-arena-remote-status-members
                               previous)))
               (state (if old (+agent-shell-arena-remote-member-state old)
                        'pending))
               (settled (memq state +agent-shell-pstack-panel--settled))
               (candidate (eq (+agent-shell-pstack-panel-member-kind member)
                              'candidate))
               (git-role (+agent-shell-arena-git-role manifest role)))
          (setf (+agent-shell-pstack-panel-member-text member)
                (+agent-shell-arena-remote--report-text reports role))
          (cond
           (settled (setf (+agent-shell-pstack-panel-member-state member) state
                          (+agent-shell-pstack-panel-member-error member)
                          (+agent-shell-arena-remote-member-error old)
                          (+agent-shell-pstack-panel-member-failure member)
                          (+agent-shell-arena-remote--failure old)))
           (candidate
            (setf (+agent-shell-pstack-panel-member-state member) 'timeout
                  (+agent-shell-pstack-panel-member-error member)
                  "The runner stopped while this candidate ran, so its work \
was captured as it stood when the runner restarted."))
           (t
            (setf (+agent-shell-pstack-panel-member-state member) 'error
                  (+agent-shell-pstack-panel-member-error member)
                  "The runner stopped before the judge finished.")))
          (when candidate
            (if (and git-role (+agent-shell-arena-git-role-git-dir git-role))
                (setf (+agent-shell-pstack-panel-member-capture member) 'stopped)
              (setf (+agent-shell-pstack-panel-member-capture member) 'failed
                    (+agent-shell-pstack-panel-member-result member)
                    (list :error "The candidate never started."))))))
      (setf (+agent-shell-pstack-panel-phase panel) 'capturing)
      (+agent-shell-arena-remote--drop-checkouts
       manifest (+agent-shell-arena-git-manifest-roles manifest)
       (lambda () (+agent-shell-pstack-panel--advance panel)))))

(defun +agent-shell-arena-remote--recover-delegate (panel previous manifest)
  "Recover delegate PANEL, whose earlier runner left status PREVIOUS.
MANIFEST is its host manifest, or nil.  A delegate is never run again.
Whatever its durable launch record owns is stopped first; until that is
proved, the work stays and the repository's lock stays quarantined.  A
member that had not settled counts as done only when its saved final
response has a matching successful-final marker, and as interrupted
otherwise.  Then collection resumes, finishing a sandboxed delegate's
one commit from the same intent; see
`+agent-shell-arena-git-finalize-delegate'."
  (let* ((runner +agent-shell-arena-remote--runner)
         (member (car (+agent-shell-pstack-panel-members panel)))
         (role (+agent-shell-pstack-panel-member-role member))
         (lease (and runner (+agent-shell-arena-remote--lease-file runner role)))
         (launched (and lease (file-exists-p lease)))
         (old (car (+agent-shell-arena-remote-status-members previous))))
    (cond
     ((and (null manifest) (not launched))
      (+agent-shell-pstack-panel--fail-preparation
       panel "The runner stopped while it prepared the delegate, before it \
started.  Request the delegate again."))
     ((null manifest)
      (+agent-shell-arena-remote--quarantine
       runner "The runner stopped with a delegate launched but no manifest.")
      (+agent-shell-pstack-panel--fail-preparation
       panel "The runner stopped with the delegate launched but its record \
lost, so Emacs kept the repository locked until someone checks the host."))
     ((not launched)
      ;; Its worktree may exist, but no agent ever ran there.
      (setf (+agent-shell-pstack-panel-arena panel) manifest)
      (+agent-shell-pstack-panel--fail-preparation
       panel "The runner stopped while it prepared the delegate, before it \
started.  Request the delegate again."))
     (t
      (setf (+agent-shell-pstack-panel-arena panel) manifest
            (+agent-shell-pstack-panel-phase panel) 'delegating
            (+agent-shell-pstack-panel-member-capture member) 'stopping)
      (+agent-shell-drive-reap-lease
       lease
       (lambda (survivors)
         (when survivors
           (+agent-shell-arena-remote--quarantine
            runner (format "Processes %s the delegate started could not be \
shown to have stopped." survivors)))
         (let ((final (+agent-shell-pstack-panel--final-text panel member))
               (state (and old (+agent-shell-arena-remote-member-state old))))
           (setf (+agent-shell-pstack-panel-member-text member)
                 (or final (+agent-shell-arena-remote--report-text
                            (expand-file-name
                             (+agent-shell-pstack-panel-id panel)
                             +agent-shell-pstack-panel-directory)
                            role)))
           (cond
            (final (setf (+agent-shell-pstack-panel-member-state member)
                         (if (memq state '(error timeout)) state 'done)
                         (+agent-shell-pstack-panel-member-error member)
                         (and old (+agent-shell-arena-remote-member-error old))
                         (+agent-shell-pstack-panel-member-failure member)
                         (and old (+agent-shell-arena-remote--failure old))))
            ((memq state +agent-shell-pstack-panel--settled)
             (setf (+agent-shell-pstack-panel-member-state member) state
                   (+agent-shell-pstack-panel-member-error member)
                   (+agent-shell-arena-remote-member-error old)
                   (+agent-shell-pstack-panel-member-failure member)
                   (+agent-shell-arena-remote--failure old)))
            (t (setf (+agent-shell-pstack-panel-member-state member) 'error
                     (+agent-shell-pstack-panel-member-error member)
                     "The runner stopped while the delegate ran; Emacs did \
not run it again.")))
           (setf (+agent-shell-pstack-panel-member-capture member) 'stopped
                 (+agent-shell-pstack-panel-member-result member)
                 (when survivors (list :survivors survivors)))
           (+agent-shell-pstack-panel--advance panel))))))))

(defun +agent-shell-arena-remote--quarantine (runner reason)
  "Keep RUNNER's repository lock held because of REASON.
A quarantined lock is never taken over as stale, even after its owner
exits, until someone removes it on the host."
  (when runner
    (setf (+agent-shell-arena-remote--runner-quarantine runner) reason)
    (when-let* ((lock (+agent-shell-arena-remote--runner-lock runner))
                ((file-directory-p lock)))
      (with-temp-file (expand-file-name "quarantine" lock)
        (insert reason "\n")))))

(defun +agent-shell-arena-remote--drop-checkouts (manifest roles done)
  "Remove the judge checkouts ROLES of MANIFEST have, then call DONE.
A recovered capture makes them again."
  (if (null roles) (funcall done)
    (let ((role (car roles))
          (next (lambda ()
                  (+agent-shell-arena-remote--drop-checkouts
                   manifest (cdr roles) done))))
      (if-let* ((checkout (+agent-shell-arena-git-role-checkout role)))
          (+agent-shell-arena-git-command
           manifest (list "worktree" "remove" "--force"
                          (directory-file-name checkout))
           (lambda (_ _)
             (ignore-errors (delete-directory checkout t))
             (setf (+agent-shell-arena-git-role-checkout role) nil)
             (+agent-shell-arena-git-write-manifest manifest)
             (funcall next)))
        (funcall next)))))

(defun +agent-shell-arena-remote--runner-report (runner panel)
  "Return the report of RUNNER's finished PANEL, or of its job when PANEL
is nil."
  (let* ((job (+agent-shell-arena-remote--runner-job runner))
         (manifest (and panel (+agent-shell-pstack-panel-arena panel))))
    (+agent-shell-arena-remote--make-report
     :id (+agent-shell-arena-remote-job-id job)
     :shape (+agent-shell-arena-remote-job-shape job)
     :delegate (and manifest
                    (+agent-shell-arena-git-manifest-delegate manifest)
                    (+agent-shell-arena-remote--outcome manifest))
     :arena (and manifest (+agent-shell-arena-git-manifest-id manifest))
     :base (+agent-shell-arena-remote-job-base job)
     :rejection (or (and panel (+agent-shell-pstack-panel-rejection panel))
                    (+agent-shell-arena-remote--runner-error runner))
     :ready (+agent-shell-arena-remote--runner-ready runner)
     :members
     (if panel
         (mapcar
          (lambda (member)
            (let ((result (+agent-shell-pstack-panel-member-result member)))
              (+agent-shell-arena-remote--make-member
               :role (+agent-shell-pstack-panel-member-role member)
               :agent (+agent-shell-pstack-panel-member-agent member)
               :kind (+agent-shell-pstack-panel-member-kind member)
               :state (+agent-shell-pstack-panel-member-state member)
               :capture (+agent-shell-pstack-panel-member-capture member)
               :text (+agent-shell-pstack-panel-member-text member)
               :error (+agent-shell-pstack-panel-member-error member)
               :failure-kind (plist-get
                              (+agent-shell-pstack-panel-member-failure member)
                              :kind)
               :commit (plist-get result :commit)
               :stat (plist-get result :stat)
               :failure (plist-get result :error))))
          (+agent-shell-pstack-panel-members panel))
       (+agent-shell-arena-remote--runner-members runner)))))

(defun +agent-shell-arena-remote--outcome (manifest)
  "Return what MANIFEST's delegate left on this host, as its report says."
  (let* ((delegate (+agent-shell-arena-git-manifest-delegate manifest))
         (branch (concat "refs/heads/"
                         (+agent-shell-arena-git-delegate-branch manifest)))
         (role (car (+agent-shell-arena-git-manifest-roles manifest)))
         (finalization (+agent-shell-arena-git-delegate-finalization delegate))
         (dirty (+agent-shell-arena-git-delegate-dirty delegate))
         (state (plist-get finalization :state)))
    (+agent-shell-arena-remote--make-outcome
     :branch branch
     :tip (+agent-shell-arena-git--read-in
           (+agent-shell-arena-git-manifest-git-dir manifest)
           (+agent-shell-arena-git-manifest-toplevel manifest)
           "rev-parse" "--verify" "-q" branch)
     :commits (mapcar (pcase-lambda (`(,oid . ,subject))
                        (+agent-shell-arena-remote--make-commit
                         :oid oid :subject subject))
                      (+agent-shell-arena-git-delegate-commits delegate))
     :workspace (or (and role (+agent-shell-arena-git-role-worktree role)) "")
     :sandbox (+agent-shell-arena-git-delegate-sandbox delegate)
     :dirty-paths (seq-take dirty 10) :dirty-count (length dirty)
     :finalization (if (memq state '(empty committed blocked)) state 'none)
     :retained-reason (when (eq state 'blocked)
                        (plist-get finalization :reason)))))

(defun +agent-shell-arena-remote--runner-files (runner panel)
  "Write results.files, the files RUNNER's fetch sends, for PANEL.
They are the report, the results bundle when there is one, each
member's egress record that exists and a delegate's transcript, by
their names in the job folder."
  (let* ((directory (+agent-shell-arena-remote--runner-directory runner))
         (transcript (expand-file-name "transcript/" directory))
         (files (append
                 (list "report.json")
                 (when (file-exists-p (expand-file-name "results.bundle"
                                                        directory))
                   (list "results.bundle"))
                 (when (file-exists-p (expand-file-name "epoch" transcript))
                   (cons "transcript/epoch"
                         (mapcar (lambda (segment)
                                   (file-relative-name
                                    (+agent-shell-arena-remote--segment-file
                                     transcript segment)
                                    directory))
                                 (+agent-shell-arena-remote--segments
                                  transcript))))
                 (seq-keep
                  (lambda (member)
                    (when-let* ((file (+agent-shell-pstack-panel-member-egress-file
                                       member))
                                ((file-regular-p file))
                                ((not (file-symlink-p file)))
                                ((file-in-directory-p file directory)))
                      (file-relative-name file directory)))
                  (and panel (+agent-shell-pstack-panel-members panel))))))
    (with-temp-file (expand-file-name "results.files" directory)
      (insert (mapconcat (lambda (file) (concat file "\n")) files "")))))

(defun +agent-shell-arena-remote--runner-finish (runner panel)
  "Write RUNNER's results for finished PANEL, then its final status.
The results bundle and report land before the status that names them.
An arena's bundle holds its result refs over the base; a delegate's its
branch over the base's parent, so even a branch still at the base
travels.  Nothing here commits."
  (let* ((manifest (and panel (+agent-shell-pstack-panel-arena panel)))
         (delegate (and manifest (+agent-shell-arena-git-manifest-delegate
                                  manifest)))
         (base (and manifest (+agent-shell-arena-git-manifest-base manifest)))
         (refs (cond
                ((null manifest) nil)
                (delegate
                 (let ((branch (concat "refs/heads/"
                                       (+agent-shell-arena-git-delegate-branch
                                        manifest))))
                   (when (+agent-shell-arena-git--read-in
                          (+agent-shell-arena-git-manifest-git-dir manifest)
                          (+agent-shell-arena-git-manifest-toplevel manifest)
                          "rev-parse" "--verify" "-q" branch)
                     (list branch))))
                (t (seq-keep (lambda (role)
                               (and (+agent-shell-arena-git-role-commit role)
                                    (+agent-shell-arena-git-role-ref role)))
                             (+agent-shell-arena-git-manifest-roles manifest)))))
         (exclude (if delegate
                      (when-let* ((parent (+agent-shell-arena-git--read-in
                                           (+agent-shell-arena-git-manifest-git-dir
                                            manifest)
                                           (+agent-shell-arena-git-manifest-toplevel
                                            manifest)
                                           "rev-parse" "--verify" "-q"
                                           (concat base "^"))))
                        (list (concat "^" parent)))
                    (and base (list (concat "^" base)))))
         (bundle (+agent-shell-arena-remote--runner-file runner "results.bundle"))
         (report
          (lambda (&optional error)
            (when error
              (setf (+agent-shell-arena-remote--runner-error runner) error))
            (+agent-shell-arena-remote--write-json
             (+agent-shell-arena-remote--runner-file runner "report.json")
             'report (+agent-shell-arena-remote--runner-report runner panel))
            (when-let* ((recorder (+agent-shell-arena-remote--runner-transcript
                                   runner)))
              (+agent-shell-arena-remote--flush-transcript recorder)
              (+agent-shell-arena-remote--record
               recorder "notice" "The runner published the delegate's results."))
            (when (or (+agent-shell-arena-remote-job-measure-egress
                       (+agent-shell-arena-remote--runner-job runner))
                      (eq 'delegate (+agent-shell-arena-remote-job-shape
                                     (+agent-shell-arena-remote--runner-job
                                      runner))))
              (+agent-shell-arena-remote--runner-files runner panel))
            (+agent-shell-arena-remote--runner-status runner t)
            (unless (+agent-shell-arena-remote--runner-quarantine runner)
              (+agent-shell-arena-remote--release
               (+agent-shell-arena-remote--runner-lock runner)))
            (setf (+agent-shell-arena-remote--runner-done runner) t))))
    (if (null refs) (funcall report)
      (let ((partial (concat bundle ".partial")))
        (+agent-shell-arena-git-command
         manifest
         (append (list "bundle" "create" "-q" partial) refs exclude)
         (lambda (_ error)
           (if error (funcall report (format "The results bundle failed: %s"
                                             error))
             (rename-file partial bundle t)
             (funcall report))))))))

(defun +agent-shell-arena-remote--runner-cancel-reason (runner)
  "Return the reason in RUNNER's cancel marker, or nil without one."
  (let ((file (+agent-shell-arena-remote--runner-file runner "cancel")))
    (when (file-exists-p file)
      (let ((reason (string-trim (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
        (if (string-empty-p reason) "The arena was cancelled." reason)))))

(defun +agent-shell-arena-remote--runner-tick (runner)
  "Check RUNNER's cancel marker, lock and deadline, and beat its heart."
  (unless (+agent-shell-arena-remote--runner-done runner)
    (let* ((panel (+agent-shell-arena-remote--runner-panel runner))
           (reason (+agent-shell-arena-remote--runner-cancel-reason runner))
           (deadline (and panel (+agent-shell-pstack-panel-deadline panel))))
      (cond
       ((and reason (not panel)
             (not (+agent-shell-arena-remote--runner-previous runner)))
        (+agent-shell-arena-remote--runner-unstarted runner reason))
       ((and reason panel
             (not (+agent-shell-arena-remote--runner-cancelled runner)))
        (setf (+agent-shell-arena-remote--runner-cancelled runner) t)
        (+agent-shell-pstack-panel-cancel panel reason))
       ((null panel)
        (when (+agent-shell-arena-remote-acquire
               (+agent-shell-arena-remote--runner-lock runner)
               (+agent-shell-arena-remote-job-id
                (+agent-shell-arena-remote--runner-job runner)))
          (+agent-shell-arena-remote--runner-start runner)))
       ((and deadline
             (> (float-time)
                (+ deadline +agent-shell-pstack-panel-arena-grace)))
        (setf (+agent-shell-arena-remote--runner-error runner)
              "The runner gave up on its capture after its deadline.")
        (+agent-shell-pstack-panel--finish panel))
       ((and deadline (> (float-time) deadline)
             (not (+agent-shell-arena-remote--runner-expired runner)))
        (setf (+agent-shell-arena-remote--runner-expired runner) t)
        (+agent-shell-pstack-panel-cancel
         panel (format "The arena reached its %d-second deadline."
                       (+agent-shell-arena-remote-job-deadline
                        (+agent-shell-arena-remote--runner-job runner)))
         'timeout))))
    (when-let* ((recorder (+agent-shell-arena-remote--runner-transcript runner)))
      (+agent-shell-arena-remote--flush-transcript recorder))
    (unless (+agent-shell-arena-remote--runner-done runner)
      (+agent-shell-arena-remote--runner-status runner))))

(defun +agent-shell-arena-remote--runner-unstarted (runner reason)
  "End RUNNER's job, cancelled for REASON before any candidate started."
  (let ((panel (+agent-shell-arena-remote--runner-panel-for runner)))
    (dolist (member (+agent-shell-pstack-panel-members panel))
      (setf (+agent-shell-pstack-panel-member-state member) 'error
            (+agent-shell-pstack-panel-member-error member) reason))
    (setf (+agent-shell-pstack-panel-phase panel) 'finished)
    (+agent-shell-arena-remote--runner-finish runner panel)))

(defun +agent-shell-arena-remote--refuse (&rest _)
  "Refuse to prompt, since nobody can answer a runner."
  nil)

(defun +agent-shell-arena-remote-runner-main (directory)
  "Run the job in DIRECTORY on this host to its end; return an exit code.
Load runner-init.el from the host's state folder first: it configures the
agents.  The runner never prompts, never asks the host to stay awake,
rewrites its status as it goes, and recovers a job an earlier runner left
unfinished."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (root (file-name-directory
                (directory-file-name
                 (file-name-directory (directory-file-name directory)))))
         (init (expand-file-name "runner-init.el" root))
         (+agent-shell-pstack-arena-directory (expand-file-name "arenas/" root))
         (+agent-shell-pstack-panel-directory
          (expand-file-name "reports/" directory))
         (+agent-shell-pstack-arena-extra-deny-reads
          (mapcar (lambda (name) (expand-file-name name root))
                  '("jobs/" "locks/" "runner-init.el")))
         (agent-shell-transcript-file-path-function nil)
         (agent-shell-inhibit-system-sleep nil)
         (shell-maker-prompt-before-killing-buffer nil)
         (job (+agent-shell-arena-remote--read-json
               (expand-file-name "job.json" directory) 'job))
         (ready (or (when-let* ((file (+agent-shell-arena-remote-job-ready-file
                                       job)))
                      (ignore-errors
                        (+agent-shell-arena-remote--read-json file 'ready)))
                    (+agent-shell-arena-remote-job-ready job))))
    (advice-add 'yes-or-no-p :override #'+agent-shell-arena-remote--refuse)
    (advice-add 'y-or-n-p :override #'+agent-shell-arena-remote--refuse)
    (when-let* ((bin (and ready (+agent-shell-arena-remote-ready-adapters-bin
                                 ready))))
      (push bin exec-path)
      (setenv "PATH" (concat bin path-separator (getenv "PATH"))))
    (when (file-exists-p init) (load init nil t))
    (let* ((previous (ignore-errors
                       (+agent-shell-arena-remote--read-json
                        (expand-file-name "status.json" directory) 'status)))
           (runner (+agent-shell-arena-remote--make-runner
                    :job job :directory directory :root root :ready ready
                    :lock (+agent-shell-arena-remote--lock-file
                           root (+agent-shell-arena-remote--repository job))
                    :previous (and previous
                                   (not (memq (+agent-shell-arena-remote-status-phase
                                               previous)
                                              '(queued finished)))
                                   previous)))
           (+agent-shell-pstack-arena-profiles
            (when-let* ((profile (+agent-shell-arena-remote-job-profile job)))
              (list (cons (+agent-shell-arena-remote--repository job)
                          (+agent-shell-arena-remote-host-profile profile)))))
           (timer nil))
      (setq +agent-shell-arena-remote--runner runner)
      (add-hook '+agent-shell-pstack-panel-change-hook
                (lambda () (+agent-shell-arena-remote--runner-status runner)))
      (if (and previous
               (eq (+agent-shell-arena-remote-status-phase previous) 'finished))
          0
        (with-temp-file (expand-file-name "pid" directory)
          (insert (format "%d\n" (emacs-pid))))
        (unwind-protect
            (progn
              (+agent-shell-arena-remote--runner-status runner)
              (setq timer (run-at-time 0 +agent-shell-arena-remote-tick
                                       #'+agent-shell-arena-remote--runner-tick
                                       runner))
              ;; Killing a login refresh midway could lose the rotated login.
              (while (or (not (+agent-shell-arena-remote--runner-done runner))
                         (when-let* ((panel
                                      (+agent-shell-arena-remote--runner-panel
                                       runner)))
                           (+agent-shell-pstack-panel-login-check panel)))
                (accept-process-output nil 0.5))
              ;; Let the stops and discards the finish started run first.
              (let ((until (+ (float-time) 3)))
                (while (< (float-time) until)
                  (accept-process-output nil 0.1)))
              0)
          (when timer (cancel-timer timer))
          (unless (+agent-shell-arena-remote--runner-quarantine runner)
            (+agent-shell-arena-remote--release
             (+agent-shell-arena-remote--runner-lock runner))))))))

(defun +agent-shell-arena-remote-runner-batch ()
  "Run the job whose folder is the next command-line argument, then exit."
  (let ((directory (pop command-line-args-left)))
    (kill-emacs
     (condition-case err
         (+agent-shell-arena-remote-runner-main directory)
       (error
        (message "pstack arena runner: %s" (error-message-string err))
        1)))))

(defun +agent-shell-arena-remote-runner-clean-batch ()
  "Clean the host's side of the job whose folder is the next argument.
Remove its arena through the manifest and its base ref, then exit."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (pop command-line-args-left))))
         (root (file-name-directory
                (directory-file-name
                 (file-name-directory (directory-file-name directory)))))
         (+agent-shell-pstack-arena-directory (expand-file-name "arenas/" root))
         (job (+agent-shell-arena-remote--read-json
               (expand-file-name "job.json" directory) 'job))
         (status (ignore-errors
                   (+agent-shell-arena-remote--read-json
                    (expand-file-name "status.json" directory) 'status)))
         (arena (and status (+agent-shell-arena-remote-status-arena status)))
         (arena-directory (and arena (expand-file-name
                                      arena +agent-shell-pstack-arena-directory)))
         (failure nil)
         (done nil))
    (if (not (and arena-directory (file-directory-p arena-directory)))
        (setq done t)
      (+agent-shell-arena-git-clean
       (+agent-shell-arena-git-read-manifest arena-directory)
       (lambda (_left error) (setq failure error done t))))
    (while (not done) (accept-process-output nil 0.1))
    (let ((default-directory (+agent-shell-arena-remote--repository job)))
      (call-process "git" nil nil nil "update-ref" "-d"
                    (format "refs/pstack-arena/%s/base"
                            (+agent-shell-arena-remote-job-id job))))
    (when failure (message "%s" failure))
    (kill-emacs (if failure 1 0))))

(provide '+agent-shell-arena-remote)
;;; +agent-shell-arena-remote.el ends here
