# Remote delegates: architecture sketch

## Caller usage

### Repository profile

```elisp
(add-to-list
 '+agent-shell-pstack-arena-profiles
 '("~/src/big-repo/"
   :host (coder . "example-workspace")
   :host-directory "~/src/big-repo"
   :delegate (:sandbox t)))
```

Both arenas and delegates now inherit `:host`. Omit `:sandbox` for an
ordinary full-access delegate using the host's own logins. To keep delegates
on the laptop while arenas use that host, set `:delegate (:host local)`.
The symbol `local` is an override, distinct from `(local . DIRECTORY)`, the
registered host backend used to exercise remote jobs with local processes.

The delegate options are `:host` (inherit when absent, or the symbol `local`),
`:isolation` (`worktree` by default or `branch`), and `:sandbox` (nil by
default, strictly t or nil). An inherited host pair uses the existing
`:host-directory`; another execution host belongs in the repository profile.
Duplicate or unknown keys are errors. Resolve these options once, before
creating any branch or uploading anything. With an effective host, reject
branch isolation with:

```
Remote delegates require :isolation worktree; use :delegate (:host local)
to use :isolation branch on the laptop.
```

Sandboxing also applies to local delegates. Local branch isolation remains
available, including with sandboxing; its existing clean-checkout and
restore rules still apply. A sandbox backend is required only when sandboxing
is requested. A remote delegate needs the backend on its host, not its laptop.

### Lead's request

The ACP panel request stays version 1. Execution policy comes from the
repository profile, never from agent-supplied JSON:

````text
```pstack-panel
{"version":1,"shape":"delegate","brief":"Add upload rate limiting and tests",
 "timeout":2700,"members":[{"role":"rate-limit","agent":"codex"}]}
```
````

Exactly one delegate, no judge or rubric. A remote delegate starts at the
laptop's HEAD, as a local delegate does, on `pstack/delegate/<panel-id>`.
It does not inherit the lead's dirty files. The existing base-bundle
negotiation transports that commit and its required history.

For a sandboxed delegate, Emacs adds this instruction to its brief:

````text
Implement and verify the task. Leave your changes uncommitted. End your final
message with exactly one block of this form, using this repository's commit
style. Give the subject and body for the single commit Emacs will make.
Do not include attribution trailers or any text after the block.

```pstack-commit
{"version":1,"subject":"[uploads] Add rate limiting",
 "body":"Reject requests above the configured upload budget.\n\nCover refill and rejection behavior."}
```
````

An unsandboxed delegate keeps the existing implement/test/commit instruction.
It receives the laptop user's git identity in its process environment.

### C-c P view

```text
3f9a0cd2e701  delegate  remote: delegating
  Lead: *agent-shell codex*
  Host: coder example-workspace  Emacs 31.1  sandbox bwrap
  Workspace on host: .../arenas/3f9a0cd2e701-12345678/rate-limit
  Branch: pstack/delegate/3f9a0cd2e701
  rate-limit  codex  running  transcript (updated 12s ago; RET)
```

The remote delegate member line targets a transcript descriptor, even after
the member finishes. RET opens its read-only buffer. A separate `Report:`
line targets the saved final report. `c` records the existing durable cancel
request. `x` cleans the finished job and host worktree through existing remote
cleanup; it does not discard commits. A dirty host worktree requires the
existing cleanup confirmation to name that data explicitly.

Queued, cancelling, fetching, unreachable, and finalization-blocked states
must be visible. An unreachable line says when it was last seen and offers
the existing reattach command. A laptop manifest directory must not be
labelled as the remote workspace. Local delegates retain their live shell.

### Transcript buffer

```text
*pstack delegate 3f9a0cd2e701 transcript*
coder example-workspace | rate-limit (codex) | running | read-only
Last poll: 14:05:30 UTC | retained history: 8 MiB | following

14:05:01  assistant  I will inspect the upload handler.
14:05:04  tool       Read upload handler
14:05:07  assistant  The limiter belongs before request dispatch.
...
```

This is a `special-mode` buffer, not an agent-shell or a remote terminal.
RET never creates a process. `g` schedules the existing poll immediately;
`q` closes the window. Keep point and window position when the reader scrolls
back; follow only windows already at the end. Show disconnects, restarts,
rotation gaps, truncation and final status as explicit transcript notices.
Rendering uses literal text, without ANSI/OSC handling, executable links,
file-local variables or evaluation of host output.

### Follow-up

```text
pstack panel 3f9a0cd2e701 (delegate) has finished: rate-limit (codex): done.
Ran on: coder example-workspace, Emacs 31.1, sandbox bwrap.
Branch: pstack/delegate/3f9a0cd2e701, 1 commit since <start>:
  <oid> [uploads] Add rate limiting
Workspace on host: .../arenas/3f9a0cd2e701-12345678/rate-limit
Report: <local report path>
<the delegate's final response>
To review: git log <start>..pstack/delegate/3f9a0cd2e701
To merge: git merge --ff-only pstack/delegate/3f9a0cd2e701,
or push it and open a pull request.
```

Use the existing delegate follow-up formatter and delivery queue. Add where
it ran, sandbox mode, and relevant retention or fetch errors. An unsandboxed
delegate can return several commits. A blocked sandbox finalization says
`No commit created`, why, and exactly where the dirty host worktree remains.
It must not describe an unfetched branch as available for merge.

## Data shapes

### One versioned job protocol

Extend `+agent-shell-arena-remote--kinds` to wire version 2. Keep job.json,
status.json, report.json, remote.json, code-by-hash shipping, the submit/poll/
fetch/clean calls, and the host-kind registry. No delegate transport, runner
entry point, remote state directory, or host-kind registry is added.

The v2 job is a tagged union with common members:

```text
job = existing common transport/base/profile fields
    + shape: arena | delegate
    + members: [{role, agent, kind: candidate | judge | delegate, focus?}]
    + arena fields: rubric, judge_timeout, deadline (duration)
    | delegate: {sandbox: boolean, identity: {name, email}}
      expires_at: absolute UTC epoch seconds
```

`id` remains the laptop manifest id; `panel` remains the panel id. The host
manifest can have its own suffix. Derive the delegate branch from `panel`,
using the existing branch-name function; never accept a destination ref or
shell fragment from the request. `base`, `prefix`, `sparse`, `profile`,
`repository`, `prerequisite`, `code`, `ready`, `ready_file` and
`measure_egress` retain their meanings. Delegates do not request egress
measurement in this change.

V2 replaces job `candidates` and `judge` with `members`. Use one new small
request-member struct; keep the existing status/report member struct, which
describes observations rather than requests. A version-1 job normalizes its
candidates and judge to members with shape arena at the decoding boundary.
Do not keep two execution paths or infer a delegate from missing arena fields.
An unknown shape, a delegate with multiple members, an arena with a delegate
payload, or an absent delegate identity is a field-specific error. The v2
writer emits only the new shape; it never emits both old and new member lists.
Existing readers already reject a newer wire version, avoiding accidental
execution of a delegate as an arena. The ACP request version is unrelated.

Status and report gain `shape`; their member-kind enum gains `delegate`, and
status phases gain `delegating` and `finalizing`. Terminal phase `finished`
still means artifacts were published, not that the task succeeded. Report
gets an optional delegate outcome:

```text
delegate = {branch, tip, commits: [[oid, subject], ...], workspace,
            sandbox, dirty_paths: [relative-path, ...], dirty_count,
            finalization: none | empty | committed | blocked,
            message_error?, retained_reason?}
```

Bound path lists to ten entries plus their total, as local follow-ups do.
The report's existing member text remains the authoritative final response.
Recompute commit lists and verify the tip locally after fetching. Do not trust
host path fields for local filesystem operations or copy the host manifest
over the laptop manifest.

### Ownership of durable state

| State | Sole writer and authority |
| --- | --- |
| Effective delegate policy | Git/profile module resolves once; job stores the frozen policy |
| Laptop identity | Git module reads repository-effective `git config user.name/user.email` on the laptop before submission |
| Host worktree, branch, commit intent | Git module, in the existing host manifest's delegate record |
| Task phase, final text/provenance, settled state | Panel module; persist text and its completion marker through its existing report writer |
| Host process containment | Drive module's process lease; runner consumes that lease for recovery |
| Job, heartbeat, artifacts, host lease | Remote runner; status is a projection of panel/manifest state |
| Transport phase, cancel request, delivery | Existing laptop remote.json owner |
| Host transcript segments | Remote runner's event recorder |
| Download cursor/cache, transcript windows | Remote module on laptop; cursor advances only after cache is durable |

Append slots to existing structs, bump the panel schema if its layout changes,
and version the manifest when adding the durable commit record. Read old
manifests with sandbox nil and no finalization. Read v1 remote records as
arenas, with no transcript cursor. Existing in-memory working functions stay
intact in this sketch; their eventual changes are listed below.

The delegate manifest gains sandbox and a finalization record:

```text
finalization = nil
  | {state: prepared, parent, tree, message, identity, author_date,
     committer_date, oid?}
  | {state: committed, ...same intent..., oid}
  | {state: empty | blocked, reason?}
```

`nil` means not attempted; it does not mean permission to rerun the agent.
The manifest owns intent and commit outcome. Status/report only copy its
summary. Identity contains no credentials. Validate nonempty name/email,
no NUL/CR/LF and no invalid git identity delimiters; never fall back to the
host identity, environment identity, or the arena capture identity.

### Host readiness

Keep ready.json's existing version and boot freshness checks; add optional
capabilities rather than a second readiness file:

```json
{"version":1,"written_at":"2026-10-28T00:00:00Z",
 "emacs":{"path":"/usr/local/bin/emacs","version":"31.1"},
 "git":{"path":"/usr/bin/git","version":"2.50.0"},
 "capabilities":{"delegate":1,"process_lease":1,
                 "sandbox_backends":["bwrap"]}}
```

Merge these with today's adapter, node and sandbox tool entries. The
delegate capability promises a smoke-tested full-access adapter, named git
worktrees, bundle/fetch, commit-tree, compare-and-swap update-ref, and a durable
process lease that can stop that adapter's descendants after runner death.
`process_lease` is required for both delegate policies. No host git user or
signing key is required. Git must support the options already used locally.
Only sandboxed delegates require a listed sandbox backend, usable user/network
namespaces on Linux, or sandbox-exec on macOS, and the existing host token
admission check. Unsandboxed delegates require their adapter's ordinary host
login, without token projection or an expiry admission check.

The probe additionally returns the host's current epoch time. Reject delegate
submission if its difference from the laptop's probe round-trip midpoint
exceeds five seconds (or the round trip cannot establish that bound).
This makes `expires_at` meaningful, including before a runner takes its lock.
Record tool versions in the outcome as today. Capability claims are checked
again by the shipped runner against its selected agent and execution mode;
an old readiness file still supports arenas but refuses delegates with setup
guidance. No Python, jq, remote Emacs server, or interactive shell is required.

## Module map and signatures

| Module | Extend existing behavior | New sketch signatures |
| --- | --- | --- |
| `+agent-shell-arena-git.el` | `--check-profile` validates new keys; delegate manifest codec persists commit intent; provisioning accepts the already resolved profile; `finish-delegate` continues to list commits/dirty paths | `delegate-options`, `user-identity`, `finalize-delegate` |
| `+agent-shell-pstack-panel.el` | `--prepare-delegate` routes by effective host; `--provision-delegate` accepts a supplied base on the host; brief, sandbox admission, stop, collect, report writer, follow-up and view understand delegate policy | `--delegate-commit-message` |
| `+agent-shell-arena-remote.el` | Existing codecs, `--job`, launch, `--members`, `--rebuild`, runner panel/start/recover/report/finish, land, poll and cleanup dispatch on shape | `--record-transcript`, `--poll-page`, `--cache-transcript`, `transcript-buffer` |
| `+agent-shell-drive.el` | `start` accepts explicit environment and an optional durable process lease; event parser exposes text chunks and tool progress to existing watchers; stop consumes/reaps the same lease | No parallel delegate launcher or watcher API |
| `+agent-shell-sandbox.el` | `create` accepts protected read-only paths; both backend renderers enforce those after profile write allowances | `--check-protected-paths` |

Full function names in the Elisp use their module prefixes. Each new function
has an inert `Not implemented` body. Existing working definitions are not
replaced. No new module or eager require is needed. Remote is still loaded
only when an effective host is selected.

`delegate-options` returns a normalized plist with explicit `:host`,
`:isolation` and `:sandbox`, and checks their cross-field constraints. It is
the only resolver: launch, prompt and follow-up read that frozen decision.
On the host, clear routing keys before entering panel provisioning, so the
reconstructed panel cannot submit itself recursively. Job shape determines
which existing provisioner runs; no generic lifecycle plugin registry is
needed for two known shapes. Transport code never branches on member count.

`user-identity` reads the laptop's repository config asynchronously, with
`--includes` so conditional repository settings apply. Inherited
GIT_AUTHOR/COMMITTER variables must not substitute for missing config. Local
sandboxed delegates use the same read. Remote unsandboxed delegates receive
all four GIT_AUTHOR_NAME, GIT_AUTHOR_EMAIL, GIT_COMMITTER_NAME and
GIT_COMMITTER_EMAIL entries on the ACP client's environment, overriding host
and profile identity settings. Extend drive's existing client-maker setup to
apply them to this shell and its subprocesses, without changing global
`process-environment`, writing shared git config, or projecting credentials.
This supplies the requested identity to ordinary commits; an unsandboxed
agent with full access can deliberately override it.

`--delegate-commit-message` accepts only one terminal `pstack-commit` JSON
block from the authoritative final response, with version 1, a nonempty
single-line subject and a string body. Limit its UTF-8 size to 64 KiB,
reject control characters other than body newlines/tabs and reject git
trailer blocks, duplicate keys or duplicate blocks. The prompt requests the
repository's own subject/body style;
Emacs does not invent a prefix, rewrite prose, or append agent trailers.
Missing/invalid output blocks finalization; partial assistant text, a tool
result or a transcript record is never a substitute.

Extend `+agent-shell-pstack-arena-sandbox` to cover a delegate's worktree or
local branch root, reusing the same profile and login registry. Read-only
protection covers the worktree's `.git` pointer, its actual git directory and
the common git directory, even when a profile's `:write` overlaps them.
The sandbox helper validates canonical protected paths; macOS emits final
write denials, Linux emits final/deepest read-only mounts. No refresh login
or runner job state becomes writable. Local branch protection also covers
the checkout's real `.git` directory. Tokens remain access-only, prepared
fresh by the existing backend after admission. Profile dependency/file
provisioning stays in the existing git and sandbox builders: dependencies
come from the host checkout, and remote `:files` use the existing plain-profile
placeholder serialization. Materialize those resolved files for an
unsandboxed remote delegate too, without creating a token sandbox. Local
unsandboxed delegates retain today's copies from the lead's checkout.

## Lifecycle and recovery

### Submit and run

1. After the interrupted lead stops, locate its repository with
   `git-repository`, resolve policy, and fix the delegate's deadline once.
   Read identity, preserving the HEAD base and sparse patterns. Remote
   preparation does not call arena's dirty-tree snapshot function.
2. The existing remote launch creates the laptop manifest (with a delegate
   record), pins the base, negotiates a prerequisite, ships the same
   hash-addressed code, and submits job.json plus the base bundle.
   `expires_at` is the original deadline: wake, preparation, upload and
   repository queueing all consume the delegate's timeout. Refuse an expired
   submission. Arena duration budgeting remains unchanged.
3. Host runner-main reads the same job kind, checks readiness and cancel,
   and acquires the same repository lock used by arenas. One lock serializes
   arenas and delegates sharing a checkout. While queued, publish heartbeats
   and enforce expiry; a restart cannot grant another timeout.
4. Rebuild a panel with its actual shape and one member of kind delegate.
   Use the supplied base with `git-base-at`; call the existing delegate
   provisioner, creating its named branch in a worktree of the host checkout.
   Persist the manifest before the worktree is made, as today. A branch
   already present from another job is a refusal, never reset/reused.
5. For sandboxed delegates, use existing admission and sandbox creation with
   the deadline plus capture grace. For unsandboxed delegates, use
   full-access policy and the host's existing logins. Before launching either,
   persist its process lease; attach transcript recording before sending the
   brief. The host panel never has a lead or a nested remote object.
6. Member completion saves final text atomically through the existing report
   writer, before stop/collection. Extend that writer to publish a small
   `<role>.final.json` marker after the text: its hash, the turn stop reason
   and whether it came from a successful terminal turn-complete event.
   Partial/cancelled/error reports have no successful-final marker. Recovery
   requires the marker's hash to match; a crash between the two writes
   conservatively leaves work uncommitted. The panel is the sole marker
   writer, and a marker contains no copy of the text. Stop via drive-stop for
   a sandbox, or drive-stop-unconfined for full access. Both use the durable
   lease to prove
   that this member's writers ended; never kill by repository directory.
7. Collection finalizes a sandboxed delegate, then uses finish-delegate to
   collect commits and dirty paths. An unsandboxed delegate goes straight to
   finish-delegate. No candidate capture, arena identity, or judge runs.
8. Runner-finish bundles the delegate branch and publishes report/artifact
   inventory before final status. Poll/fetch imports it, updates the laptop
   manifest's delegate outcome, saves the local report/transcript, and invokes
   the existing delegate formatter and once-only follow-up delivery.

### The one commit

The panel owns ordering; the git module owns the mutation. Extend
`--collect-delegate` to call `git-finalize-delegate` only after stop has
confirmed no writers. Pass the identity frozen at preparation and the parsed
message. The same call is used locally and on a host. Runner-finish only
packages results; it never makes a commit.

The panel passes a message to the finalizer only when the successful-final
marker matches the report; text alone cannot prove a turn completed. The
finalizer checks the recorded git paths, branch and HEAD; a sandboxed
delegate must still be on its branch at the recorded base. Unexpected commits
or ref movement block it and remain preserved. Snapshot with the existing
temporary-index machinery, including sparse behavior and existing exclusions
for shared links, generated paths and agent-shell files. Restore excluded
generated paths to base content. Do not stage against the lead's index.
Two stable trees are required, as in arena capture. An unchanged tree records
`empty` and produces no artificial commit.

For changes, persist the prepared intent (parent, tree, exact message, user
identity, fixed author/committer dates) atomically. Run commit-tree outside the
sandbox with that identity, signing disabled, hooks disabled, explicit UTF-8
encoding and no generated trailers. Reuse snapshot machinery, not
`git--commit`, which hardcodes `pstack arena`. Persist the returned oid before
compare-and-swap `update-ref <branch> <oid> <base>`. Update only this branch.
Reconcile the worktree's index to the committed tree without overwriting its
files, then mark committed. This prevents the old index appearing as staged
reversals after a temporary-index commit, and lets local branch restore work.

After a crash, the saved oid at the branch tip means the operation already
landed: reconcile the index and finish. If it still points to base, reuse the
saved oid, or recreate it from the fixed intent if none was saved, and CAS.
Any other tip blocks finalization. A crash before oid persistence can leave
an unreachable object, but never another reachable commit. Repeated collection
and recovery therefore produce zero or one new commit, with the message and
both identities exactly as requested. Filters or disk errors block the step
and retain the worktree; they do not trigger an arena-style fallback commit.

### Transcript format, rotation and polling

The runner writes a projection of normalized drive events to UTF-8 JSONL in
`jobs/<id>/transcript/<segment>.jsonl`. Each record has `seq`, `time`, `kind`
and `text`; assistant chunks, tool titles/status, and lifecycle notices are
enough. Do not serialize ACP permission payloads, auth responses, environment
variables, full tool arguments, or hidden reasoning. Actual agent/tool text
can contain repository data; store job files with private user permissions.
The final report is independent of this lossy live projection.

Extend drive's event parser to emit assistant text chunks and tool progress
even when no preceding assistant paragraph exists; current parsing drops
both cases for watchers. Panel handlers ignore those events unless recording
is enabled. Keep drive as the sole ACP subscription owner. The host panel gets
an optional event observer, installed by runner-panel-for, which receives
normalized member events before lifecycle handling. Its closure calls the
recorder; local panels have no observer. This slot is the only event connection
between panel and runner and requires the normal appended-slot/schema bump.
The recorder never adds an ACP subscriber or scrapes a shell buffer.

Use monotonically numbered segments, at most 1 MiB each and eight retained
segments (8 MiB retained, with one transient rollover segment). Split text at
UTF-8 boundaries into records of at most 16 KiB encoded, counting JSON
escaping. Rotate before a whole record would
overflow; never split a JSON line across segments. The stream has one epoch
per job and increasing sequence numbers across runner restarts. Status adds
only its epoch, oldest/newest available segment and seq; it never embeds the
transcript. Prune oldest segments after publishing the new retained bounds.
On recovery truncate any incomplete final JSON line, recover the last seq,
then append a runner-restarted notice. Final report and commit intent files
are never rotated with transcript history.

Extend the existing poll request with an optional cursor `(epoch, segment,
byte-offset)` and a fixed maximum page of 64 KiB of complete JSONL records.
The v2 poll body is a single bounded JSON envelope containing the existing
alive/cancel/status observations and an optional transcript page:

```text
tail = {epoch, from: cursor, next: cursor, gap: boolean,
        records: [...], more: boolean}
```

The private `--poll-page` helper reads status and retained segments under the
job directory and produces that envelope. The existing poll script invokes
it in a short batch Emacs from the already shipped code, using the readiness
Emacs. This costs one short Emacs process per poll and avoids another host
dependency or quoting JSON in shell. Preserve v1 poll parsing for older
arena jobs; v2 arenas have no tail. Cancel marker handling stays first, even
if the transcript page cannot be read. A rotation race returns a gap/retry,
never a malformed status or a failed cancel acknowledgment.

`--cache-transcript` validates ids, cursor continuity, sequence order and
bounds, writes the page to the laptop cache, then saves the cursor through
the remote record owner. Replay after a crash compares sequence numbers and
does not duplicate text. A missing/rotated cursor resumes at the oldest
retained record with a visible gap. Page requests remain in the existing
per-host call queue. When `more`, schedule the next poll after yielding, with
at most one catch-up page between ordinary host operations; cancel, heartbeat
and fetch must not starve. No buffer owns a timer or network connection.

Bound the laptop cache and visible buffer to the same 8 MiB of encoded text;
discard complete oldest records and mark the gap. On the final fetch include
the retained segments in results.files, validate and reconcile by epoch/seq,
and cache them before delivery. A missing transcript never blocks commits or
the final report. Opening the buffer after restart uses that disk cache and
reattaches to the same polling stream. Closing it does not stop recording.

### Failure, cancel, timeout and restart

| Event | Behavior and preservation |
| --- | --- |
| Bad policy, missing laptop identity or unready host | Reject before launch; no fallback to local or to a different identity |
| Provisioning/admission failure before launch | Existing preparation cleanup removes only recorded partial resources and an unchanged empty branch; publish rejection |
| User cancel or lead killed | Persist cancel locally, send existing marker, stop the member, then collect; do not delete its commits |
| Laptop quits or network drops | Host keeps running to its fixed deadline; laptop reattaches from remote.json; quitting does not cancel |
| Timeout, including host queue | Host stops editing at expires_at, with existing bounded capture grace for stop/finalize/publish; keep commits and dirty files |
| Stop cannot establish quiescence | Block finalization, retain worktree and process lease, report survivors; never commit a changing tree or reset a branch |
| Runner dies while member runs | Restart recovers resources, stops the recorded lease, marks task interrupted/error, collects; never reruns the brief |
| Runner dies after final response | Read the atomic member report and existing manifest; finish stopping and resume the same finalizer, even if task outcome is interrupted |
| Runner dies after commit or while packaging | Verify journal and branch tip, reuse the commit, rebuild artifacts, publish terminal status last |
| Results fetch fails or laptop ref collides | Keep artifacts and host branch; retry fetching, never launch again or overwrite the conflicting branch |

Cancellation/timeout do not authorize an invented commit message. If a valid
authoritative terminal response was saved, finish its single commit after
stopping, even if cancel raced with completion. Without one, a sandboxed
delegate's work stays dirty on the host and finalization is blocked. The
follow-up names the host and recovery path for manual review/commit. An
unsandboxed delegate's existing commits always survive; its dirty files stay
on the host, explicitly distinguished from the fetched branch. A missing
commit message must never cause automatic cleanup of a dirty workspace.

The existing runner restart policy (stale stopped runner, at most two
automatic restarts) applies. A restart is recovery, not a second agent turn.
Do not treat heartbeat absence as proof that children died: current arena
recovery's assumption is insufficient for an unconfined delegate. Extend
drive's stop machinery with a durable process lease before launch. On Linux
use the job's existing service/cgroup with a member-specific child scope;
on macOS/local backend use a small supervisor whose control/owner record
outlives Emacs and tracks the member's process tree/groups. Record an opaque
lease id with a nonce/start identity, not a reusable bare pid. The supervisor
must reap on lost owner and expose quiescence to a restarted runner. It does
not provide an interactive shell or contain ordinary unsandboxed file access.

Readiness advertises that capability only after killing a runner mid-task
and proving its adapter and writers can be reaped. If the supervisor's
identity cannot be verified after a crash, do not signal a possibly reused
pid. Preserve the branch/worktree, mark recovery blocked, and keep the
repository lease quarantined until its writers are confirmed stopped. A
recovery process must acquire that same repository lock before touching the
manifest; a dead runner alone does not make a delegate lock reclaimable.
Consult the durable launch record before interpreting an old `preparing`
status: status can lag a launched member, so that phase alone never authorizes
deleting a worktree or deciding that no delegate ran.
Arena and delegate submissions both honor quarantine. Once writers are
stopped, a failed finalizer can release the repository lock while retaining
the dirty worktree. This is additional host readiness work, not something
today's `kill -0` check already guarantees.

If the hard grace expires, publish the known branch tip and failure reason
without making a speculative commit. Do not drop the job's process lease or
delete files while writers are unaccounted for. A runner restart can later
complete artifact publication; it cannot change an already published terminal
outcome or quietly add a late commit to its branch.

### Fetch and cleanup

Generalize runner-finish's ref selection and `--land`'s mapping, not fetch's
transport. A delegate report names only the derived
`refs/heads/pstack/delegate/<panel>` and its verified tip. Include that branch
in results.bundle. Exclude the base's parent rather than the base for a
delegate bundle (no exclusion for a root base): this includes the base commit
itself, allowing even an unchanged delegate branch to travel through the
same fetch. Arena bundling still excludes base and uses its result refs.

On the laptop verify bundle, shape, panel, expected source ref and oid, and
that base is an ancestor of tip. Fetch into a temporary ref under this job's
existing arena ref namespace, verify, then publish
`refs/heads/pstack/delegate/<panel>` with compare-and-swap. Accept an existing
destination only when it already equals the reported tip. Reject one checked
out in any laptop worktree or moved by the user; report the collision and
retain the fetched temporary ref for recovery. Do not force-fetch into a user
branch. Compute commits and review information locally from the verified tip.
Extend finish-delegate to record the branch's full ref and tip on its role,
without calling git-record's arena-ref constructor. Remote landing writes
those observations and the commit list into the laptop delegate record; it
does not call the local worktree collector on a host path. Select the delegate
role by name, since the laptop manifest also has the reserved base role.

The laptop manifest keeps local paths only; host workspace/dirty information
is report data. Its remote cleanup runs the existing host clean operation,
then local cleanup. Successful fetched delegate branches remain. An empty
branch can be dropped only under the existing base-equality/no-worktree
rules, and after host workspace cleanup; an empty branch with retained dirty
work must remain recoverable. The host never removes a committed delegate
branch as part of generic arena ref cleanup. Transcripts follow job retention:
explicit cleanup removes host segments; the laptop's saved final report and
bounded transcript remain available as panel reports.

## Test plan (for implementation; not run in this sketch)

1. Extend profile/delegate tests: inherited host, absent vs explicit local,
   an inherited registered local host pair, default nil sandbox, strict
   boolean/duplicate/unknown-key errors, and remote branch rejection before
   any provisioning. Verify local branch restore with sandboxing.
2. Extend wire tests: normalize v1 arenas, round-trip both v2 shapes, reject
   invalid cross-shape fields, preserve old result readers, and reattach both
   shapes. Assert shape survives every panel rebuild and manifest rewrite.
3. In scratch git repos, use deliberately different host/laptop identities.
   Verify both author and committer for sandbox commits and multiple ordinary
   unsandboxed commits. Missing laptop config must refuse. Check conditional
   includes, spaces/non-ASCII names and invalid identity characters. Ensure
   neither arena capture identity nor agent trailers appear in delegate commits.
4. Finalizer tests assert observable history: zero commits for no changes;
   one commit for tracked/untracked changes and deletes; exact subject/body,
   identity and parent; sparse paths and generated exclusions; no real lead
   index changes. Crash at intent/object/oid/CAS/index/journal boundaries,
   recover repeatedly, and still find exactly one reachable commit. A moved
   branch, invalid message, surviving writer or filter failure retains work.
   A partial report containing a plausible commit block still cannot commit;
   exercise crashes before/after the matching successful-final marker.
5. ACP fixtures on `(local . DIRECTORY)` exercise each policy through the
   shipped runner. Assert host worktree and named branch, no host checkout
   change, profile provisioning, host-login selection, remote sandbox with no
   laptop sandbox, correct result fetch, ordinary delegate follow-up plus host,
   and exactly one follow-up after laptop restart. Keep arena fixture coverage.
6. Transcript fixtures include chunks without tool calls, tools without text,
   Unicode splits, huge/escaped records, partial final lines, rotation during
   polling, cursor replay, gaps, restart, malformed pages and final catch-up.
   Assert fixed size limits, no credential payloads, no buffer command sending,
   scroll preservation, and RET before launch/during run/after finish/reattach.
7. Kill real fixture writers and runner independently on Linux and macOS.
   Confirm durable leases stop descendants/groups with changed working
   directories, protect against pid reuse, and quarantine unresolved writers.
   Test cancel before upload/queued/admission/running/finalizing, offline cancel,
   timeout without a final message, repeated runner restarts, hard grace, and
   no new commit after terminal publication. No host login refresh is killed
   mid-rotation; reuse the existing admission cancellation rule.
8. Bundle tests include root/unchanged bases and multi-commit delegates. Verify
   destination checked-out/moved/colliding branches are untouched, fetched
   temporary refs survive retries, no laptop branch is published before
   verification, and x preserves committed branches while naming dirty loss.
   Carry forward current LFS refusal/flagging and sparse-result checks.

The eventual gate is the focused delegate/remote/git/sandbox fixtures followed
by `emacs -Q --batch -l test/run-config.el`. This design turn runs syntax
inspection only, as requested.

## Rationale and rejected alternatives

The current implementation already separates the hard parts: panel owns
delegate settlement and follow-up, git owns the manifest and worktree,
sandbox owns credential projection and OS rules, drive owns shells and stops,
and remote owns reliable job delivery. The chosen design generalizes the
job boundary and reuses those owners. Its public additions are one policy
resolver, identity lookup, a durable finalizer, and a transcript view; the
other signatures are private boundary operations.

I compared two whole shapes: running the existing delegate panel inside the
shared runner, and having transport supervise a special remote delegate
process directly. The shared runner wins because the direct-process shape
would duplicate stop/collect, cancellation, branch recovery and follow-up
rules inside transport. A second delegate job protocol loses for the same
reason. Making a delegate a one-candidate arena also loses: it would silently
inherit arena snapshot bases, capture authorship, judging and private refs.

Keep one common members representation in new jobs and translate v1 once at
the boundary. Retaining parallel candidates/delegate-member encodings would
make every runner/rebuild path learn two requests. Do not introduce a general
shape-plugin framework for two cases; explicit shape dispatch keeps validation
and ownership visible.

I rejected finalizing on the laptop: the host owns the dirty worktree, so it
would require a second change-transfer/capture path and delay the commit
beyond host completion. I rejected parsing commit prose from a rolling
transcript or synthesizing cancellation messages: either can commit with text
the delegate never proposed. A terminal structured block makes missing intent
an explicit retained-work outcome. I rejected changing shared host git config,
which would race with other work; per-shell environment and explicit finalizer
identity keep attribution scoped to this task.

The main tradeoff is operational: a remote full-access delegate needs durable
process ownership beyond today's arena readiness. We accept that setup work
and a conservative recovery block in exchange for never committing a live
tree or restarting an agent that might still be writing. Bounded transcript
retention and a short batch Emacs per poll also cost history and polling
overhead, respectively, in exchange for bounded memory and no additional
installed host program. Start implementation with v2 codecs/policy resolution
and finalizer crash fixtures; then wire the shared lifecycle and live view.

## Synthesis amendments (lead, binding over the sections above)

Candidate B is the base. These changes override it wherever they conflict.

1. **Process containment on Linux hosts is a systemd scope.** The runner
   already runs as a `systemd-run --user` service with `MemoryMax` and
   `OOMPolicy=continue`. Start each delegate member's adapter in its own
   transient scope under that user manager (for example `systemd-run
   --user --scope --unit=pstack-delegate-JOB-ROLE --collect ...`), and
   record the unit name in the durable launch record. Stopping, timeout,
   cancel and recovery after runner death stop that unit, which kills
   every descendant however it detached; quiescence is the unit being
   inactive. No separate supervisor process, and no macOS supervisor:
   remote delegates need a Linux host kind; the `local` test host kind
   keeps today's process-tree stop. A missing or failed scope refuses the
   delegate with a fixed reason rather than running it uncontained.
2. **No clock-skew check.** Deadlines stay durations from the runner's
   start, as arenas use today; drop `expires_at` and the probe's epoch time.
3. **Readiness.** Add one capability, `"delegate":1`, to ready.json; the
   sandbox backend and tool versions are already reported. An old
   ready.json without it keeps running arenas and refuses delegates with
   setup guidance. The private host setup playbook will be updated to
   write it; nothing in this repo may name a specific host.
4. **Grafts from candidate A.** Factor the existing delegate commit-list
   query into one `delegate-log` helper used by local collection and
   remote landing. Poll faster while a delegate's transcript buffer is
   visible, through the existing polling owner, and back to the normal
   cadence when it is not. Keep A's note that the current watcher only
   exposes tool-call boundaries and final text, as context for the drive
   change B proposes.
5. **Tests.** In addition to B's plan: inside each sandbox backend, a
   sandboxed delegate's attempts to run `git commit`, to write a ref
   directly, and to write the git config or a profile fail; then Emacs
   creates exactly one commit authored and committed as the laptop
   identity shipped in the job, with the delegate's message, on
   pstack/delegate/ID. A remote delegate whose scope is stopped mid-run
   leaves no process behind and its branch intact.
