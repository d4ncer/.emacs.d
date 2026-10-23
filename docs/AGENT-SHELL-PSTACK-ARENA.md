# pstack arena panels: design

Status: proposed, revision 2. Nothing here is implemented yet. It extends the
panels in `docs/AGENT-SHELL-PSTACK.md`. Revision 1 relied on a permission
responder. Interrogate panel `30662dcfd102` broke that, and this revision
replaces it with an OS sandbox. The triage at the end maps each finding to
the change it caused.

## Goal

A lead running the pstack `arena` skill asks Emacs for candidates the same
way it asks for `how` and `interrogate` panels. Emacs runs arena's Fan out
and Cross-judge phases across vendors. The lead keeps Frame, Pick, Graft and
Verify. For the lead, an arena is one request and one follow-up. That
follow-up names each candidate's result commit, its diff against a shared
base, its rationale, and the judge's scores against a rubric no candidate
saw.

v1 supports macOS only, and only leads inside an ordinary git repository.

## Ground truth

These facts come from how panel `d40612ff9781`, interrogate panel
`30662dcfd102` (both under `var/pstack-panels/`), and probes run on this
machine.

- **No permission responder can confine a candidate.** An allowed `emacs -Q
  --batch -l payload.el` or `mix test` runs code the candidate wrote, with
  the user's privileges. Two reviewers raised this independently.
- **Codex's request shapes defeat path and escalation checks.** A command
  approval can need more authority without carrying `additionalPermissions`
  or a `url`. A file-change approval omits rename destinations, and its
  location list can be empty.
- **`sandbox-exec` confines a whole process tree.** A profile that denies
  writes outside a root blocked a direct `touch` and a write from a child
  `emacs --batch` (probe). It also blocked `emacsclient` from the Emacs
  server socket (probe). HTTPS to `api.anthropic.com` worked once the
  `mDNSResponder` socket was allowed (probe).
- **Sandboxes do not nest.** `sandbox-exec` inside `sandbox-exec` fails with
  `sandbox_apply: Operation not permitted` (probe). Codex in
  `workspace-write` uses Seatbelt itself, so it cannot run inside ours in
  that mode.
- **Killing a shell does not stop its writers.** A backgrounded `touch`
  survived its parent's exit and wrote the file 2 seconds later (probe).
  Inside the sandbox, the write still stayed in the root.
- **Dependency directories link back to the lead.**
  `elpaca/builds/agent-shell/agent-shell.el` is an absolute symlink into
  the lead's `elpaca/sources/` (verified). `cp -c -R` keeps such links.
- **A mode change can go unseen.** `current_mode_update` updates session
  state without emitting an event (verified, `agent-shell.el:3478`).
- **Killing a shell from its own `turn-complete` corrupts upstream state.**
  The rest of upstream's callback then runs in whichever buffer is current.
  Today's code defers the kill with `run-at-time 0` for this reason
  (verified, `lisp/+agent-shell-pstack-panel.el:388`).
- **A member is "done" whenever its turn ends.** That holds whatever the
  stop reason (verified, `lisp/+agent-shell-pstack-panel.el:419`).

## Isolation

The boundary is the OS sandbox. Each candidate's agent adapter starts under
`sandbox-exec` with a generated profile. Everything the agent runs,
including test processes, background jobs, hooks and Lisp, inherits the
profile. Inside the sandbox, the agent runs with its own checks off. Codex
uses `agent-full-access`, so it never calls Seatbelt itself, and Claude Code
uses `bypassPermissions`. The responder is not a security boundary in this
design.

**The profile for a candidate with root ROOT:**

- **Allow by default**, so the agent can read the toolchain and system
  files.
- **Deny writes** everywhere except:
  - ROOT;
  - the candidate's private `TMPDIR`;
  - the candidate's private agent config directory, set through
    `CLAUDE_CONFIG_DIR` or `CODEX_HOME`, so it cannot plant hooks or
    settings in the user's real config;
  - `/dev`.
- **Deny writes to `ROOT/.git`**, the worktree's pointer file, so the
  candidate cannot redirect Emacs's later git commands.
- **Deny reads** of:
  - the lead's checkout, except the shared dependency paths;
  - `+agent-shell-pstack-panel-directory`;
  - every other candidate's root.

  This keeps the rubric in the lead's transcript and the other candidates'
  work out of reach, so withholding the rubric is enforced, not just a
  convention.
- **Deny unix-socket connections** except `/private/var/run/mDNSResponder`.
  This blocks `emacsclient` and other local daemons.
- **Allow outbound TCP**, which the agent's own API needs. Exfiltration over
  the network stays possible, and v1 accepts that.

**Dependencies** come in two kinds, configured per project in
`+agent-shell-pstack-arena-dependencies`:

- **Shared** paths, such as `elpaca`, are symlinked into ROOT from the lead.
  The profile makes them read-only, so a candidate can load them but not
  change them.
- **Private** paths, such as `_build`, are cloned with `cp -c -R`. Every
  symlink in the clone that points into the lead's checkout is rewritten to
  the clone's own copy. A private path needs this because candidates write
  build output into it.

**The workspace root** is
`+agent-shell-pstack-arena-directory`/`<arena-id>`/`<role>`/, outside
`~/.emacs.d`. The default is `~/.local/state/pstack-arena/`. Claude Code
reads CLAUDE.md files from parent directories, so a workspace under
`~/.emacs.d` would load this repository's instructions for every project.
`<arena-id>` is the panel id plus a random suffix, so a request repeated
after a restart never collides with leftovers. A lead in a subdirectory
gets candidates that start in the same subdirectory of their worktrees.

## Request

The request is a `pstack-panel` block like this:

```json
{"version":1,"shape":"arena","brief":"The task, identical for every candidate",
 "rubric":["3 to 6 gradeable criteria, shown only to the judge"],
 "timeout":2400,"judge_timeout":900,
 "members":[{"role":"opus","agent":"claude-code"},
            {"role":"codex","agent":"codex","focus":"optional design direction"}],
 "judge":{"agent":"codex"}}
```

- **`members`.** These are 1 to 4 candidates. `focus` names a design
  direction. The task stays identical.
- **`rubric`.** This is required, with 3 to 6 strings.
- **`judge`.** By default it is the first read-only agent whose identifier
  differs from the lead's, which approximates arena's "prefer a different
  model family".
- **Rejections.** Emacs rejects an arena, with a reason the lead can act
  on, in any of these cases:
  - the lead is not in a git repository, or `HEAD` is unborn;
  - the repository has submodules, uses Git LFS, or uses a sparse checkout;
  - the platform is not macOS;
  - an agent has no sandboxed launch configuration.

## Lifecycle

Phases belong to the panel. Kinds and the `pending` state belong to members.
Only the current phase's members are subject to expiry and abandonment.

```
panel:     preparing ─▶ candidates ─▶ capturing ─▶ judging ─▶ finished
               │            │             │           │
               └─ failure ──┴─ lead killed or no usable candidate ─▶ finished

candidate: pending ▶ starting ▶ waiting ▶ running ▶ stopping ▶ captured
                                            └▶ error | timeout ▶ stopping ▶ captured
judge:     pending ▶ starting ▶ waiting ▶ running ▶ done | error | timeout
```

- **Preparing.** Emacs does git and clone work in async processes, never in
  an event handler, so it doesn't freeze. The candidates' timer starts only
  when preparing ends. A request found mid-turn waits for the lead's
  cancelled `turn-complete` before capturing the base, so the lead's
  interrupted edit has landed first.
- **Stopping.**
  1. The kill is deferred out of the candidate's own event callback, as
     today.
  2. The adapter runs in its own process group, which Emacs signals with
     TERM, then KILL after a grace period.
  3. Emacs waits for the group to empty.

  A process that left the group can still write to ROOT. The capture and
  the judge below don't depend on ROOT after this point.
- **Capturing** records each candidate's result commit (see Snapshots).
- **Judging.** The judge reads an immutable checkout of each result commit,
  made outside every candidate's root, never the live worktree. Its timer
  is `judge_timeout`.
- **Transitions are idempotent.** Each one sets the phase before starting
  the next member and arms the timer before that member can fail. A member
  that fails to start can't re-enter a finished panel.
- **Stop reasons count.**
  - A turn ending `cancelled`, `refusal`, `max_tokens` or
    `max_turn_requests` settles as `error` with that reason.
  - An `error` event settles a member only while it is `running` and its
    prompt request failed.
  - The fix applies to every shape.
- **A usable candidate** has a result commit that differs from the base and
  a settled state of `captured` from a completed turn. A timed-out candidate
  is still captured and shown to the judge, marked as timed out. A dropout
  produced no change, and the follow-up says so. With no usable candidate,
  the judge never starts.
- **Killing the lead** captures what the candidates produced, so their work
  is not lost. It never starts the judge.

**Reloading a changed struct.** The panel and member structs only ever gain
slots at the end. They also carry a schema number. On load, the file drops
any in-memory panel whose schema differs, with a message. This avoids the
silent breakage that hit the live session when a slot was added.

## Snapshots and results

Emacs runs every git command with an explicit `--git-dir` and `--work-tree`
taken from its own records, never discovered from the candidate's tree. It
also passes `-c core.hooksPath=/dev/null` and `-c core.fsmonitor=false`.

- **Base.**
  1. Resolve `HEAD` once.
  2. Copy the lead's real index to a temporary file, so force-added files
     and the tracked set carry over.
  3. Run `git add -A` against it through `GIT_INDEX_FILE`.
  4. Run `write-tree`, then `commit-tree -p <resolved HEAD>`.
  5. Repeat steps 2 to 4 until two consecutive trees match, up to three
     attempts.

  The lead's own index never changes.
- **Candidate result.** Emacs captures the same way from the worktree, with
  the base as the parent. It records the commit under
  `refs/pstack-arena/<arena-id>/<role>`. That is outside `refs/heads`, so
  the lead's branch lists stay clean, and the ref keeps the commit from
  garbage collection.
- **Manifest.** `<arena-id>/manifest.json` records:
  - the repository's git directory;
  - the lead's root and subdirectory;
  - the base commit;
  - each role's worktree path, result commit and final state.

  Cleanup trusts the manifest, not path or ref prefixes.

## The judge and the follow-up

- **The judge's brief** holds:
  - the task;
  - the rubric;
  - for each candidate, its label, final state, immutable checkout path,
    `git diff --stat <base> <result>`, and rationale.

  The judge scores each criterion for each candidate and recommends a base.
  It runs read-only.
- **The follow-up** gives, for each candidate:
  - its state;
  - its result commit and ref;
  - its diff stat and rationale;
  - a ready command, `git diff --binary <base> <result>`, whose output
    applies to the lead's current tree with `git apply --3way`.

  It also includes the judge's report and arena's Pick, Graft and Verify
  instructions. It never suggests `git checkout <ref> -- PATH`, which would
  overwrite edits the lead made after the base.

## Cleanup

`x` in the panels view and the command `+agent-shell-pstack-arena-clean`
both read manifests. For each worktree, cleanup checks that it is
registered in the manifest's repository, then removes it with `git worktree
remove --force`. Everything worth keeping already lives in the result
commit. Cleanup then deletes the private directories and the
`refs/pstack-arena/<arena-id>/` refs. A ref that no longer points at the
manifest's commit is left alone and reported. Nothing is removed
automatically, because grafting can take days.

## Experiments before implementation

1. **Agents under the profile.**
   - Start `claude-agent-acp` with `bypassPermissions` and `codex-acp` with
     `agent-full-access`, each under the profile with a private config
     directory.
   - Check that authentication works: Claude Code keeps credentials in the
     Keychain, and Codex in `auth.json`.
   - Check that a prompt completes, and that writes outside ROOT, reads of
     the lead's checkout and `emacsclient` all fail.
2. **Process-group stop.** Start a background `sleep 5; touch late` from a
   candidate, stop the candidate, and confirm the file never appears, or
   that the capture and the judge ignore it.
3. **`refs/pstack-arena/` refs.** Confirm that `git gc` keeps the commits
   they point at, and that `git worktree prune` and the lead's normal git
   commands ignore them.

## Tests

- **Unit tests** with stubbed drive functions cover:
  - request validation, including the rejected repository states;
  - the phase barrier and `pending` judge;
  - expiry while the judge is pending;
  - killing the lead during candidates and during judging;
  - judge start failure;
  - an out-of-turn `error`;
  - stop reasons;
  - idempotent transitions;
  - schema-mismatched records surviving a reload;
  - absence of rubric strings from every candidate brief.
- **ACP fixture tests.** A fixture candidate writes a file, starts a
  background writer, and ends with a rationale. Assert that:
  - the result commit holds exactly the file;
  - the background write changes neither the commit nor the judge's
    checkout;
  - the judge sees both candidates;
  - cleanup removes worktrees, refs and directories.
- **Profile tests** run `sandbox-exec` against the generated profile, as the
  probes did.
- **A live check** with real Claude Code and Codex candidates on a scratch
  repository.

## Triage of interrogate panel 30662dcfd102

| Finding | Severity | Agreement | Outcome |
|---|---|---|---|
| Allowlist runs arbitrary code | critical | escape, git | Fixed. The OS sandbox replaces the responder. |
| Codex escalation and edit shapes defeat checks | high | escape | Fixed. The responder is no longer a boundary. |
| Git metadata and capture run candidate config | high | escape | Fixed. `ROOT/.git` is unwritable, the git dir is explicit, and hooks and fsmonitor are off. |
| Cloned dependencies link into the lead | high | escape, git | Fixed. Shared paths are read-only, and private clones get their links rewritten. |
| A killed shell leaves writers | high | all three | Fixed. Process-group stop, immutable judge checkouts, and Experiment 2. |
| Startup settings and hooks escape policy | high | escape | Fixed. Private config directories, with the sandbox active from launch. |
| Mode drift unseen | medium | escape | Dismissed. Inside the sandbox, the session mode no longer bounds anything. |
| `fs/write_text_file` bypasses policy | medium | escape | Fixed. Candidates advertise no fs write capability. Neither adapter uses it today. |
| Synchronous kill in its own callback | critical | lifecycle | Fixed. Deferred kill and a `stopping` state. |
| `clean-up` precedes process death | critical | lifecycle | Fixed. Capture waits for the process group to empty. |
| Judge settled by expiry or abandonment | high | lifecycle | Fixed. `pending` state, and phase-scoped expiry. |
| Killing the lead starts the judge | high | lifecycle | Fixed. Capture, then finish. |
| Struct reload corrupts records | high | lifecycle | Fixed. Slots appended, plus a schema number. |
| Synchronous git and clone work | high | lifecycle | Fixed. A `preparing` phase with async processes. |
| Base races the interrupted edit | high | lifecycle | Fixed. Capture after the cancelled `turn-complete`, plus a consistency retry. |
| Id collides after restart | medium | lifecycle | Fixed. A random suffix in `<arena-id>`. |
| Settling on every `error` | medium | lifecycle | Fixed. Only a failed prompt while running. `max_tokens` also counts. |
| Judge start failure re-enters `--finish` | medium | lifecycle | Fixed. Idempotent transitions. |
| Judge can't tell unfinished candidates | medium | lifecycle | Fixed. The state is in the judge brief. |
| View lacks a phase | low | lifecycle | Fixed. The phase is in the view and the mode-line help-echo. |
| Workspaces under `~/.emacs.d` | low | lifecycle | Fixed. Moved to `~/.local/state/`, and the lead's subdirectory is kept. |
| `checkout -- PATH` overwrites lead work | high | git | Fixed. Advice is `git diff --binary` with `git apply --3way`. |
| Submodules lost | high | git | Fixed. Rejected in v1. |
| Index from `HEAD` loses tracked files | medium | git | Fixed. The temporary index starts from the real index. Sparse checkouts are rejected. |
| Inconsistent base capture | medium | git | Fixed. Resolve once, plus a bounded retry. |
| Root and subdirectory conflated | medium | git | Fixed. The manifest records both. |
| Binary and LFS grafts | medium | git | Fixed. `--binary`, with LFS rejected in v1. |
| Cleanup trusts prefixes | medium | git | Fixed. Manifest-verified cleanup. |
| Rubric reachable through transcripts | medium | git | Fixed. The profile denies reads of the lead's checkout and the panel reports. |
