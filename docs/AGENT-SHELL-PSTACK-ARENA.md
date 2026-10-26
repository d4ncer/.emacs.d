# pstack arena panels: design

Status: implemented, revision 2. The panel side lives in
`lisp/+agent-shell-pstack-panel.el`, the git work in
`lisp/+agent-shell-arena-git.el`, and the sandbox in
`lisp/+agent-shell-sandbox.el` with `+agent-shell-drive-start :policy` and
`+agent-shell-drive-stop`. [How the implementation departs from this
design](#how-the-implementation-departs) lists every difference. It extends
the panels in `docs/AGENT-SHELL-PSTACK.md`. Revision 1 relied on a
permission responder. Interrogate panel `30662dcfd102` broke that, and this
revision replaces it with an OS sandbox. The triage at the end maps each
finding to the change it caused.

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
  - the candidate's private temp folder, `/tmp/pa-<8 hex>`, set as
    `TMPDIR`, `CLAUDE_CODE_TMPDIR` and `GOTMPDIR`, and appended to
    `JAVA_TOOL_OPTIONS` as `-Djava.io.tmpdir`, because the JVM ignores
    `TMPDIR` on macOS, while `GOTELEMETRY=off` keeps Go tools from
    writing telemetry under `~/Library`;
  - exact paths in the macOS per-user folders that toolchains write even
    with `TMPDIR` set: `TemporaryItems` for xcodebuild, anything starting
    `xcrun_db` for xcrun, and `hsperfdata_<user>` for the JVM in
    `getconf DARWIN_USER_TEMP_DIR`, plus `com.apple.DeveloperTools` for
    xcodebuild in `getconf DARWIN_USER_CACHE_DIR`. The rest of the temp
    folder stays unwritable, since it holds other programs' files such as
    the Emacs server socket;
  - the candidate's private agent config directory, set through
    `CLAUDE_CONFIG_DIR` or `CODEX_HOME`, so it cannot plant hooks or
    settings in the user's real config;
  - `/dev`.
- **Deny writes to `ROOT/.git`**, the worktree's pointer file, so the
  candidate cannot redirect Emacs's later git commands.
- **Deny reads** of:
  - the lead's checkout, except the shared dependency paths and the git
    directory the worktrees share;
  - `+agent-shell-pstack-panel-directory`;
  - every other candidate's root;
  - every sandbox's private temp and config folders, by two anchored
    regexes: `/private/tmp/pa-` followed by 8 hex digits, and the true
    name of `temporary-file-directory` followed by `/pa-config-`. Config
    folders are always made there, and the folder is fixed on first use.
    A later rule allows the candidate's own two folders back. The pattern
    matters because each sandbox's folders are made when it is, so an
    earlier candidate's profile cannot name a later one's. A config
    folder holds copied credentials such as Codex's `auth.json`;
  - the credential locations in `+agent-shell-sandbox-secret-reads`: SSH
    and GnuPG keys, the AWS, Google Cloud, Azure and Kubernetes logins,
    Docker's registry logins, `~/.netrc` and `~/.authinfo`, the GitHub
    and 1Password CLI logins, `~/Library/Keychains`, and
    `+private-config-directory`. A missing path is harmless. Private
    config can extend the list, and a repository profile's `:read` can
    reopen a path inside one.

  This keeps the rubric in the lead's transcript, the other candidates'
  work and logins, and the user's credentials out of reach, so withholding
  the rubric is enforced, not just a convention. The network stays open,
  so what a candidate can read it can send out.

  agent-shell serves the agents' ACP file reads in Emacs, outside the
  profile. `+agent-shell-drive--file-allowed-p` applies the same rules
  through `+agent-shell-sandbox-readable-p`, erring towards refusal on
  patterns: a denying regex refuses every path starting with its literal
  prefix, and an allowing regex opens nothing there, so only a path
  allowance reopens a denied path.
- **Allow metadata reads** of the lead's top level itself, which git stats
  when it resolves the main worktree. With the git directory readable, a
  candidate runs read-only git such as `git status` and `git diff`. The
  git directory stays unwritable, so every candidate gets
  `GIT_OPTIONAL_LOCKS=0` and status never tries to refresh the index.
- **Deny unix-socket connections** except `/private/var/run/mDNSResponder`
  and the sockets a repository profile lists in `:connect`, such as a
  build tool's tunnel to its remote cache. This blocks `emacsclient` and
  other local daemons. The kernel matches a socket's true name, so the
  profile allows the true name of each listed path. Emacs rejects a
  listed path in or under `server-socket-dir`.
- **Allow outbound TCP**, which the agent's own API needs. Exfiltration over
  the network stays possible, and v1 accepts that.

**Repository profiles** in `+agent-shell-pstack-arena-profiles` supply
everything specific to one repository. Arenas need none in a small or
medium repository. A profile names its paths in these kinds:

- **Shared** paths, such as `elpaca`, are symlinked into ROOT from the lead.
  The profile makes them read-only, so a candidate can load them but not
  change them.
- **Private** paths, such as `_build`, are cloned with `cp -c -R`. Every
  symlink in the clone that points into the lead's checkout is rewritten to
  the clone's own copy. A private path needs this because candidates write
  build output into it.
- **Copied** paths, such as generated config the lead's tools wrote, are
  cloned like private ones and left out of the result.
- **Generated files** are written into ROOT before the agent starts, by
  functions of the candidate's sandbox, so their content can point tools
  at the private temp and config folders. They are left out of the result
  too.
- **Allowances** add write and read paths to the profile, and
  **environment** entries take precedence over the defaults. An allowance
  may be a `(regexp . RE)` pattern, emitted as an SBPL `(regex #"RE")`
  filter, for files with random names in a shared folder. Emacs checks
  each pattern at the boundary: `RE` starts with `^` and the literal true
  name of a folder below the root, has no alternative outside
  parentheses, holds no double quote, and compiles under `sandbox-exec`.
  So no pattern can open `/` or the whole filesystem.

Capture restores copied and generated paths to the base with `git reset`
after `git add`, rather than excluding them with a pathspec, because
`git add` fails when an exclude pathspec names an ignored path.

**Trace mode** is a lever for writing profiles.
`+agent-shell-pstack-arena-trace` provisions one throwaway candidate as an
arena would and runs a command there under a profile where each refusing
rule becomes `(allow OP (with report) (with message TAG) ...)`. The
allowances that follow still override, so only what the candidate profile
would refuse is reported. `log stream --style ndjson` with the predicate
`eventMessage CONTAINS TAG` selects this run's kernel reports and no other
sandboxed process's. Two probes bracket the command. Each one reads
`/dev/null` under a profile that reports with `TAG ready` or `TAG done`.
`log stream` prints its filter line before it receives events, so the
first probe shows the stream is live, and the second shows the log has
caught up after the command exits. The kernel drops some reports even
so. 110 processes making one write each lost 5 reports, which never
arrived within 4 seconds, and 30 processes making four writes each lost 1
of 120.

**Enforcing mode** runs the command under the candidate profile itself,
with a prefix argument to the same command, to prove a profile works.
Every sandbox's refusing rules are `(deny OP (with message TAG) ...)`, so
the kernel logs each refusal as `Sandbox: PROCESS(PID) deny(COUNT) OP
PATH` followed by a line holding TAG, the same shape as a trace report
with `deny(COUNT)` in place of `allow`. A probe confirmed it: a `touch`
refused under such a profile logged `Sandbox: touch(81663) deny(1)
file-write-create /private/tmp/pa-probe/denied-file` and then the tag.
The same predicate and probes therefore select this run's refusals and
no other process's, with no need to filter by process tree or path. The
tag is `pa-sandbox-<8 hex>`, made when the sandbox is, so real
candidates' refusals in the system log carry their sandbox's tag too.

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
  - the repository has submodules;
  - the platform is not macOS;
  - an agent has no sandboxed launch configuration.

## Lifecycle

Phases belong to the panel. Kinds and the `pending` state belong to members.
Only the current phase's members are subject to expiry and abandonment.

```
panel:     preparing ─▶ candidates ─▶ capturing ─▶ judging ─▶ finished
               │            │             │           │
               └─ failure ──┴─ lead killed or no usable candidate ─▶ finished

candidate: pending ▶ starting ▶ waiting ▶ running ▶ done | error | timeout
capture:   stopping ▶ stopped ▶ (all stopped) capturing ▶ captured | failed
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
  2. Emacs walks the adapter's process tree and kills every descendant,
     whatever its process group, then the adapter.
  3. Emacs sweeps processes whose working directory lies under ROOT or the
     private temp folder, and repeats until none remain.
  4. Emacs deletes the private temp and config folders in the background
     with `+agent-shell-sandbox-delete`, which gives the owner write
     permission back first. Bazel's outputs and Go's module cache are
     read-only trees, and a plain delete of one fails. A folder that still
     cannot go is reported, and the stop goes on.

  Experiment 2 shows why steps 2 and 3 are both needed. A process that
  escapes both can still write to ROOT, but the capture and the judge below don't
  depend on ROOT after this point.
- **Capturing** records each candidate's result commit (see Snapshots),
  but only once every candidate has stopped. A candidate can read the
  shared git directory, so a result captured while another candidate ran
  would reach that candidate.
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
  3. Run `git add -A --sparse` against it through `GIT_INDEX_FILE`.
  4. Run `write-tree`, then `commit-tree --no-gpg-sign -p <resolved HEAD>`.
  5. Repeat steps 2 to 4 until two consecutive trees match, up to three
     attempts.

  The lead's own index never changes.
- **Candidate result.** Emacs captures the same way from the worktree, with
  the base as the parent. The temporary index starts as a copy of the
  worktree's own index, from its git directory, reset to the base with
  `git read-tree --reset`. That keeps the stat data of every entry whose
  content matches the base, so `git add -A` hashes only the files changed
  since checkout rather than every file in the repository. On a generated
  50,000-file repository, `test/bench-agent-shell-arena-capture.el` measured
  a median capture of 47 seconds before and 3 after. It records the commit
  under
  `refs/pstack-arena/<arena-id>/<role>`. That is outside `refs/heads`, so
  the lead's branch lists stay clean, and the ref keeps the commit from
  garbage collection.
- **Sparse checkouts.** `git worktree add`, run from the lead's git
  directory, copies the lead's `info/sparse-checkout` and per-worktree
  config into the new worktree before it checks out. Cone and non-cone
  patterns, a sparse index, and sparse settings kept in the shared config
  all carry over without extra steps. Both snapshots keep skip-worktree
  entries as the index has them. `--sparse` makes `git add` take files a
  candidate created outside the patterns instead of failing, and the
  follow-up names them.
- **Git LFS.** Hooks stay off, but clean and smudge filters are config, so
  they still run. A candidate's checkout holds the real files, and `git
  add` stores an edited LFS file as a new pointer while untouched ones keep
  their stat data and never reach the filter. With git-lfs installed, `git
  apply --3way` applies a pointer change to the lead's hydrated file from
  the shared LFS store. A conflict on an LFS file leaves conflict markers
  inside the pointer, so the follow-up flags those files for a graft by
  hand.
- **Manifest.** `<arena-id>/manifest.json` records:
  - the repository's git directory;
  - the lead's root and subdirectory;
  - the base commit;
  - each role's worktree path, the worktree's own git directory, its result
    commit and final state.

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
    applies to the lead's current tree with `git apply --3way`;
  - the changed files outside the lead's sparse-checkout patterns, if any;
  - the changed Git LFS files, if any, with a note to graft them by hand
    when `git apply --3way` conflicts on them.

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

Private clones and the arena folder go through `+agent-shell-sandbox-delete`,
which restores owner write permission before it deletes and never follows
a symlink. When `git worktree remove` fails on a read-only tree, git has
already dropped the worktree's registration, so cleanup deletes what it
left the same way. A step that fails is recorded and the remaining steps
still run. The callback then gets every failure. A worktree that stays
keeps the arena folder and its manifest for a later cleanup.

## Experiment 1 results

Run on 2026-10-25 with `claude-agent-acp` 0.81.2 and `codex-acp` 2.1.1.
Each agent started from a sandboxed copy of its live agent-shell config,
under `sandbox-exec` with a profile shaped like the one above. The profile
denied writes outside the root, the private config folder and the private
`TMPDIR`. It denied reads of `~/.emacs.d`, `~/.claude` and `~/.codex`, and
every unix socket except `mDNSResponder`. Each agent then ran six probe
commands, one per tool call, and the results were checked on disk.

**Codex in `agent-full-access` passes.** It authenticated from a copied
`auth.json` in a private `CODEX_HOME` and completed the turn.

| Probe | Result |
|---|---|
| Write a file in the root | written, and present on disk |
| Write a file outside the root | `operation not permitted`, and absent on disk |
| Read `~/.emacs.d/AGENTS.md` | `Operation not permitted` |
| List `~/.claude` | `Operation not permitted` |
| `emacsclient` to the live server socket | `Operation not permitted` |
| HTTPS to example.com | `200` |

The `emacsclient` row comes from rerunning the command against the real
socket path under the same profile. Inside the agent, the private `TMPDIR`
meant `emacsclient` never found the socket, so it never tried to connect.

**Claude Code in `bypassPermissions` passes with its own token.** It
signed in with a long-lived `CLAUDE_CODE_OAUTH_TOKEN` made by `claude
setup-token`. Emacs read the token from the Keychain item
`pstack-arena-claude-token` and passed it in the environment. Claude Code
also had a private `CLAUDE_CONFIG_DIR` holding no credentials file. Every
probe gave the same result as for Codex, and the file checks matched on
disk. Two earlier attempts failed, and they show why the token is needed:

- **Copied credentials file.** Your live credentials are in the Keychain
  item `Claude Code-credentials`. `~/.claude/.credentials.json` is a stale
  copy from Aug 5. With `CLAUDE_CONFIG_DIR` pointing at a copy of it, the
  prompt failed: "OAuth session expired and could not be refreshed".
- **Private `HOME`.** With `HOME` pointing at a private folder, the prompt
  failed: "Authentication required". Claude Code did not find the Keychain
  item.

**Claude Code's Bash ignores `TMPDIR`.** Its per-project scratch directory
(`/private/tmp/claude-<uid>/<cwd slug>`) and each command's cwd file
(`/tmp/claude-XXXX-cwd`) went straight to `/tmp`, which the profile denied.
Every command failed until the scratch directory was allowed. After that,
commands still exited with an error after printing correct output, because
of the cwd file. Setting `CLAUDE_CODE_TMPDIR` to the private temp folder
moved both files there. All probes then passed with correct exit statuses,
and the profile needed no `/tmp` exception.

The Claude Code binary warns when that directory is longer than about 30
bytes. The path has to fit an AF_UNIX socket address, and the 39-byte test
path was over the limit. Past the limit, child processes fall back to
another `$TMPDIR`. Candidates therefore get a short private temp folder,
`/tmp/pa-<8 hex>`.

**What experiment 1 changes in the design.**

- **Each agent signs in its own way.** A Claude Code candidate gets
  `CLAUDE_CODE_OAUTH_TOKEN` from the Keychain item
  `pstack-arena-claude-token`. It is never refreshed, so a candidate cannot
  rotate your login away. Emacs rejects an arena with a Claude Code
  candidate when the item is missing. A Codex candidate gets a copy of
  `~/.codex/auth.json` in a private `CODEX_HOME`.
- **Codex token refreshes need a copy-back.** A candidate that refreshes
  its token holds the newest refresh token. If refresh tokens rotate, that
  would leave your real `~/.codex/auth.json` stale. No refresh happened in
  this run: both files still recorded the Oct 18 refresh. When a candidate
  ends, Emacs copies its `auth.json` back if the copy is newer and the real
  file has not changed since it was copied.
- **Candidates can read their own credentials.** The token or `auth.json`
  sits inside the sandbox, and the network is open, so a hostile candidate
  could send it out. That's the same exposure any agent session has today,
  and v1 accepts it. No candidate can read another's copy, since every
  profile denies every sandbox's config folder but its own.
- **agent-shell writes into the root.** It created `.agent-shell/`
  (transcripts) inside each root. Candidates start with transcripts off,
  and capture excludes `.agent-shell/` either way.

## Experiment 2 results: stopping background writers

Each agent ran `(sleep 20; touch late.txt) > /dev/null 2>&1 &` and ended
its turn. Emacs then killed its shell with `+agent-shell-drive-kill`.

| Agent | Result |
|---|---|
| Claude Code | The background job survived the kill and wrote `late.txt` about 20 s later. It ran in its own process group, 35702. |
| Codex | No `late.txt`, and no surviving `sleep` was seen. Inferred, not verified: Codex kills a command's process group when the command finishes. |

So signalling the adapter's process group misses Claude Code's commands.
The stop step becomes:

1. **Walk the adapter's process tree** by parent pid while the adapter is
   still alive. Kill every descendant whatever its process group, then the
   adapter.
2. **Sweep by working directory.** Kill every process whose cwd lies under
   ROOT or the private temp folder (`lsof -t -a -d cwd +D <dir>`), and
   repeat until none remain. This catches jobs that were orphaned and
   reparented to launchd before the walk.

A probe confirmed the sweep. A detached `(sleep 15; touch late3.txt) &`
started under the profile was found by its working directory and killed,
and `late3.txt` never appeared. A process that leaves ROOT and
double-forks can still escape the sweep. Its writes stay inside ROOT, the
private temp folder and the private config folder, and nothing reads those
after capture. The capture's two-matching-trees check covers writes that
land during capture.

## Experiment 3 results: result refs

In a scratch repository, a commit referenced only by
`refs/pstack-arena/abc123/opus` behaved as follows:

| Check | Result |
|---|---|
| `git gc --prune=now` with reflogs expired | kept |
| `git worktree prune` | kept |
| `git branch` and `git status` | unaffected |
| A normal `git push` | did not send the ref |
| `git log --all` | lists it, so "all refs" views show candidate results until cleanup |
| Delete the ref, then gc | removed, which confirms cleanup frees it |

This user has `commit.gpgSign` set globally. That made the scratch repo's
first `git commit` try to sign and fail. `git commit-tree` itself did not
sign, even with `commit.gpgSign=true` set globally and locally. The git
library's tests confirmed this on git 2.56: its output had no `gpgsig`
header. Snapshot commits still pass `--no-gpg-sign` as a guard, and set a
fixed author and committer, so capture needs neither the user's identity
nor their signing key.

## Remaining experiments

None. Every probe above was run on this machine.

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

## How the implementation departs

- **An `interrupting` phase.** A request found mid-turn puts the panel in
  `interrupting` until the lead's cancelled `turn-complete`, then
  `preparing` starts. The design folded that wait into `preparing`.
- **Stopping runs per candidate, capture waits for all.** Each candidate
  is stopped as soon as it settles, so its tree stops changing, and its
  capture state becomes `stopped`. `capturing` waits until every
  candidate has stopped, then captures them all concurrently, each with
  its judge checkout when its result differs from the base. Captures
  write separate refs and git tolerates concurrent object writes, so
  running them together costs nothing in safety. A candidate that never
  got a worktree fails as soon as it stops.
- **Checkouts are worktrees.** Each judge checkout is a detached worktree
  under the arena's `checkouts/` folder, recorded as the role's `checkout`
  in the manifest, so `+agent-shell-arena-git-clean` removes it like a
  candidate worktree.
- **Labels hide the agent.** The judge sees candidates as A, B, C and D,
  not by role, so a role named after a model does not tell the judge the
  vendor. The follow-up maps each label to its role and agent.
- **The graft command names the top level.** It reads `(cd <top level> &&
  git diff --binary <base> <result> | git apply --3way)`, because `git
  apply` run from a subdirectory skips paths outside it.
- **Shared links need an allowance.** A `sandbox-exec` probe showed that a
  read through a symlink is checked against its target, so a shared
  dependency linked into a read-denied checkout was unreadable. The sandbox
  spec gained `:allow-reads`, emitted after the denials, and each candidate
  allows its shared paths. The profile also denies reads of `~/.claude`
  and `~/.codex`, as experiment 1 did.
- **Candidates read the lead's git directory.** A worktree's `.git` file
  points into the lead's git directory, so with the lead's checkout denied
  git failed with "gitfile does not point to a valid repository". Then it
  failed with "Invalid path" when it resolved the main worktree. The
  profile now allows reads of the common git directory and metadata reads
  of the literal top level. This exposes the repository's objects and
  refs, but no working file of the lead. No candidate sees another's
  result through them, because nothing is captured until every candidate
  has stopped. The arena ACP test checks that each capture starts with
  both candidates stopped and no `refs/pstack-arena/<arena-id>/` ref.
- **A missing Claude Code token fails the candidate, not the request.** The
  Keychain lookup happens when the candidate starts. That candidate settles
  as `error` with the `security add-generic-password` hint, and the others
  run.
- **An `error` event settles a running member.** The event carries only a
  code and a message, so Emacs cannot tell a failed prompt from another
  failed request. It settles a member on any `error` while the member's
  brief runs, and ignores it otherwise.
- **Failed preparation is a rejection.** A base or worktree failure ends the
  arena with the rejection follow-up, and Emacs discards the partly built
  arena itself, since no candidate ran in it.
- **The default judge falls back to the lead's agent.** When no other
  read-only agent exists, the judge runs on the lead's own agent.
- **The ACP test checks reads, not late writers.** The arena ACP test
  checks that each candidate cannot read the lead's checkout and never
  saw the rubric. Late background writers are covered by the sandboxed
  shell test, `test/agent-shell-sandbox-acp-test.el`.

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
| Index from `HEAD` loses tracked files | medium | git | Fixed. The temporary index starts from the real index, so skip-worktree entries in a sparse checkout survive. |
| Inconsistent base capture | medium | git | Fixed. Resolve once, plus a bounded retry. |
| Root and subdirectory conflated | medium | git | Fixed. The manifest records both. |
| Binary and LFS grafts | medium | git | Fixed. `--binary`, and LFS files are captured as pointers and flagged in the follow-up. |
| Cleanup trusts prefixes | medium | git | Fixed. Manifest-verified cleanup. |
| Rubric reachable through transcripts | medium | git | Fixed. The profile denies reads of the lead's checkout and the panel reports. |
