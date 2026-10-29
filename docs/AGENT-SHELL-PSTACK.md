# pstack in agent-shell

[pstack](https://github.com/cursor/plugins/tree/main/pstack) is Lauren Tan's
Cursor skill stack. This configuration does not port it. The skills stay in
each agent, and `lisp/+agent-shell-pstack.el` adapts how agent-shell drives
them. agent-shell is an ACP client above the harness, so it can add things a
single-harness port cannot, while upstream text stays untouched.

## Layers

| Layer | Status | Scope |
|---|---|---|
| 0. Skills in agents | Done | pstack installed in Claude Code and Codex |
| 1. Sticky mode and guard | Done | `+agent-shell-pstack-mode`, `+agent-shell-pstack-guard-mode` |
| 2. Cross-vendor panels | Done | `how` and `interrogate` members run as read-only sibling shells; `arena` candidates run in sandboxed worktrees, here or on a remote host |
| 3. Views | Partial | Panel status buffer; decision trail and plan views are planned |

## Layer 0: install the skills

- Claude Code uses the first-party plugin. Run `/plugin marketplace add
  cursor/plugins` and `/plugin install pstack@cursor-plugins` in a terminal
  `claude` session, then `/pstack:setup-pstack`. Model roles are written to
  `~/.claude/rules/pstack-models.md`.
- Codex uses the harness-neutral mirror. Run `npx skills add backnotprop/pstack`,
  select every skill and Codex, then `$setup-pstack`. Model roles are written
  to `~/.agents/pstack-models.md`.

Both ACP adapters load these installs: `claude-agent-acp` reads user settings
and plugins, and `codex-acp` runs `codex app-server`. Their commands appear in
agent-shell slash completion as `/pstack:<skill>` and `$<skill>`.

## Layer 1: sticky mode and guard

`C-c p` in an agent shell toggles `+agent-shell-pstack-mode`. The next prompt
is sent as `/pstack:poteto-mode <prompt>` or `$poteto-mode <prompt>`, chosen by
the agent config identifier in `+agent-shell-pstack-invocations`. Later prompts
in the same session carry `+agent-shell-pstack-reminder` as a trailing text
block. A prompt that already starts with a command is sent unchanged. A new
session id loads poteto-mode again; toggling the mode does the same after a
context compaction.

The mode installs an outgoing-request decorator on the shell's ACP client,
chained after any existing decorator. Only the ACP request changes. The buffer
and transcript keep the typed text.

`+agent-shell-pstack-guard-mode` is global and enabled with agent-shell. It
wraps `agent-shell-permission-responder-function`: execute tool calls matching
`+agent-shell-pstack-irreversible-patterns` (force pushes, history rewrites,
merges, infrastructure changes, SQL drops) always reach the interactive prompt;
everything else goes to the wrapped responder. The guard only sees requests
the agent sends. Session modes in `+agent-shell-pstack-unguarded-modes` skip
permission prompts, and agent-side "always allow" rules never reach Emacs, so
keep agent-side `ask` rules for these commands as well.

## Layer 2: cross-vendor panels

Upstream panels mix model families. Ports to one harness lose that, or shell
out to other vendors' CLIs. Here Emacs starts each panel member as an agent
shell for a configured agent. `lisp/+agent-shell-pstack-panel.el` holds the
panels, and `+agent-shell-pstack-panel-mode` is global and enabled with
agent-shell. It covers the `how`, `interrogate`, `arena` and `delegate`
shapes in `+agent-shell-pstack-panel-shapes`. `how` and `interrogate`
members only read. `arena` candidates write code, so each runs in its own
sandboxed worktree; see [Run an arena](#run-an-arena) and the design in
`docs/AGENT-SHELL-PSTACK-ARENA.md`. A `delegate` hands the whole task to
one agent, which commits on a branch of its own; see
[Delegate panels](#delegate-panels).

### Request a panel

A lead is a shell with `+agent-shell-pstack-mode` on. Its priming prompt
carries the protocol, which names every agent in `agent-shell-agent-configs`
that has a read-only mode and tells the lead to use panels in place of the
`how`, `interrogate` and `arena` skills' own subagents. Every later prompt
adds one line that restates the rule and names
`var/pstack-panels/PROTOCOL.md`, which Emacs rewrites with the current
protocol, so a lead that compacted its context can reread it. The lead ends
its final message with a fenced block, then ends its turn:

````
```pstack-panel
{"version":1,"shape":"how","brief":"...","timeout":900,
 "members":[{"role":"data-flow","agent":"codex","focus":"..."}]}
```
````

- `version` is 1.
- `shape` names an entry in `+agent-shell-pstack-panel-shapes`.
- `members` lists 1 to 4 members. Each `role` matches `[a-z0-9-]+` and is
  unique. Each `agent` is one the protocol named. `focus` is optional.
- `timeout` is 1 to 3600 seconds. It defaults to
  `+agent-shell-pstack-panel-default-timeout`, 900.

Emacs reads the first complete block in the lead's message, as it reaches a
live `turn-complete` or the tool call that ends the message, so a restored
session never starts a panel. A lead that calls a tool after its block has
broken the protocol: Emacs starts the panel anyway and interrupts the lead's
turn, so the lead waits for the reports instead of working around them. The
follow-up opens by naming the interrupted call, which may not have finished. The
panel id is the first 12 hex digits of the SHA-1 of the session id and the
block, and Emacs ignores a block it has seen. A malformed block gets a prompt
back that says why it was rejected.

The request travels over ACP rather than through a file inbox. The lead needs
no write permission, which plan modes often refuse, and its shell identifies
it. The file inbox stays in `agent-shell-projects`, its only user.

### Keep members read-only

Each member starts through `+agent-shell-drive-start :policy 'read-only` in the
lead's directory. Three layers keep it read-only:

1. The session mode comes from `+agent-shell-drive-read-only-modes`: `plan`
   for Claude Code and `read-only` for Codex.
2. A buffer-local permission responder approves `read`, `search`, `fetch` and
   `think` tool calls once and rejects everything else. It leaves the dialog
   to you only when a request offers no way to reject.
3. Once the session is initialized, Emacs kills a member whose mode differs
   from the requested one, before it sees the brief. Upstream treats a
   refused mode as optional and carries on, so this check is what catches it.

The guard wraps the default responder, so it leaves members' responders
alone. Members are never displayed.

A member's prompt is the brief, its focus and the shape's contract. Its report
is the final text of its turn, after its last tool call. When Claude Code asks
to leave plan mode with a plan longer than that text, the rejected plan is the
report. A turn that ends `cancelled`, `refusal`, `max_tokens` or
`max_turn_requests` settles the member as `error` with that reason, and so
does an `error` event while the member's brief runs.

### Reports and the follow-up

Emacs writes each report once to
`var/pstack-panels/<panel-id>/<role>.md` under
`+agent-shell-pstack-panel-directory`. A member still working at the timeout
settles with its partial text. A member whose shell ends early settles with
the reason. When every member has settled, Emacs prompts the lead with each
member's agent, state, report path and report text, then the shape's triage
line. The lead still triages and judges.

Delivery uses `+agent-shell-drive-send`, which never interrupts a busy lead or
an unsent draft. Emacs retries on the lead's next `turn-complete` or
`prompt-ready`, and each follow-up is sent once.

Panels live in memory. Restarting Emacs ends the lead and every member, so an
interrupted panel does not resume. An arena on a remote host is the
exception; see [On a remote host](#on-a-remote-host). A delegate that worked
in the lead's checkout leaves a record that lets Emacs return the checkout
later; see [Delegate panels](#delegate-panels). Killing the lead
stops its unfinished panels. The panel and member structs only gain slots at the end and carry
a schema number, `+agent-shell-pstack-panel--schema`. Loading the library
again outside the reload command drops finished panels from another schema,
with a message, rather than letting a changed struct break them. It keeps
unfinished ones and names them; run the reload command to carry them over.

### Provider failures

ACP has no stop reason for a turn the provider failed, so an adapter may end
it with `end_turn` and the provider's error as its text. Every member, in
every shape and on a host too, classifies each finished or failed turn with
`+agent-shell-drive-failure`, from the first of:

- a JSON-RPC error on the prompt whose data names an `errorKind` (Claude
  Code) or a `codexErrorInfo` (Codex);
- a turn that saw Codex's `_meta.codex.error` warning and ends with exactly
  that error's message;
- a whole final message matching the agent's `:failure-patterns` in
  `+agent-shell-sandbox-agents`, such as Codex's capacity line. A report that
  merely mentions such a line does not match.

The drive library advises agent-shell's error and notification handlers, for
the shells it watches only, so the error data and Codex's warnings reach it.
Kinds are `rate_limited`, `quota_exhausted`, `overloaded`, `transport_lost`,
`auth_required`, `context_exhausted`, `budget_exhausted`, `policy_denied`,
`internal_error` and `provider_error`.

A `rate_limited`, `overloaded`, `transport_lost` or `internal_error` failure
is retried once: after `+agent-shell-pstack-panel-retry-delay` seconds, 60
by default, Emacs asks the same shell to continue where it left off. No retry
starts past the phase's timeout or an arena's deadline. Any other kind, a
second failure, or a retry that does not fit settles the member as `error`.
It keeps its text, report, commits and transcript, and the follow-up says
what failed, such as `Codex was overloaded: Selected model is at capacity.`
A host's status and report carry each member's `failure_kind`.

### Reloading live

After editing pstack code, run `M-x +agent-shell-pstack-reload`. It reloads
the already loaded libraries in dependency order and adopts changed option
defaults when the current value still equals the old default. Other option
values and enabled global minor modes are preserved. Panels and remote
records carry over to changed struct layouts slot by slot by name: new slots
take their defaults, and records whose layout is unchanged keep their
identity. If an unfinished panel holds a value in a slot the new code
removes or renames, the command loads nothing and names the panels; retry
after they finish. The summary names the libraries reloaded and options
reset, and counts the panels carried over.

### Run an arena

An arena runs arena's Fan out and Cross-judge phases; the lead keeps Frame,
Pick, Graft and Verify. It needs a sandbox backend, macOS's or Linux's, and
a lead inside a git repository with a commit and no submodules. A sparse checkout carries over. Each
candidate's worktree gets the lead's patterns, and the follow-up names any
file a candidate changed outside them. Git LFS files are captured as
pointers, and the follow-up flags a candidate that changed one. The
protocol describes arenas only when some configured agent has an entry in
`+agent-shell-sandbox-agents`. Every member signs in with the access token
of the host's own login, never a refresh token. Before any member starts,
Emacs checks that each agent's login outlasts the arena, refreshes it once
on the host if not, and refuses the whole arena if one still falls short;
see Host logins in [the arena design](AGENT-SHELL-PSTACK-ARENA.md).

The request adds three fields to the panel block:

````
```pstack-panel
{"version":1,"shape":"arena","brief":"The task, identical for every candidate",
 "rubric":["correctness","simplicity","tests"],"timeout":2400,"judge_timeout":900,
 "members":[{"role":"opus","agent":"claude-code"},
            {"role":"codex","agent":"codex","focus":"optional direction"}],
 "judge":{"agent":"codex"}}
```
````

- `members` lists 1 to 4 candidates, each on an agent in
  `+agent-shell-sandbox-agents`. The role `judge` is reserved.
- `rubric` lists 3 to 6 criteria. Only the judge sees them.
- `judge` is optional, on an agent in `+agent-shell-sandbox-agents`. By
  default it is the first such agent whose identifier differs from the
  lead's.
- `timeout` bounds the candidates, and `judge_timeout` bounds the judge,
  900 seconds by default.

The panel moves through these phases, shown in the panels view and the
lead's mode-line help:

1. `interrupting`, only for a request found mid-turn: Emacs waits for the
   lead's interrupted turn to end, so its last edit lands in the base.
2. `preparing`: `lisp/+agent-shell-arena-git.el` snapshots the lead's
   working tree as a base commit and gives each candidate a detached
   worktree under `+agent-shell-pstack-arena-directory`. A failure here
   rejects the arena and discards what it made.
3. `candidates`: each candidate starts through `+agent-shell-drive-start`
   with a `+agent-shell-sandbox` rooted at its worktree. The sandbox
   refuses writes outside the worktree and refuses reads of the lead's
   checkout, `+agent-shell-pstack-panel-directory`, `~/.claude`, `~/.codex`,
   the other candidates' worktrees, every other sandbox's private temp and
   config folders, and the credential locations in
   `+agent-shell-sandbox-secret-reads`, such as `~/.ssh`, `~/.aws`,
   `~/Library/Keychains` and `+private-config-directory`. agent-shell's
   own file reads for the agent obey the same rules. The repository's
   profile, below, can add shared links, which stay readable, and its
   `:read` can reopen a path inside a credential location. The lead's git directory
   stays readable too, so candidates can run read-only git. Every
   candidate gets `TMPDIR`, `GOTMPDIR` and `-Djava.io.tmpdir` in
   `JAVA_TOOL_OPTIONS` pointing at its private temp folder, along with
   `GOTELEMETRY=off` and `GIT_OPTIONAL_LOCKS=0`, and may write the few
   macOS per-user temp and cache paths that xcodebuild, xcrun and the JVM
   need. The candidates' timer starts here.
4. `capturing`: Emacs stops each candidate with every process it started
   as soon as it settles. Once every candidate has stopped, Emacs commits
   each tree under `refs/pstack-arena/<arena-id>/<role>` and checks a
   result that differs from the base out in a separate worktree under the
   arena's `checkouts/` folder. Capture waits for the last candidate
   because a running candidate can read the git directory, so it would
   see an earlier result.
5. `judging`: a judge in a read-only sandbox over the `checkouts/` folder
   gets the task, the rubric, and each changed candidate's label, outcome, checkout, diff stat
   and rationale. It scores each criterion and recommends a base.
6. `finished`: the lead gets one follow-up with each candidate's state,
   result commit, ref, diff stat, rationale and a command that applies its
   diff, `(cd <top level> && git diff --binary <base> <result> | git apply
   --3way)`, then the judge's report and arena's Pick, Graft and Verify
   instructions.

With no candidate that changed the base, the arena finishes without a
judge. Killing the lead captures what the candidates made and finishes
without a judge.

Results stay on disk, because grafting can take days. To clean up an
arena, press `x` on its lines in the panels view once it has finished, or
run `M-x +agent-shell-pstack-arena-clean` to choose from every arena on
disk. Either asks first, then removes the worktrees, checkouts, result
refs and the arena's folder through `+agent-shell-arena-git-clean`, which
trusts only the arena's manifest. Read-only trees, such as Go's module
cache in a private clone, are made writable before they are deleted. A
step that fails is reported, and the rest of the cleanup still runs.

#### Repository profiles

Arenas need no setup in an ordinary repository. A repository whose tools
need more registers a profile in `+agent-shell-pstack-arena-profiles`,
from private config rather than this one. Each entry maps a matcher, a
directory compared by true name with the top level or a predicate called
with the top level, to a plist. The first match applies. These are the
keys:

- `:shared` lists paths symlinked from the lead, readable but not
  writable.
- `:private` lists paths cloned into each candidate, with symlinks into
  the lead rewritten to the clone.
- `:copy` lists ignored or untracked paths cloned from the lead, such as
  generated config.
- `:files` maps a path to a function of the candidate's
  `+agent-shell-sandbox` returning the text written there before the agent
  starts.
- `:write` and `:read` add sandbox allowances. Each element is an
  absolute path, a `(regexp . RE)` pattern, or a function of the sandbox
  returning paths or patterns. A pattern suits a tool that makes randomly
  named files in a shared folder, such as `diff`'s
  `/private/tmp/diff.XXXXXXXX`: `(regexp . "^/private/tmp/diff\\.")`.
  `RE` is a `sandbox-exec` regex matched against true names, so it says
  `/private/tmp`, not `/tmp`. It must start with `^` and the literal name
  of a folder below the root, with no `|` outside parentheses, and
  `sandbox-exec` must compile it. Emacs rejects anything else, such as
  `".*"` or `"^/.*"`, which could match the whole filesystem.
  `+agent-shell-sandbox-regexp-quote` quotes a path a function computes.
- `:connect` lists unix socket paths a candidate may connect to, or
  functions of the sandbox returning them. Every other unix socket is
  refused but mDNSResponder's, so a build tool that reaches its remote
  cache through a local tunnel, such as `/private/tmp/<name>-tunnel.sock`,
  needs its socket here. Each path is absolute, and its true name goes
  into the profile, so `/tmp/x.sock` and `/private/tmp/x.sock` both work.
  Emacs refuses any path in the Emacs server's socket folder,
  `server-socket-dir`, however a symlink reaches it, so no candidate can
  drive this Emacs.
- `:env` lists `NAME=VALUE` entries, or is a function of the sandbox
  returning them. They win over the defaults.

Paths in `:shared`, `:private`, `:copy` and `:files` are relative to the
top level. Copied and generated files never reach a candidate's result.
Emacs checks the profile when a candidate is provisioned, and what its
functions return when the sandbox is built. A malformed value rejects the
arena with an error naming the key.

This hypothetical repository shares its vendored packages, copies its
generated `local.env`, points its build tool's cache at each candidate's
private temp folder, lets its language server write its own cache, and
lets `diff` write its randomly named temp files:

```elisp
(add-to-list
 '+agent-shell-pstack-arena-profiles
 `("~/src/widget/"
   :shared ("vendor")
   :copy ("local.env")
   :files ((".widgetrc"
            . ,(lambda (sandbox)
                 (format "cache_dir = %s/widget-cache\n"
                         (+agent-shell-sandbox-tmp sandbox)))))
   :write ("~/.cache/widget-lsp" (regexp . "^/private/tmp/diff\\."))
   :env ("WIDGET_TELEMETRY=0")))
```

To learn what a profile needs, run `M-x +agent-shell-pstack-arena-trace`
with the repository and a command such as its build or test. It
provisions one throwaway candidate with the current profile, as an arena
would, and runs the command there with the candidate's environment. The
sandbox runs in trace mode, which allows and reports every write and read
the candidate profile would refuse, so trace only commands you would run
unconfined. The trace buffer groups the reported
paths by operation, collapses crowded folders, and shows the exit status
and the output's last lines. `C-c C-k` stops the command, and killing the
buffer stops it too.

With a prefix argument, `C-u M-x +agent-shell-pstack-arena-trace` runs
the command under the candidate profile itself rather than in trace
mode, so a refused operation fails as it would in an arena. That proves
a profile works. The buffer shows the exit status, the output's last
lines, and what the profile refused, grouped the same way.

Either way the private folders and the worktree are
removed, even when the command left read-only trees such as Bazel's
outputs in them. A folder that cannot be removed is reported and the
rest of the cleanup still runs. The kernel drops
an occasional report, so trace again before trusting that a path is
absent.

#### On a remote host

A profile with `:host` runs the whole arena on another machine, such as
a Coder workspace. The candidates, their worktrees and the judge
stay there. `:host` is `(coder . NAME)` for the workspace reached as `ssh
coder.NAME`, `(ssh . HOST)` for a plain SSH host, or `(local .
DIRECTORY)`, which runs the host's side as local processes and serves the
tests. `:host-directory` names the repository's checkout on the host:

```elisp
(add-to-list
 '+agent-shell-pstack-arena-profiles
 '("~/src/big-repo/" :host (coder . "example-ws")
   :host-directory "~/src/big-repo"))
```

The rest of the profile travels with each job as plain data. Its
functions are called on the laptop against a stand-in sandbox whose
folders are `@ROOT@`, `@TMP@` and `@CONFIG@`, and the host fills in its
own. The host's setup writes `~/.local/share/pstack-arena/ready.json`,
which names its Emacs and adapters, and
`~/.local/state/pstack-arena-remote/runner-init.el`, which configures its
agents. A host without the first is refused.

The lead's request is the same, and so is its follow-up. The follow-up
adds one line naming the host. The panel moves through `preparing`,
`submitting`, `remote`, `fetching` and `finished`, and its members show
the states the host reports. The laptop:

- snapshots the base;
- keeps it under `refs/pstack-arena/<id>/base`;
- bundles it against a commit the host already has;
- extends a Coder workspace's stop time to cover the arena's deadline;
- uploads the job, polling every 30 seconds after that.

The results land in `refs/pstack-arena/<id>/<role>` in the lead's
repository, as local ones do. A base that changes Git LFS files is
refused. Arenas sharing a host checkout run one at a time.

Each remote arena keeps a record on disk. After Emacs restarts, the
library re-attaches to every arena that has not delivered its results.
It gives the follow-up to whichever shell resumes the lead's session.
`c` in the panels view cancels an arena, and so does ending the lead's
shell. A cancel captures what the candidates made and skips the judge.
Quitting Emacs leaves the arena running.

A host that stops answering for 15 minutes gets the lead an "unreachable"
follow-up. `M-x +agent-shell-pstack-arena-reattach` resumes polling it.
`x` and `M-x +agent-shell-pstack-arena-clean` clean both sides. See
`docs/AGENT-SHELL-PSTACK-ARENA-REMOTE.md`.

### Delegate panels

A delegate hands the whole task to one agent the lead picks. The agent
implements it, runs the tests and commits on a new branch,
`pstack/delegate/<panel-id>`. By default it runs unsandboxed, in its
agent's full-access mode from `+agent-shell-sandbox-agents`:
`bypassPermissions` for Claude Code and `agent-full-access` for Codex. It
then needs no sandbox backend, so it works on Linux and macOS alike, but
its repository needs a commit and no submodules, as an arena's does.

The repository's profile decides where and how it runs, once, when the
request arrives; see `+agent-shell-arena-git-delegate-options`. A profile
with `:host` runs its delegates on that host, as its arenas do;
`:delegate (:host local)` keeps them here. `:delegate (:sandbox t)`
confines a delegate to its workspace, here or on the host, and Emacs
makes its one commit; see below. The agent-supplied request never
chooses any of this.

The request names exactly one member and no rubric or judge:

````
```pstack-panel
{"version":1,"shape":"delegate","brief":"Add rate limiting to the uploads
 endpoint and cover it with tests","timeout":2700,
 "members":[{"role":"rate-limit","agent":"codex"}]}
```
````

- `members` lists one member, on an agent in `+agent-shell-sandbox-agents`.
  Emacs rejects zero or several members, an agent without a full-access
  mode, and the arena fields `rubric`, `judge` and `judge_timeout`, each
  with a reason the lead can act on.
- `timeout` bounds the whole run, preparation included. It defaults to
  `+agent-shell-pstack-panel-default-delegate-timeout`, 2700 seconds, and
  is at most 3600.

A request found mid-turn waits for the lead's interrupted turn to end, as
an arena's does. The delegate's shell is hidden from `agent-shell-buffers`.
A permission request rejects and ends it, since its mode should never ask.

#### Isolation

By default the delegate gets a git worktree of its own under
`+agent-shell-pstack-arena-directory`, on its new branch at a snapshot of
the lead's working tree, taken as an arena's base is: staged, unstaged
and untracked files that are not ignored, after the lead's interrupted
turn has ended. So the delegate sees what the lead sees. A clean tree's
snapshot is `HEAD` itself; otherwise it is a commit on top of `HEAD`,
made with the arena's identity, and the lead's checkout, index and
stash stay as they were. The repository's profile provisions the
worktree as it would an arena candidate: `:shared` links, `:private`
and `:copy` clones, and the lead's own copies of the `:files` paths.

A repository too large for a second worktree can have the delegate work in
the lead's own checkout instead. Its profile in
`+agent-shell-pstack-arena-profiles` says so with `:delegate`, whose keys
are `:isolation`, `worktree` or `branch`, `:host`, only ever `local`,
`:sandbox`, `t` or `nil`, and `:link`, below; a key it does not know, or
names twice, is an error:

```elisp
(add-to-list
 '+agent-shell-pstack-arena-profiles
 '("~/src/big-repo/" :delegate (:isolation branch)))
```

Emacs then refuses a request while the checkout has changes, and lists up
to ten of them. A clean checkout switches to the delegate's branch until
the delegate finishes. Leave it alone meanwhile; the panels view and the
lead's mode-line help say so. When the delegate has finished, Emacs
switches the checkout back to the branch or commit it was on, but only if
the checkout is clean and still on the delegate's branch, and then forgets
the delegate's record. Otherwise it leaves the checkout where it is, keeps
the record and says why. Branch isolation is laptop-only: with an
effective host Emacs refuses it before making anything, and says to use
`:delegate (:host local)`.

#### Linked folders

A worktree has none of the lead's ignored folders, such as installed
packages, so a delegate's tests may not run there. `:delegate (:link
("DIR" ...))` names such folders, relative to the top level. Before the
delegate starts, Emacs symlinks each one the lead has from the lead's
checkout into the worktree, recording it in the manifest first. It
removes the links, never what they point at, before it collects what the
delegate left and before it cleans the workspace, so they never reach a
commit or the list of uncommitted paths. A sandboxed delegate may read
the linked folders but not write them.

The profile check is strict: each path is relative, without `..`, and
untracked and ignored by git in the lead's checkout. `:link` with
`:isolation branch` is an error, since the delegate then works in the
lead's checkout already, and a delegate on a host refuses it before
anything starts. Each link must be ignored as a symlink too, so a
`.gitignore` pattern with a trailing slash, such as `elpaca/`, which
matches only folders, fails the preparation; use `/elpaca` instead.

This repository ignores `/elpaca`, so to let delegates run its tests, add
its entry yourself:

```elisp
(add-to-list
 '+agent-shell-pstack-arena-profiles
 '("~/.emacs.d/" :delegate (:link ("elpaca"))))
```

#### Sandboxed delegates

With `:delegate (:sandbox t)`, the delegate runs as an arena candidate
does: its host login is admitted first, it gets the access token and no
refresh credential, and it writes only its workspace, its private folders
and the profile's `:write` paths. Its git metadata stays read-only even
where `:write` overlaps it: the worktree's `.git` pointer, its own git
directory and the common one, or the checkout's `.git` in branch
isolation. So it cannot commit, write a ref or change git config. Its
prompt tells it to leave its changes uncommitted and end its final
message with exactly one block:

````
```pstack-commit
{"version":1,"subject":"[uploads] Add rate limiting",
 "body":"Reject requests above the configured upload budget."}
```
````

Once every one of its processes has stopped, Emacs makes at most one
commit of the workspace, through a private index, as the laptop user's
git identity: `user.name` and `user.email` as the lead's repository
reads them, conditional includes and all, never the environment's
`GIT_AUTHOR_*`, the host's, or the arena's. Both author and committer
are that identity, the message is the block's subject and body as given,
and nothing is signed or appended. A request is refused before anything
starts when that identity is missing or holds `<`, `>` or control
characters.

The block must close the final response of a turn that completed: the
report is written with a `ROLE.final.json` marker holding its hash, and a
partial, cancelled or timed-out response never proposes a commit, even
when it holds a block. Duplicate or unknown keys, a multi-line subject,
control characters, trailers such as `Co-authored-by:` and messages over
64 KiB are refused too. Then, or when its processes could not be shown to
have stopped or its branch moved, the follow-up says `No commit created`,
why, and where the changes stay. A tree identical to the start makes no
commit.

The commit is journaled in the manifest: its parent, tree, message,
identity and dates are saved before it is made, its id before the branch
moves to it by compare-and-swap. Collecting again after a crash reuses or
remakes the same commit, so a delegate never gets two, and a branch that
moved meanwhile is left alone.

#### On a remote host

A delegate whose profile names a `:host` runs there in a worktree of the
host's checkout, on `pstack/delegate/<panel-id>` at the same snapshot of
the lead's working tree a local worktree gets, which reaches the host in
the job's base bundle. The job travels as an
arena's does, through the same submit, poll, fetch and clean calls; see
`docs/AGENT-SHELL-PSTACK-ARENA-REMOTE.md`. The host's readiness file must
list the capability `"delegate":1`.

An unsandboxed remote delegate uses the host's own agent logins and
commits itself, with `GIT_AUTHOR_*` and `GIT_COMMITTER_*` naming the
laptop user's identity in its shell's environment. A sandboxed one runs
in the host's sandbox, and the host's runner makes its one commit as
above, as the laptop identity the job carries. When the delegate ends,
its branch comes back as `refs/heads/pstack/delegate/<panel-id>`,
checked against the report and the start, and never over a branch of
that name already here at another commit. Its commits are listed from
the fetched tip.

The panels view shows where it runs, its branch and its workspace on the
host. Its member line opens a read-only transcript with `RET`, even after
it finished: a bounded record of its agent text, tool calls and
lifecycle notices, fetched by the same polls, which come every five
seconds while the transcript is shown. `g` there polls now and `q` closes
it. It is text, never a shell: nothing there reaches the agent. A
`Report:` line opens the saved final report.

#### The panels view

The delegate's line in `C-c P` shows its shell, and `RET` opens it, live,
while it runs; a remote delegate's opens its transcript instead. `c` cancels it. A worktree delegate's line shows its
workspace, and once it has finished, `x` removes the worktree. A
branch-mode delegate's line shows the checkout's state, and `x` switches
the checkout back if Emacs could not, then forgets the record.

Cleaning then deletes the delegate's branch, but only when the branch your
checkout is on has every commit the delegate's branch has since its base,
by ancestry, as after a merge, or as an equal patch by `git cherry`, as
after a rebase or cherry-pick, and no worktree has it checked out. A merge
commit counts only by ancestry. A branch with any other commit stays, and
the cleanup message says why: how many of its commits your branch lacks,
or where it is checked out.

#### The follow-up

The brief tells the delegate not to end its turn while a command it
started is still running. When its turn completes anyway with such a
command running, Emacs asks the same shell once to wait for its commands
and end its turn again with its report, and kills nothing. A command
counts as running when one of the delegate's execute tool calls since the
brief ran it: a process the drive library's walk of the adapter's tree
and process groups finds whose command line holds that tool call's
command, or one of its simple commands when it ran in the background,
and that process's descendants. MCP servers, hooks, execution hosts and
anything else not traceable to a tool call never count, though Emacs
still stops them with the delegate. A second turn that ends with
commands still running settles as usual, and the follow-up names them. A
remote delegate's runner does the same on its host.

Once the delegate's turn ends, times out or is cancelled, Emacs stops its
adapter's processes and process groups, never a whole directory's, then
lists the branch's commits since the commit it started from. The lead gets
one follow-up:

```
pstack panel 3f9a0cd2e701 (delegate) has finished: rate-limit (codex): done.
Branch: pstack/delegate/3f9a0cd2e701, 2 commits since <start>, the commit it started from:
  d4e5f6a7b8c9 Add a token-bucket limiter for uploads
  00aa11bb22cc Add limiter tests
Workspace: ~/.local/state/pstack-arena/3f9a0cd2e701-7e2b9a04/rate-limit
Report:
...
To review: git log <start>..pstack/delegate/3f9a0cd2e701
To merge: git merge --ff-only pstack/delegate/3f9a0cd2e701, or push it and open a pull request.
```

A delegate that started from a snapshot of uncommitted work says so in a
`Started from:` line naming the snapshot commit and the `HEAD` under it.
Its review lines then separate the delegate's own commits,
`git log <snapshot>..<branch>`, from everything including the snapshot,
`git log <head>..<branch>`. Merging the branch also brings the snapshot
commit, so the follow-up says to commit your own work first or rebase
the delegate's commits onto yours with `git rebase --onto`.

A branch-mode follow-up says instead whether Emacs switched the checkout
back. A sandboxed delegate's says whether Emacs made its commit, and if
not, why. A remote delegate's names where it ran, its workspace on the
host and the uncommitted paths left there, apart from the branch, and
says when the branch could not be brought back here. A timeout or a cancellation keeps every commit the delegate made. A
worktree's uncommitted changes are named, since cleaning the workspace up
loses them.

A delegate that made no commits leaves an empty branch. Emacs deletes it
when the branch still points exactly at the commit the delegate started
from and no worktree has it checked out, and the follow-up says so in
place of the review and merge lines. A branch-mode delegate's branch goes
once the checkout is back; a worktree delegate's stays with its worktree.
When preparation fails after the branch was made, Emacs discards what it
made, deletes the branch under the same rules, and the rejection says
so. A branch with commits of its own, or one moved since the start, always
stays.

#### Recovery

Every delegate keeps a manifest under `+agent-shell-pstack-arena-directory`.
A branch-mode manifest records where the checkout was before Emacs switched
it, and is written before the switch and again after Emacs tries to switch
back. If Emacs exits while a branch-mode delegate runs, the next Emacs lists
it in `C-c P` as interrupted, and `x` returns the checkout under the same
rules. Returning it twice does nothing the second time. A record whose
checkout came back is gone, so `C-c P` lists only the ones still left on
the branch. A process that outlived the delegate's stop also keeps the
checkout on the branch, and the panels view names it.

A remote delegate's runner never runs the delegate twice. Before the
adapter starts, the runner writes a launch record: on a systemd host, the
adapter runs in a transient scope of its own,
`pstack-delegate-<job>-<role>`, which stopping ends with every process in
it; elsewhere the record holds the adapter's pid and start time. A
restarted runner stops what the record owns, marks an unfinished
delegate as interrupted, and collects; a saved final response with a
matching marker still gets its commit. When the writers cannot be shown
to have stopped, the work stays, nothing is committed, and the host's
repository lock stays quarantined until someone looks.

Roles map to agent configs rather than model slugs; record that fork with its
reason. `lisp/+agent-shell-drive.el` holds the session start, delivery and
event watching shared with `agent-shell-projects`.

## Layer 3: views

`C-c P` in an agent shell runs `+agent-shell-pstack-panels`. It lists every
panel since Emacs started, with its id, shape, phase or status and lead, and
each member's role, agent, state and report. An arena also shows its folder
and each candidate's capture, and a remote one its host and the host's
phase. `RET` visits the report, the arena folder, or the member or lead
shell, `x` cleans up a finished arena or delegate, and `c` cancels an
unfinished panel. Below the panels it lists each branch-mode delegate an
earlier Emacs left unreturned. The buffer redraws whenever a panel changes.
The decision trail and plan views are planned.

If a lead's latest turn contains a block that starts no panel, `C-c P`
shows a **Last request** line with the outcome. Press `l` on that line or
on a panel's row to run `+agent-shell-pstack-panel-log` for its lead. You
can also run the command from the lead shell. The read-only log shows the
last 50 handled events per lead, newest first, with time, session, final
text length, whether it ends in a fenced block, and the outcome or error.
It records when a split final part was recovered from the whole turn.
The log stays in memory and retains at most 200 characters of message text
and 200 characters of the block's first line per event. Request errors
also appear in `*Messages*`.

While a lead has a panel running, its mode line shows `panel 1/3`, the
settled and total member counts, next to `pstack`. Hovering lists each
member's role, agent and state, and `mouse-1` opens the panels view. The item
updates only when a member or panel changes state and disappears once every
member has settled.

Member shells stay out of `agent-shell-buffers`, so
`agent-shell-switch-buffer`, project reuse through `SPC p a` and the agent
projects author picker never offer them. Reach them from the panels view.

## Non-goals

- Reimplementing playbooks in Elisp.
- Routing every session into poteto-mode automatically. Upstream keeps it
  opt-in.
- Reusing `C-c C-m` or `C-c C-o`, which agent projects owns.
