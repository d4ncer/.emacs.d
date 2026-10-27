# pstack arena panels on a remote host: design

Status: revision 2, implemented except the Linux sandbox. The transport,
base transfer, runner and laptop side live in
`lisp/+agent-shell-arena-remote.el`, with hooks in
`lisp/+agent-shell-pstack-panel.el` and `lisp/+agent-shell-arena-git.el`.
They run end to end on a `(local . DIRECTORY)` host, whose runner still
uses the macOS sandbox. [The implementation](#the-implementation)
describes them, and [how it departs](#how-the-implementation-departs)
lists every difference from this design. The Linux sandbox backend is
separate work in `lisp/+agent-shell-sandbox.el`.

Revision 1 was attacked by interrogate
panel `6799a86a4a5d` (`var/pstack-panels/6799a86a4a5d/`). Its findings
and the probes they prompted changed the sandbox, credentials, versioning
and lifecycle sections below. The triage table at the end maps each
finding to what changed. It extends `docs/AGENT-SHELL-PSTACK-ARENA.md`, where
candidates run on the laptop under `sandbox-exec`. Here an arena runs on
a remote Linux host, such as a Coder workspace. Emacs on the laptop only
sends the job and receives the outcomes.

## Why

On the laptop, a candidate in a large monorepo, with hundreds of thousands
of files, costs 2 to 3 minutes and about 10 GB for its worktree, plus
Bazel's load on the machine. A remote workspace is built for that work and
can keep running while the laptop sleeps. Other repositories keep running
locally.

## Ground truth

Measured on a large Linux Coder workspace on 2026-10-26. The host's own
details and the records of its live runs stay in the user's private notes.

- **Machine.** Ubuntu 22.04, kernel 6.8, 16 CPUs, about 120 GB of RAM and
  a 1 TB home volume.
- **Repository.** A full checkout of a large monorepo, with a `.git` of
  tens of gigabytes. Bazel, bazelisk, Nix and tmux are installed.
- **Agents.** Claude Code and codex-cli are installed, and the user logged
  both in once. Claude Code keeps its credentials in
  `~/.claude/.credentials.json`, and Codex in `~/.codex/auth.json`. Codex's
  browser login needed the forward `ssh -L 1455:localhost:1455`, because
  its callback server runs on the host.
- **ACP adapters** are not installed, but they are on npm:
  `@agentclientprotocol/claude-agent-acp` 0.95.0 and
  `@zed-industries/codex-acp` 0.16.0.
- **Emacs** is not installed. apt offers 27.1, which is too old for these
  libraries (`sort` with `:key` needs Emacs 30). Nix offers 31.1, and
  agent-shell needs 29.1.
- **Sandboxing.** `bwrap` is installed and works. It ran with a read-only
  root bind and a private tmpfs `/tmp`, and a write to `/etc` failed with
  "Read-only file system". Landlock is in the LSM list,
  `kernel.apparmor_restrict_unprivileged_userns` is 0, and user namespaces
  are available.
- **Moving the base.** A direct `git push` of a laptop commit to the
  host ran for more than 10 minutes and was killed. The laptop lacked
  the host's newer refs, so it could not know which objects the host
  already had. A bundle of only the commits after the merge base with
  `origin/master` was a few kilobytes, built in under a second, and moved
  and fetched in about 6 s, including fetching the merge base from GitHub.
  The host got the exact same commit id.
- **Lifetime.** The workspace stops itself some hours after it starts,
  extended with activity. It is not yet known whether a running process
  counts as activity. The host's own `coder` CLI has
  `coder schedule extend`, and `systemd-run --user --wait true` works there.
- **What the host exposes.** Probed after the panel:
  - Unix sockets: container runtime sockets, the user's session bus,
    agent sockets such as gpg-agent's, the Nix daemon socket and the
    editor server's, plus abstract sockets, which have no path to hide.
    The container runtime gives root on the box, and the session bus can
    start unsandboxed commands through `systemd-run --user`.
  - The environment: the Coder agent's token, which can control the
    workspace, the forwarded SSH agent and a git credential helper.
  - Home: SSH keys, cloud credentials, container registry logins and the
    folders of internal developer tools.
- **Network isolation.** `bwrap --unshare-net` works but leaves only
  loopback, so DNS fails. `slirp4netns` 1.0.1 is installable from apt and
  would give the namespace outbound access without the host's sockets.
  `pasta` is not available.

## Shape: the host runs the whole arena

The remote host runs the arena's lifecycle from preparing to finished:
provisioning, sandboxed candidates, stopping, capture, judge and
cleanup. Emacs on the laptop submits a job, polls its status, fetches its
results, and builds the lead's follow-up from them.

The runner is the same Elisp arena code, running in a headless Emacs 31 on
the host. It reuses the existing phase machine, capture, judge and
manifest. Only the sandbox backend and the transport are new.

### What crosses the wire

To the host, once per arena:

1. **The job.** A JSON file holding:
   - the shape, brief, rubric, candidates, judge and timeouts;
   - the lead's repository-relative prefix;
   - the base commit id.
2. **The base.** A git bundle of `base ^prerequisite`, where the base is
   the laptop's snapshot commit of the lead's working tree, built exactly
   as a local arena builds it. The prerequisite is negotiated, not
   assumed. The host reports its `origin/master` tip. The laptop picks
   their merge base if it has that commit. Otherwise it uses the merge base
   with its own `origin/master` and has the host fetch it from `origin`.
   The host runs `git bundle verify` before fetching. If the prerequisite
   is unavailable, it reports which one, and the laptop rebuilds the bundle
   against an older ancestor. The job also carries the lead's sparse
   patterns, and the host applies them to each worktree. A base or result
   that needs LFS payloads the other side lacks is refused or flagged in
   the first version, since bundles carry only LFS pointers.

To the host during a run: **cancel**, when the lead's shell ends or the
user cancels in the panels view.

To the laptop:

1. **Status.** A small JSON file the runner rewrites on every transition,
   holding the phase and each member's state. The laptop polls it over SSH
   every few seconds, and the panels view and mode line read it.
2. **Results.** When the arena finishes:
   - a bundle of every result ref against the base, fetched into the
     laptop's `refs/pstack-arena/<id>/<role>`;
   - a JSON report with each candidate's state, diff stat and rationale,
     and the judge's report.

   The lead's follow-up and graft commands stay as they are today.

Nothing else crosses: no worktrees, Bazel output, credentials or live
agent streams. Seeing a candidate's live work would take a command that
opens its log on the host over SSH.

### The Linux sandbox backend

`+agent-shell-sandbox` gains a second backend for the same spec. Where the
macOS profile denies a list and allows the rest, the Linux sandbox shows
an allowlist and hides the rest, because hiding a few known folders would
leave the sockets and secrets above reachable. A candidate's adapter runs
under `bwrap` with:

- **Filesystem.** `--ro-bind` of the system paths it needs (`/usr`,
  `/etc`, `/nix`, `/opt` and the toolchain folders the profile names), a
  writable `--bind` of its worktree, private temp folder and private
  config folder, and a private, empty `$HOME` holding only what the
  profile adds. The main checkout, other candidates' worktrees and private
  folders, the runner's records and every real credential stay outside.
- **Namespaces.** `--unshare-pid`, `--unshare-ipc`, `--unshare-net` with
  `slirp4netns` for outbound access, a fresh `--dev` and `--proc`, and
  `--die-with-parent`. With its own network namespace, a candidate cannot
  reach the host's abstract sockets or loopback services. Only sockets a
  profile's `:connect` binds in are reachable, and the bind fails loudly
  when the socket does not exist.
- **Environment.** `--clearenv`, then an explicit allowlist (`PATH`,
  `HOME`, `LANG`, `TERM`, the temp variables, the profile's `:env` and the
  agent's own login entries). `CODER_AGENT_TOKEN`, `SSH_AUTH_SOCK` and the
  rest never reach a candidate.
- **ACP file requests.** agent-shell serves the agents' file requests in
  the runner, outside the namespace. On Linux the gate in
  `+agent-shell-drive--file-allowed-p` allows only paths the candidate can
  see inside its namespace, so it never reads the runner's `/proc` or a
  hidden file.
- **Stopping.** Killing `bwrap` ends the candidate's PID namespace and
  everything in it. The macOS-only process walk (`/bin/ps -axo`) and the
  `lsof` sweep are replaced by this on Linux, and cloning uses `cp
  --reflink=auto` in place of macOS's `cp -c`.

Profiles keep their keys with Linux meanings: `:read` and `:write` become
read-only and writable binds of existing paths, `:connect` binds an
existing socket, and regex allowances are refused on Linux, because a
mount needs a path.

### Credentials

Credentials live on the host, so nothing is sent per job. Candidates and
the judge sign in with the access token of the host's own login and never
get a refresh token, so there is nothing to rotate and nothing to copy
back. A runner admits an arena as a laptop does: its panel checks every
agent's host login before any member starts, refreshes a short one once on
the host under the shared per-agent lock, and refuses the whole arena if
one still falls short. Host logins in
[the arena design](AGENT-SHELL-PSTACK-ARENA.md) has the details. macOS and
Linux hosts behave alike.

The status's `logins` field records each agent, whether the check
refreshed it, and the seconds its token had left, with no token values.
The runner writes each agent's result as its check ends. The laptop's host
line shows them, as in `logins: claude-code refreshed, 8.0 h left`.

The copy-back rule is gone everywhere. It could not survive two sandboxes
refreshing at once, and it promoted sandbox-written data into the host's
real login.

**Measured on the remote host, 2026-10-27.** With `expiresAt` set a
minute in the past, a fixture remote arena with one Claude Code candidate
and a Claude Code judge started under the shipped runner. Its status
recorded `claude-code` as refreshed with about 8 hours left. The host's
refresh token had changed, and the candidate and judge both finished.
Codex's token lasted more than 9 days, and `codex exec` did not refresh
it when only `last_refresh` was old. Whether it refreshes a token near its `exp` is untested, which is
why its refusal says a refresh could not be triggered.

### Setting up the host and versions

The user's private setup playbook runs on every boot. It installs only
what changes rarely, each at a pinned version: Emacs 31 from Nix, the two
ACP adapters from an `npm ci` lock, `slirp4netns` from apt. When the
installation passes a smoke check, it writes a readiness file listing the
versions. A submission checks that file first and refuses an unready host.

The runner's code is not installed by the playbook. Each job ships it:
the arena libraries and the agent-shell, acp.el and shell-maker sources the
laptop runs, as a tarball named by its content hash. The host keeps each
hash it has seen, so a code change costs one upload, and the runner always
runs exactly what the laptop has. The job, status and report formats carry
their own version number, so a laptop that updated mid-arena still reads
an older runner's results. Profiles, which can hold functions, are resolved
on the laptop and sent as plain data in the job.

### Laptop side

A profile gains `:host`, naming a remote host: `(coder . "example-ws")`
for a Coder workspace reached as `ssh coder.<name>`, or a plain SSH host.
The monorepo's private profile sets it. Other repositories leave `:host`
unset and run locally.

A remote arena gets its own phases on the laptop: preparing, submitting,
remote, fetching and finished. Its members mirror the runner's states read
from the status file and never go through the local settle and halt path.
The laptop keeps a record of each remote arena on disk, holding:
- the panel id and the lead's session id;
- the host, the remote arena id and the base;
- the last phase seen, and whether results were fetched and delivered.

After an Emacs restart or a library reload, it re-attaches by session id
and finishes the fetch and delivery. The base gets a local
`refs/pstack-arena/<id>/base`, so the follow-up's diffs survive gc.

Transport runs SSH asynchronously, one call in flight at a time, through a
shared `ControlMaster` connection with `ConnectTimeout` and
`ServerAliveInterval`. It polls every 30 seconds with backoff. Submission
uploads the job and starts the runner in one round trip, through
`systemd-run --user`, so the runner outlives the SSH session.

The runner writes its status through a temporary file and a rename, with a
heartbeat, its pid and a wire version. It writes results before the status
that announces them. It has an overall deadline from
`+agent-shell-pstack-panel-arena-budget`, as a local arena does:
preparation allowance plus the candidates' timeout plus the judge's
timeout plus overhead. It extends the
workspace's stop time with `coder schedule extend` while it runs, and
refuses a job when less than that deadline remains.

The laptop has a deadline of its own. When the heartbeat stops, it
delivers a follow-up saying the arena is unreachable, with a command to
re-attach. Cancel is a marker file in the job folder, which the runner
checks at start and on every tick. The laptop keeps a cancel it could not
send and retries it. Cancelling captures what candidates produced and never
starts the judge, as locally. Quitting Emacs does not cancel; the runner's
deadline covers that.

On the host, a runner that finds an unfinished job captures what its
candidates left as timed out, and reports it. Killing `bwrap` left no
writers, so this is safe. A host-side command lists and cleans arenas, and
`+agent-shell-pstack-arena-clean` cleans both sides from the laptop's
records.

Arenas sharing a host repository run one at a time in the first version.
A running candidate can read the common git directory, so a second arena's
candidates could read the first's captured objects. Per-arena object stores
are a later option.

## The implementation

### Records

Four records are versioned JSON, each a kind in
`+agent-shell-arena-remote--kinds`. Every file carries `version` and
`kind`. A reader takes versions 1 up to its own, refuses a newer one
with "update this Emacs", ignores fields it does not know, and checks
every field's type at the boundary. Ids must be names safe in a path and
a ref, commits full hex ids, and agents identifiers. An error names the
field, such as `status.members[0].agent`.

- **The job**, `job.json`: the panel id, brief, rubric, timeouts,
  candidates, judge, the lead's prefix, the base, its prerequisite, the
  lead's sparse patterns, the profile as plain data, the host checkout,
  the code hash, and the runner's deadline in seconds.
- **The status**, `status.json`: the runner's phase (`queued`, then the
  panel's), its pid, heartbeat and deadline, the host's arena id, each
  member's state, capture and error, and whether results are written.
- **The report**, `report.json`: each member's state, capture, text,
  error, result commit, diff stat and capture failure, and any rejection.
- **The laptop's record**, `remote.json` in the laptop's arena folder:
  the panel and session ids, the host, the laptop phase, the cancel
  state, whether the job was submitted, the results fetched and the
  follow-up delivered, the last heartbeat and when it changed, the
  restarts, the host's arena id and Emacs.

The host's setup writes `~/.local/share/pstack-arena/ready.json` once its
smoke check passes, and deletes it on every boot first. It has its own
format, version 1, with no `kind`. `emacs` is required, and `written_at`,
`adapters_bin`, `claude_agent_acp`, `codex_acp`, `node`, `bwrap`,
`unshare` and `slirp4netns` are optional. Each tool is an object with
`path` and `version`:

```json
{"version":1,"written_at":"2026-10-26T10:29:45.684Z",
 "emacs":{"path":"/nix/store/...-emacs-nox-31.1/bin/emacs","version":"31.1"},
 "adapters_bin":"/home/user/.local/share/pstack-arena/adapters/node_modules/.bin",
 "codex_acp":{"path":".../codex-acp/dist/index.js","version":"2.2.2"}}
```

A submission refuses a host without the file, with an unknown version or
a malformed tool, with an Emacs older than 30, or with a file written
before the host last booted. Setup deletes the file on every boot, so an
older one means setup has not finished since. Both times are compared as
epoch seconds, so the host's time zone does not matter. It starts the runner
with that Emacs. The job carries the file as the laptop read it. The
runner reads it again, puts `adapters_bin` on its `PATH` for
`runner-init.el`, and copies it into the report. The follow-up's host
line then names the Emacs and adapter versions that ran. A local host's
file is `ready.json` in its state folder.

### Phases on the laptop

```
preparing ─▶ submitting ─▶ remote ─▶ fetching ─▶ finished
                  │            │ ▲        │
                  │            ▼ │        ▼
                  │        unreachable ◀──┘
                  └─ rejection ─▶ finished
```

`+agent-shell-arena-remote--transitions` is the table, and every move
saves the record. `preparing` snapshots the base as a local arena does.
`submitting` probes the host, negotiates the prerequisite, bundles,
uploads and starts the runner. `remote` polls. `fetching` lands the
results. A host silent for `+agent-shell-arena-remote-unreachable-after`
(900 s) makes the arena `unreachable`, and only a re-attach moves it
back to `remote`. A failed fetch goes back to `remote` and is retried.
The panel shows the phase, and members take the states the status gives
without the local settle and halt path.

### Transport

Each call runs `sh -c SCRIPT` in the host's state folder: through `ssh
coder.NAME` or `ssh HOST`, or as a local process for a local host.
`+agent-shell-arena-remote--call` keeps one call in flight per host, so
polls never overlap and a submission never races a cancel.

- **SSH options.** Every call shares a `ControlMaster` connection kept 10
  minutes. It uses `BatchMode`, a 15-second `ConnectTimeout`, and
  `ServerAliveInterval` 15 with `ServerAliveCountMax` 3.
- **Timeouts.** Each call has its own timeout: 120 s for a poll, 900 s
  for a submission or a fetch. A call that outlives it is killed and
  reported.
- **Polling.** Polls come every 30 s. After failures the wait doubles,
  up to 10 minutes.
- **Files.** Standard input and output can be local files, so the upload
  and the results each travel in one call.
- **Tests.** `+agent-shell-arena-remote-spawn-function` is the process
  layer, which tests stub.

The scripts are constants in the library, so the host needs nothing of
this code installed:

| Script | Arguments | Does |
|---|---|---|
| probe | checkout, code hash, readiness file | Reports the checkout's top level, its `origin/HEAD` tip (or `HEAD`), whether the code is there, the readiness file's absolute name, the boot time from `/proc/stat`, and the readiness file's contents. |
| submit | id, checkout, Emacs, launcher | Unpacks the upload on standard input into `jobs/<id>/`. Installs the code under `code/<hash>/` if new. Verifies the bundle, fetching the prerequisite from `origin` if missing. Fetches the base into `refs/pstack-arena/<id>/base`. Starts the runner. Answers `started`, `missing`, `code` or `failed`. A started job answers `started` again. |
| poll | id, cancel, reason | Writes the cancel marker when asked, then reports whether the runner lives, whether a marker exists, and the status. |
| resume | id, Emacs, launcher | Starts the runner again unless it runs. |
| fetch | id | Writes the report and results bundle as a tar stream. |
| clean | id, Emacs | Waits up to 20 s for a finished runner to exit. Then removes the host's arena through its manifest, the base ref and the job folder, unless the runner still runs. |

The runner starts through `systemd-run --user` on an SSH host and through
`nohup` on a local one, so it outlives the call either way. Its systemd
unit has `MemoryMax=50%` and `OOMPolicy=continue`; see [runner
memory](#runner-memory).

### Base and code

The laptop keeps the base under `refs/pstack-arena/<id>/base`, recorded
as the manifest's `base` role so cleanup removes it. `base` is therefore
a reserved role name. `+agent-shell-arena-remote-prerequisites` returns
the commits to cut the bundle against, in order:

1. The merge base with the host's tip, when the laptop has that commit.
2. The merge base with the laptop's own `origin/HEAD` or upstream. The
   host fetches it from `origin` if it lacks it.
3. Nothing, a bundle of the whole history, when neither exists.

On `missing`, the laptop tries the next rung. When all fail, it rejects
the arena, naming the commits tried.

A base that changes Git LFS files after the prerequisite is refused,
since the host would lack their contents. A result that changes one is
flagged in the follow-up as locally, with a note that the contents stayed
on the host.

The runner's code is the libraries in `+agent-shell-arena-remote-libraries`
plus every source file of the packages in
`+agent-shell-arena-remote-packages` (agent-shell, acp, shell-maker), as
this Emacs loads them. Its hash is the SHA-256 of their names and
contents, and the tarball is built once per hash under
`<arena directory>/code/`.

### The runner

`+agent-shell-arena-remote-runner-batch` runs under `emacs -Q --batch`
with the shipped code on its load path. Before the job, it loads
`runner-init.el` from the host's state folder, which configures the
agents: their configs hold functions, so they cannot travel in a job.
Then the runner:

1. Writes its pid. If an earlier status is finished, it exits.
2. Waits in `queued` until it holds the host repository's lock,
   checking the cancel marker on every tick.
3. Builds a panel without a lead from the job. The panel's sink writes
   the results instead of delivering a follow-up. It gives the panel a
   base at the fetched commit, fixes its deadline, and runs the panel's
   own provisioning, login admission, candidates, capture and judge.
4. Rewrites `status.json` through a temporary file and a rename,
   whenever the panel changes and every 10 s as a heartbeat.
5. On a cancel marker, cancels the panel with the marker's reason. That
   captures what the candidates made and skips the judge.
6. At its deadline, settles the running members as timed out. Two
   minutes later it reports regardless.
7. When the panel finishes, bundles every result ref against the base,
   writes `report.json`, and only then writes the final status.

It never prompts and never asks the host to stay awake: it binds
`agent-shell-inhibit-system-sleep` to nil, since the host's D-Bus refuses
a runner. A runner that finds an earlier, unfinished status from
a runner that died captures each candidate's worktree as `timeout`,
marks the judge as stopped, and reports. One that died while preparing
reports a rejection and removes the partial arena.

The host keeps its state under `~/.local/state/pstack-arena-remote/`:

- `runner-init.el`;
- `code/<hash>/`, the runner's code;
- `jobs/<id>/`, each job's files and reports;
- `arenas/`, the arenas' worktrees and checkouts;
- `locks/`, one lock per host repository.

Candidates cannot read `jobs/`, `locks/` or `runner-init.el`.

### One arena per host repository

The lock is a folder, `locks/<first 16 hex of the SHA-1 of the
checkout's true name>`, made with `make-directory`, which fails when it
exists. An `owner` file inside holds the runner's pid and job. A waiting
runner retries on every tick and reports `queued`, with its heartbeat
going. A lock whose owner no longer runs is stale. The runner renames it
aside before taking it, so only one waiter wins. The lock is released
once the results and the final status are written. Waiters are not
served in order.

### The laptop

When a profile names `:host`, the panel's preparation snapshots the base
and hands the panel to `+agent-shell-arena-remote-launch`.

Submission first waits until the host is ready. For a Coder workspace it
reads `coder list -o json`. A stopped workspace is started with `coder
start --yes NAME`. A workspace that is starting, or running with its
agent not yet connected, is checked again every 10 seconds. The wait
then probes the host over SSH until it answers with a readiness file
written since it booted. A host that booted less than
`+agent-shell-arena-remote-wake-timeout` seconds ago, 600, also gets the
wait, whatever its kind. A host that is still not ready after that
timeout is rejected with what was last wrong, and a cancel ends the wait.
A running host that fails its first probe is rejected at once, as before.

The laptop extends a Coder workspace's stop time from the laptop, with
`coder schedule extend`, before the upload. It does so again whenever the
runner reports a deadline past the last extension, which a wait in the
queue causes.

When the runner has stopped unfinished and its heartbeat is older than
`+agent-shell-arena-remote-stale-after` (120 s), the laptop restarts it,
at most twice. After fetching, the laptop lands each result in
`refs/pstack-arena/<id>/<role>` and checks it against the report. It
takes each diff locally, so `:outside` and `:lfs` use the lead's sparse
patterns and attributes. Then it composes the follow-up with the local
arena's code. The only addition is a line naming the host.

`+agent-shell-arena-remote-mode` re-attaches every record on disk that
has not delivered, and every 5 s it gives arenas without a live lead the
shell whose session id matches. Turning off panels while Emacs quits does
not cancel. Ending the lead's shell otherwise cancels, as locally.

## How the implementation departs

- **No older ancestors in the prerequisite ladder.** Revision 2 said to
  fall back to an older ancestor. The ladder stops after the upstream
  rung and the whole-history bundle. A host that lacks the merge base
  with the laptop's upstream lacks its ancestors too, unless it is
  stale. A stale host's tip is in the laptop's history, so rung 1
  already covers it.
- **The laptop extends the workspace, and the runner does not check the
  time left.** The experiments found the host's `coder` logged out, so
  the runner cannot read or move its stop time.
- **Profiles cross through placeholders.** Functions in a profile are
  called on the laptop with a stand-in sandbox whose root, temp and
  config folders are `@ROOT@`, `@TMP@` and `@CONFIG@`. The host turns
  each value holding one back into a function of its own sandbox, quoting
  the folders inside a regex allowance.
- **Sparse patterns go in as non-cone patterns.** The host gives each
  worktree the lead's patterns with `git sparse-checkout set --no-cone`.
  Cone patterns match the same paths that way. A sparse index does not
  carry over.
- **Agents are configured on the host.** `runner-init.el` holds them,
  since agent configs hold functions.
- **The cancel marker holds the reason**, which the follow-up shows. A
  cancel before submission stops the arena locally. A cancel during
  submission travels in the upload or with the first poll.
- **The host's arena id differs from the laptop's.** The job id is the
  laptop's arena id. The host's arena folder takes the panel id and a
  suffix of its own, and the results move from
  `refs/pstack-arena/<host arena>/<role>` to the laptop's.
- **The judge runs in a read-only sandbox** over the host arena's
  `checkouts/` folder, as it does on a laptop, so on Linux its root is a
  read-only bind.
- **The panel gained a sink, a remote slot, a change hook, a cancel
  command and a reserved role.** `+agent-shell-pstack-panel` has two new
  slots, schema 6. `+agent-shell-pstack-panel-cancel`, bound to `c` in
  the panels view, cancels any unfinished panel. `+agent-shell-pstack-arena-extra-deny-reads`
  hides the runner's records from candidates.
- **The Linux backend plugs in unchanged.** Arenas are gated on
  `+agent-shell-sandbox-backend` rather than macOS, so a runner on Linux
  accepts them. Each candidate's sandbox gets the runner's deadline, plus
  its two minutes' grace, as `:deadline`, which the panel now passes to
  each sandbox explicitly. The runner's code ships
  `+agent-shell-sandbox-launch.sh`, which the backend finds on
  `load-path`.
- **Results stay on the host until cleaned.** This is as designed, but it
  keeps every candidate's worktree there; see the open questions.

## Experiment results, 2026-10-26

Run on the remote host after revision 2.

**The network namespace.** `bwrap --unshare-net` with `slirp4netns`
attached failed with `setns(CLONE_NEWNET): Operation not permitted`, even
with `--userns-path`. This order worked:

1. `unshare --user --map-root-user --net` creates the user and network
   namespaces.
2. `slirp4netns --configure --disable-host-loopback <pid> tap0` attaches to
   that process.
3. `bwrap` runs inside for the mounts, the PID and IPC namespaces and the
   cleared environment, with `--unshare-user` and the user's own `--uid`
   and `--gid`, so the agent is not root inside.

`/etc/resolv.conf` is a symlink into `/run/systemd/resolve`, so the
sandbox gets a `nameserver 10.0.2.3` file at
`/run/systemd/resolve/stub-resolv.conf` under a private `/run`.

| Check inside the sandbox | Result |
|---|---|
| Interface | `tap0` up at 10.0.2.100 |
| HTTPS to api.anthropic.com and github.com | reached (404 and 200) |
| Host loopback through 10.0.2.2 | refused |
| Abstract unix sockets | none visible |
| The container runtime's socket | absent |
| `~/.ssh` | absent |
| Environment | 5 variables |

With `--ro-bind / /` instead of the allowlist, the container runtime's
socket was reachable even in its own network namespace, because path
sockets are files. The allowlist is necessary.

**Credentials without refresh tokens.** The host's Claude Code access
token had a few hours left. The host's Codex access token is a JWT that
lasts about 10 days.

- **Claude Code** ran `claude -p` in bypass mode inside the full sandbox,
  with only `CLAUDE_CODE_OAUTH_TOKEN` set to the host's access token and a
  private `CLAUDE_CONFIG_DIR`. It wrote a file in its worktree and found
  `~/.ssh` absent. As root inside the namespace, it refused
  `--dangerously-skip-permissions`, which the uid mapping above fixes.
- **Codex**:
  - It refused `codex login --with-access-token`: "agent identity JWT
    payload is not valid JSON". That flag takes another kind of token.
  - A private `auth.json` with `auth_mode` `chatgpt` and the access token,
    ID token and account id worked. Its `refresh_token` has to be present,
    so it is the empty string.
  - `codex exec` with approvals and its own sandbox bypassed then wrote a
    file and found `~/.ssh` absent.
- **The host's login files** kept their modification times through every
  run.

So candidates get only an access token. The arena checks that the token
outlives its deadline and refreshes the host's own login first if it does
not.

**Autostop.** The host's `coder` CLI is not logged in. The laptop's is,
and `coder schedule extend NAME 6h` moved the stop time to six hours from
the command. The laptop extends the workspace at submission to cover the
arena's deadline, so the host needs no Coder token.

## The Linux backend as built, 2026-10-26

**Host setup.** The user's private setup playbook, run on every boot,
installs slirp4netns from apt, `emacs-nox` 31.1 from a pinned nixpkgs
revision as a GC-rooted link at `~/.local/share/pstack-arena/emacs`, and
the adapters with `npm ci` into `~/.local/share/pstack-arena/adapters`.
It removes `ready.json` first and `ready.sh` writes it back once every
tool runs. With everything installed the setup takes a few seconds,
mostly the adapters' smoke check.

**Adapters.** `@zed-industries/codex-acp` stopped at 0.16.0. That build
defaults to `gpt-5.5`, which a ChatGPT login rejects, and newer models
need a newer Codex. The package is now `@agentclientprotocol/codex-acp`,
pinned at 2.2.2, the laptop's version, with its full-access mode still
called `agent-full-access`. `claude-agent-acp` is pinned at 0.95.0 and
brings its own native Claude Code binary.

**Launching.** On GNU/Linux `+agent-shell-sandbox-backend` is `bwrap`. The
client runs `/bin/bash lisp/+agent-shell-sandbox-launch.sh NAMES
BWRAP-ARGUMENTS`. The launcher starts `unshare --user --map-root-user
--net`, waits until that process has its own network namespace, attaches
slirp4netns and waits for its ready pipe, and only then lets the process
become bwrap. bwrap gets `--clearenv` and the named variables through
`--args` on a pipe, so no token is in a command line. slirp4netns exits
when the launcher's end of its exit pipe closes. On SIGTERM the launcher
kills the namespace's first process, named by bwrap's info pipe, and exits
once bwrap has, which is once every process in the namespace has.
`+agent-shell-drive-stop` sends that signal and waits.

`+agent-shell-sandbox-linux-mounts` is the one list of what an agent sees.
It renders bwrap's arguments and decides the ACP read gate, which allows a
path only when its deepest mount binds it in place. Mounts are ordered by
depth, so `ROOT/.git` stays read-only inside a writable root. The agent's
uid and gid are the user's real ones. The Nix daemon's socket folder is
covered by an empty tmpfs, since a socket on a read-only mount still
accepts connections. The adapter's npm `node_modules` is bound read-only
so the adapter can run.

**Smoke run.** Each real adapter started in this sandbox through
`+agent-shell-drive-start`, wrote a file in its root, found `~/.ssh`
absent, answered, and stopped with no survivors, each in about 12 s. The
host's login files kept their modification times.

## Faults found in live use, 2026-10-27

The first remote arena on the monorepo, with a Claude Code and a Codex
candidate, hit five faults.

- **A stopped workspace.** The workspace had stopped itself, so the
  submission's SSH probe timed out during the banner exchange and the
  arena was rejected. That probe woke the workspace, which ran about a
  minute and a half later. Submission now starts and waits for it, as
  above.
- **A refusal without a reason.** The host's Claude Code access token had
  expired hours earlier, so the Linux backend refused that candidate.
  The status carried no member errors, so it showed only `state error`,
  and a restarted runner lost the reason. A status member now carries its
  error, the laptop's panel mirrors it, and recovery restores it.
- **A sweep with macOS tools.** A candidate whose adapter was already gone
  was swept with `/usr/sbin/lsof`, which Linux lacks. The halt signalled
  from its timer, the candidate stayed `stopping`, and the arena stalled
  at the capture barrier. On Linux the sweep now finds nothing, since an
  agent's processes end with its PID namespace. A stop that signals fails
  the candidate's capture with the error, so the others go on.
- **Runner memory.** See below.
- **Sleep-inhibit lines.** The runner's journal had hundreds of lines of
  `Sleep inhibit unavailable`, one per busy turn, since the host's D-Bus
  refuses a runner. The runner no longer asks.

### Runner memory

The first runner's unit reached nearly 40 GB while its candidates ran,
and a restarted one grew by about 15 GB in under a minute. Measured under
a capped scope, the runner's Emacs stayed under 80 MB throughout. The
rest was the unit's children and the kernel memory charged to it:

- Provisioning two candidates peaked near 30 GB, almost all page cache
  and inode slab for two new worktrees of hundreds of thousands of files,
  with git's own memory under 3 GB. Under an 8 GB cap it took the same
  time, about two and a half minutes, since the kernel reclaims that
  cache.
- The restarted runner's recovery made the judge checkout of the changed
  candidate, a full `git worktree add`: nearly a minute and 14 GB alone,
  most of it page cache. With the sparse judge checkout, the whole
  recovery took about 16 s and peaked under 1 GB, against about 90 s and
  the full 8 GB cap before.
- About 10 GB of the first runner was most likely the Codex candidate's
  Bazel server. The monorepo's Bazel configuration lets the JVM take half
  the memory it sees, and inside the sandbox, which has no
  `/sys/fs/cgroup`, it sees the whole host. This share is inferred, not
  measured.

`MemoryMax=50%` on the runner's unit is defence in depth. At that limit
the kernel reclaims page cache first. The limit still leaves half the
host to the user's own work, whose Bazel server also takes up to half.
Past it the kernel kills the largest process, a build rather than the
runner, and `OOMPolicy=continue` keeps systemd from stopping the runner
with it.

## Open questions to settle by testing

1. **Settled: the ACP adapters** run inside the same sandbox; see the
   smoke run above.
2. **Bazel inside the sandbox sizes its heap from the whole host.** The
   sandbox has no `/sys/fs/cgroup`, so the JVM cannot see the runner's
   memory limit. A read-only bind of the runner's cgroup would let it.
3. **What the monorepo's build needs from the host.** Bazel, its private home
   cache and toolchain paths, and whether it needs the Nix daemon socket or
   Docker. The trace command, ported to Linux, answers this.
4. **Bazel on the host.** How cold a candidate's first build is, and
   whether candidates can share a repository cache.
5. **Settled: refreshing the host's Claude Code login** without a person.
   A plain `claude -p` refreshes an expired token; see Credentials. Whether
   `codex exec` refreshes a Codex token near its expiry is still open.
6. **Cleaning host worktrees after the fetch.** A monorepo worktree is about 10 GB,
   and nothing on the host needs it once the laptop holds the results.
7. **The SSH and systemd paths on a real host.** The tests cover the
   remote command's quoting through a local shell, not `ssh`,
   `systemd-run` or `coder schedule extend` themselves.
8. **Which adapter versions to require.** The submission checks the
   readiness file and Emacs's version, and records the rest without
   comparing them with the laptop's.

## Tests

Implemented so far:

- **`test/agent-shell-arena-remote-test.el`**: round trips and boundary
  validation of every record, profiles crossing as data, the laptop's
  transitions, prerequisite negotiation and bundling against scratch
  clones, LFS refusal, one call per host, timeouts, backoff and
  unreachability with a stubbed process layer, the SSH command's quoting,
  the runner's lock, and re-attaching from records on disk. Login results
  cross the wire and reach the host line, a refusal refuses every member
  before any starts and reaches the status agent by agent, and a runner
  cancelled mid-refresh exits only once the refresh has. The login check
  itself, with fake `claude`, `codex` and `security` executables, is
  covered in `test/agent-shell-sandbox-test.el`.
- **`test/agent-shell-arena-git-test.el`**: a base made in another clone,
  given the lead's sparse patterns, and cleaning a fetched role.
- **`test/agent-shell-pstack-arena-remote-acp-test.el`**, the gate
  "Remote pstack arenas over ACP": a lead with local commits and an
  uncommitted edit, and a host clone at a newer upstream commit, on a
  `(local . DIRECTORY)` host. Fixture candidates run in the macOS sandbox
  under the shipped runner. The scenarios are:
  - the bundle requires only the merge base;
  - the status mirrors into the panel;
  - the results land with the right content;
  - the follow-up equals the same arena's local follow-up, apart from ids,
    paths and the host line;
  - cleanup clears both sides;
  - a cancel while queued, and one mid-run;
  - a killed runner, restarted and recovered;
  - a laptop restart that forgets every panel, then re-attaches and
    delivers.

Still planned:

- **Unit tests** for the `bwrap` argument builder from a sandbox spec.
- **A Linux sandbox test** that runs only on Linux, through `bwrap`, as
  the macOS tests run through `sandbox-exec`.
- **Runner tests on the host.** The fixture arena runs on the remote host
  in batch mode under Emacs 31, with tests for the Linux stop path,
  re-attaching after a laptop restart, a frozen heartbeat, cancel before
  the runner starts, a prerequisite the host cannot fetch, and two arenas
  submitted at once.
- **A live check on the remote host** with two real candidates on the
  monorepo Bazel test target used for the laptop profile.

## Triage of interrogate panel 6799a86a4a5d

| Finding | Severity | Members | Outcome |
|---|---|---|---|
| Hiding socket folders leaves sockets reachable | high | isolation | Fixed. Own network namespace through slirp4netns, sockets only by `:connect`. The probe found Docker, the user D-Bus and 10 abstract sockets. |
| ACP file requests bypass the namespace | high | isolation | Fixed. On Linux the gate allows only paths visible inside the namespace. |
| Other candidates' private folders and host secrets readable | high | isolation | Fixed. Allowlist mounts, private home, cleared environment. The probe found the Coder agent's token, SSH keys and cloud credentials. |
| Concurrent refresh rotation breaks copy-back | high | isolation | Fixed. Sandboxes get no refresh tokens, only the host login's access token. |
| Copy-back promotes candidate data into the real login | medium | isolation | Fixed. Copy-back is deleted. |
| SysV IPC shared | medium | isolation | Fixed. `--unshare-ipc`. |
| Profile keys lose meaning under `bwrap` | medium | isolation | Fixed. Linux meanings defined; regex refused; missing sockets fail loudly. |
| Laptop keeps nothing on disk | high | lifecycle | Fixed. A record per remote arena, and re-attach by session id. |
| No liveness signal; macOS-only stop binaries | high | lifecycle, git | Fixed. Heartbeat, deadlines on both sides, and Linux stop and clone paths. |
| Autostop can kill an arena mid-run | high | lifecycle | Fixed in design. `coder schedule extend` from the runner, with a submit check; open question 1. |
| Config-commit version check too strict and too weak | high | lifecycle, git | Fixed. Ship the runner code per job by content hash; pin and record Emacs and the adapters; version the wire formats. |
| The laptop cannot reuse the local phase machine | high | lifecycle | Fixed. Separate laptop phases and mirrored member states, plus a runner entry point. |
| Bundles omit LFS payloads | high | git | Fixed for v1. Refuse or flag LFS payloads the other side lacks. |
| Bundle prerequisite may be unfetchable | high | git | Fixed. Negotiate the prerequisite, verify the bundle, and fall back to an older ancestor. |
| `cp -c` is macOS-only | high | git | Fixed. `cp --reflink=auto` on Linux. |
| Concurrent arenas read each other's objects | high | git | Fixed for v1. One arena per host repository at a time. |
| Cancel can be lost | medium | lifecycle | Fixed. Marker file, saved and retried; defined as capture without judge. |
| SSH polling can hang or overlap | medium | lifecycle | Fixed. Async, one call in flight, ControlMaster, timeouts, 30 s polls. |
| Results handoff not atomic | medium | lifecycle | Fixed. Results before status, both through rename. |
| No recovery after a stop or crash | medium | lifecycle | Fixed. The runner captures leftovers of an unfinished job. |
| Remote arenas leak | medium | lifecycle, git | Fixed. Records on both sides; a host-side clean. |
| Submission races boot provisioning | medium | lifecycle, git | Fixed. Readiness file after a smoke check. |
| Sparse settings and host profile state lost | medium | git | Fixed. The job carries sparse patterns and the resolved profile. |
| Batch Emacs prompts | low | lifecycle | Fixed. The runner never prompts; launched through `systemd-run --user`. |
| Laptop base has no ref | low | lifecycle, git | Fixed. A local base ref, removed by cleanup. |
| Test plan misses the risky parts | low | lifecycle | Fixed. Runner tests on the host under Emacs 31. |
