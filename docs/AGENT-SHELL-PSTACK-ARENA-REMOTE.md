# pstack arena panels on a remote host: design

Status: proposed, revision 2. Revision 1 was attacked by interrogate
panel `6799a86a4a5d` (`var/pstack-panels/6799a86a4a5d/`). Its findings
and the probes they prompted changed the sandbox, credentials, versioning
and lifecycle sections below. The triage table at the end maps each
finding to what changed. It extends `docs/AGENT-SHELL-PSTACK-ARENA.md`, where
candidates run on the laptop under `sandbox-exec`. Here an arena runs on
a remote Linux host, a a large monorepo remote host in the first use. Emacs on the laptop
only sends the job and receives the outcomes.

## Why

On the laptop, a a large monorepo monorepo candidate costs 2 to 3 minutes and 11 GB
for its worktree, plus Bazel's load on the machine. A remote host is built for
that work and can keep running while the laptop sleeps. Other repositories
keep running locally.

## Ground truth

Measured on the remote host `example-ws`, created from the default
`default-template` template's "large preset" preset,
on 2026-10-26.

- **Machine.** Ubuntu 22.04.5, kernel 6.8, 16 CPUs, 124 GB RAM. 841 GB
  of the 1 TB home volume is free.
- **Repository.** `~/src/big-repo` is a full checkout of `master`, about an
  hour old when created, with its `.git` at 32 GB. Bazel, bazelisk, Nix
  and tmux are installed.
- **Agents.** Claude Code 2.1.304 and codex-cli 0.157.0 are installed,
  and the user logged both in once. Claude Code keeps its credentials in
  `~/.claude/.credentials.json`, and Codex in `~/.codex/auth.json`. Codex's
  browser login needed the forward `ssh -L 1455:localhost:1455`, because
  its callback server runs on the remote host.
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
  remote host ran for more than 10 minutes and was killed. The laptop lacked
  the remote host's newer refs, so it could not know which objects the remote host
  already had. A bundle of only the commits after the merge base with
  `origin/master` was 4 KB, built in 0.2 s, and moved and fetched in 6.2 s,
  including fetching the merge base from GitHub. The remote host got the exact
  same commit id.
- **Lifetime.** The template stops a workspace 8 hours after it starts,
  extended by 2 hours with activity. It is not yet known whether a running
  process counts as activity. The remote host's own `coder` CLI has
  `coder schedule extend`, and `systemd-run --user --wait true` works there.
- **What the host exposes.** Probed after the panel:
  - Unix sockets: `/run/docker.sock` and containerd's sockets, the user
    D-Bus at `/run/user/1001/bus`, gpg-agent sockets, the Nix daemon socket
    and code-server's, plus 10 abstract sockets, which have no path to hide.
    Docker access is root on the box, and the user D-Bus can start
    unsandboxed commands through `systemd-run --user`.
  - The environment: `CODER_AGENT_TOKEN`, which can control the workspace,
    `SSH_AUTH_SOCK` and `GIT_ASKPASS`.
  - Home: `~/.ssh`, `~/.aws`, `~/.docker/config.json`, `~/.tool-a`, `~/.tool-b`.
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

Credentials live on the host, so nothing is sent per job. Candidates never
get a refresh token, so there is nothing to rotate and nothing to copy
back:

- **Claude Code** candidates get a long-lived `CLAUDE_CODE_OAUTH_TOKEN`
  from `claude setup-token`, run once on the host and kept in a file only
  the runner reads, as on the laptop.
- **Codex** candidates get only the current access token, passed with
  `codex login --with-access-token` into their private `CODEX_HOME`. It
  expires, so the runner checks its lifetime covers the arena's deadline
  before starting, and refreshes the host's own login first if it does
  not. This needs testing.

The copy-back rule is dropped for remote candidates. It could not survive
two candidates refreshing at once, and it promoted candidate-written data
into the host's real login.

### Setting up the host and versions

The user's private `~/.sync` playbook runs on every boot. It installs only
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
the monorepo's private profile sets it. Other repositories leave `:host` unset
and run locally.

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
that announces them. It has an overall deadline: preparation allowance plus
the candidates' timeout plus the judge's timeout plus slack. It extends the
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

## Experiment results, 2026-10-26

Run on `example-ws` after revision 2.

**The network namespace.** `bwrap --unshare-net` with `slirp4netns`
attached failed with `setns(CLONE_NEWNET): Operation not permitted`, even
with `--userns-path`. This order worked:

1. `unshare --user --map-root-user --net` creates the user and network
   namespaces.
2. `slirp4netns --configure --disable-host-loopback <pid> tap0` attaches to
   that process.
3. `bwrap` runs inside for the mounts, the PID and IPC namespaces and the
   cleared environment, with `--unshare-user --uid 1001 --gid 1001` so the
   agent is not root inside.

`/etc/resolv.conf` is a symlink into `/run/systemd/resolve`, so the
sandbox gets a `nameserver 10.0.2.3` file at
`/run/systemd/resolve/stub-resolv.conf` under a private `/run`.

| Check inside the sandbox | Result |
|---|---|
| Interface | `tap0` up at 10.0.2.100 |
| HTTPS to api.anthropic.com and github.com | reached (404 and 200) |
| Host loopback through 10.0.2.2 | refused |
| Abstract unix sockets | none visible |
| `/run/docker.sock` | absent |
| `~/.ssh` | absent |
| Environment | 5 variables |

With `--ro-bind / /` instead of the allowlist, `/run/docker.sock` was
reachable even in its own network namespace, because path sockets are
files. The allowlist is necessary.

**Credentials without refresh tokens.** The host's Claude Code access
token had 3.8 hours left. The host's Codex access token is a JWT that lasts
about 236 hours, and it had been issued 4.1 hours earlier.

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

So candidates get only an access token. The runner checks that the token
outlives the arena's deadline and refreshes the host's own login first if
it does not.

**Autostop.** The remote host's `coder` CLI is not logged in. The laptop's is,
and `coder schedule extend example-ws 6h` moved the stop time from 01:46
to 04:16, six hours from the command. The laptop extends the workspace at
submission to cover the arena's deadline, so the host needs no Coder token.

## Open questions to settle by testing

1. **The ACP adapters** inside the same sandbox, once installed. The
   agents' CLIs work there, so this is expected to hold.
2. **What the monorepo's build needs from the host.** Bazel, its private home
   cache and toolchain paths, and whether it needs the Nix daemon socket or
   Docker. The trace command, ported to Linux, answers this.
3. **Bazel on the host.** How cold a candidate's first build is, and
   whether candidates can share a repository cache.
4. **Refreshing the host's Claude Code login** without a person: whether a
   plain `claude -p` on the host refreshes a token near expiry, or a
   one-year `claude setup-token` should be made once.

## Tests

- **Unit tests** for the `bwrap` argument builder from a sandbox spec, the
  job and status file formats, and the results report parser.
- **A Linux sandbox test** that runs only on Linux, through `bwrap`, as
  the macOS tests run through `sandbox-exec`.
- **An ACP fixture test** that runs a fixture arena through the runner in
  a local scratch "host": a subprocess with its own directories, reached
  without SSH. It checks the base transfer, status polling and result
  fetch.
- **Runner tests on the host.** The fixture arena runs on the remote host
  in batch mode under Emacs 31, with tests for the Linux stop path,
  re-attaching after a laptop restart, a frozen heartbeat, cancel before
  the runner starts, a prerequisite the host cannot fetch, and two arenas
  submitted at once.
- **A live check on `example-ws`** with two real candidates on the
  monorepo Bazel test target used for the laptop profile.

## Triage of interrogate panel 6799a86a4a5d

| Finding | Severity | Members | Outcome |
|---|---|---|---|
| Hiding socket folders leaves sockets reachable | high | isolation | Fixed. Own network namespace through slirp4netns, sockets only by `:connect`. The probe found Docker, the user D-Bus and 10 abstract sockets. |
| ACP file requests bypass the namespace | high | isolation | Fixed. On Linux the gate allows only paths visible inside the namespace. |
| Other candidates' private folders and host secrets readable | high | isolation | Fixed. Allowlist mounts, private home, cleared environment. The probe found `CODER_AGENT_TOKEN`, `~/.ssh` and `~/.aws`. |
| Concurrent refresh rotation breaks copy-back | high | isolation | Fixed. Candidates get no refresh tokens: a setup-token for Claude Code, an access token for Codex. |
| Copy-back promotes candidate data into the real login | medium | isolation | Fixed. Copy-back is dropped for remote candidates. |
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
