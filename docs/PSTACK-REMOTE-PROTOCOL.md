# pstack remote host protocol

This document is normative: it says what a laptop and a host exchange to
run one pstack job, an arena or a delegate, on a remote host. It is for
anyone implementing or reviewing a host, its setup, or a host kind.
[AGENT-SHELL-PSTACK-ARENA-REMOTE.md](AGENT-SHELL-PSTACK-ARENA-REMOTE.md) is
the design history: why it is shaped this way and what was measured.
Where they disagree, this document and the code win.

The implementation is `lisp/+agent-shell-arena-remote.el`. The record
schemas below are generated from `+agent-shell-arena-remote--kinds` and
the capability table from `+agent-shell-arena-remote-capability-registry`
by `scripts/protocol-schemas.el`; the gate fails when they drift. Never
edit text between `GENERATED` markers by hand. After changing a record
kind or a capability, run:

```sh
emacs -Q --batch -l scripts/protocol-schemas.el -f +protocol-schemas-batch
```

"Must", "must not" and "may" carry their usual meaning.

## Actors

- **The laptop** is the user's Emacs. It builds the job, ships it, polls,
  fetches the results and cleans up. It owns every decision to start,
  restart, cancel or give up.
- **The runner** is a batch Emacs on the host, started from the code the
  laptop shipped. It runs the job's panel to its end and writes the
  status and the results. It never prompts and never talks to the
  laptop: the laptop only reads its files.
- **The host kind** is the laptop's transport backend for one family of
  hosts, such as `coder`, `ssh` or `local`. It turns a command into a
  local process that runs it in the host's state folder, and may wake
  the host or extend its lifetime. See [Host kinds](#host-kinds).
- **The host's setup** is outside this repository. It installs the
  host's tools, proves its capabilities, and writes `ready.json`. See
  [The host contract](#the-host-contract).

## Calls and files

Every laptop-to-host interaction is one call: `sh -c SCRIPT sh ARG...`,
where `SCRIPT` is a constant in the library, run in the host's state
folder through the host kind. Calls to one host run one at a time, so
polls never overlap and a submission never races a cancel. A call
answers with its standard output; a nonzero exit or a timeout is a
transport error. Scripts answer with a prefixed last line, such as
`pstack-arena-submit started`, so stray output before it is harmless.

The state folder is `~/.local/state/pstack-arena-remote/` on an SSH or
Coder host and the named folder on a local one. It holds:

| Path | Written by | Holds |
|---|---|---|
| `runner-init.el` | host setup | Elisp the runner loads first; it configures the agents |
| `code/HASH/` | submit | the runner's code for one content hash |
| `jobs/ID/` | submit, runner | one job's files, below |
| `arenas/` | runner | the jobs' checkouts and worktrees |
| `locks/` | runner | one lock per host checkout |

Sandboxed members must not be able to read `jobs/`, `locks/` or
`runner-init.el`.

## One job

Each step names who writes what, its direction, and its format. `ID` is
the job's id, `REPO` the profile's `:host-directory`.

1. **Wake.** Laptop, through the host kind's `:wake`. A Coder host is
   started if stopped, then waited on until its agent connects. Kinds
   without `:wake` go straight to the probe.
2. **Probe.** Laptop to host, the probe script with `REPO`, the code
   hash and the readiness file's path. The host answers a line
   `pstack-arena-probe 1`, then `toplevel`, `tip` (`origin/HEAD`, else
   `HEAD`), `code` (`1` when `code/HASH/` exists), `ready` (the file's
   absolute name) and `boot` (`btime` from `/proc/stat`), one per line,
   then the readiness file verbatim.
3. **Readiness.** The laptop reads `ready.json`, a [`ready`](#ready)
   record written by the host's setup. It refuses the host when the file
   is missing or malformed, was written before the host booted, names an
   Emacs older than 30, or `REPO` is not a git checkout. Within the wake
   timeout, 600 s, it probes again instead of refusing. It then checks the
   job's [capabilities](#capabilities): a job whose needs the file does
   not prove is refused at once, naming each missing capability and how
   to set it up, before anything below is shipped.
4. **Prerequisite.** The laptop picks the commit the base bundle is cut
   against, in order: its merge base with the host's `tip` when the
   laptop has that commit; the merge base with its own upstream, which
   the host fetches from `origin` if it lacks it; then none, a bundle of
   the whole history.
5. **Base and code.** The laptop builds `base.bundle`, holding
   `refs/pstack-arena/ID/base` over the prerequisite, and the runner's
   code: its libraries and the agent-shell, acp and shell-maker sources,
   named by the SHA-256 of their names and contents. A base that changes
   Git LFS files after the prerequisite is refused.
6. **Submit and launch.** Laptop to host, the submit script with `ID`,
   `REPO`, the Emacs path from `ready.json` and the launcher, and an
   uncompressed tar on standard input holding `job.json` (a
   [`job`](#job) record), `base.bundle`, `prerequisite` (a commit id or
   empty), `code-hash`, `code.tar` unless the probe said the host has
   the code, and `cancel` when a cancel came first. The host unpacks it
   into `jobs/ID/`, installs the code under `code/HASH/` atomically,
   runs `git bundle verify`, fetching the prerequisite from `origin` if
   that fails, fetches the base into `REPO`'s `refs/pstack-arena/ID/base`,
   and starts the runner. It answers `started`, `code` (send the code
   tarball), `missing COMMIT` (try the next prerequisite) or
   `failed REASON`. A job already started answers `started`, so a resent
   submission is harmless. The runner is
   `EMACS -Q --batch -L code/HASH -l code/HASH/+agent-shell-arena-remote.el
   -f +agent-shell-arena-remote-runner-batch jobs/ID`, started by
   `systemd-run --user` on a remote host, in a transient service with
   `MemoryMax=50%` and `OOMPolicy=continue`, and by `nohup` on a local one.
7. **Run.** The runner writes `jobs/ID/pid`, waits in phase `queued`
   until it holds `locks/` for `REPO`, then runs the panel. It rewrites
   `status.json`, a [`status`](#status) record, through a temporary file
   and a rename on every change and at least every 10 s. Its `heartbeat`
   is the write time in epoch seconds. Under systemd each delegate's
   adapter runs in a scope, `pstack-delegate-ID-ROLE`, recorded in
   `jobs/ID/leases/ROLE.json`; a delegate is never run without its scope.
8. **Poll.** Laptop to host, every 30 s with backoff to 10 minutes, the
   poll script with `ID`, `cancel` or `-`, a cancel reason, a transcript
   cursor and the Emacs path. With cursor `-`, an arena's, the host
   answers `pstack-arena-poll alive=0|1 cancel=0|1` then `status.json`
   verbatim, or `pstack-arena-poll none` for an unknown job. A delegate
   passes `start` or `EPOCH:SEGMENT:OFFSET:SEQ`; the host then runs
   `+agent-shell-arena-remote-poll-batch` from the job's code, which
   answers `pstack-arena-poll 2` and one JSON envelope: `version` 2,
   `alive`, `cancel`, `status` (the record or null) and `tail`, a
   transcript page or null. When that batch fails the plain answer
   follows.
9. **Transcript pages.** A delegate's runner appends records to
   `jobs/ID/transcript/NNNNNNNN.jsonl`, one JSON object per line with
   `seq` (whole number), `time` (number), `kind` (`assistant`, `tool` or
   `notice`) and `text`, never permission payloads, credentials,
   environment, tool arguments or hidden reasoning. A record is at most
   16 KiB, a segment at most 1 MiB, never split mid-line, and the newest
   8 segments stay. `transcript/epoch` names the transcript; a restarted
   runner keeps it and numbers on. A page, the envelope's `tail`, holds
   `epoch`, `gap` (records were lost before these), `from` and `next`
   (each `segment` and `offset`), `records` (at most 64 KiB of whole
   records after the cursor) and `more`, or only `error`. The laptop
   advances its cursor only once the page is in its cache, so a replayed
   page adds nothing.
10. **Cancel.** The laptop asks by passing `cancel` and a reason to the
    next poll, which writes `jobs/ID/cancel` holding the reason before
    anything else. It retries until a poll reports `cancel=1`. The
    runner checks the marker at start and every 2 s: a queued job ends
    unstarted, a running one captures what its members made and never
    starts the judge.
11. **Heartbeat and restart.** When the runner is not alive and the
    heartbeat has not changed for 120 s, the laptop restarts it with the
    resume script, at most twice. A restarted runner never runs a member
    again: it reaps what a delegate's lease owns, captures what members
    left as timed out, and reports. When it cannot prove a delegate's
    writers ended, it quarantines the repository lock, which is then
    never taken over as stale. After 900 s without a new heartbeat the
    laptop tells the lead the job is unreachable and stops polling until
    a re-attach.
12. **Deadline.** The runner's deadline is the job's `deadline` seconds
    from when it took the lock. While an unsettled member of the current
    phase still runs a command when it passes, the runner moves it on a
    minute at a time, never more than
    `+agent-shell-pstack-panel-max-runtime` less the job's `timeout`
    beyond where it started, and reports each new one in the status.
    Past it, it settles running members as timed out, and two minutes
    later reports regardless. The laptop
    extends a Coder workspace's stop time past the deadline before the
    upload and whenever a status reports a later one.
13. **Report.** Runner, host side. When the panel finishes it writes
    `results.bundle` (an arena's result refs over the base; a delegate's
    `refs/heads/pstack/delegate/PANEL` over the base's parent), then
    `report.json`, a [`report`](#report) record, then, for a measured
    arena or a delegate, `results.files`, and only then the final status
    with phase `finished` and `results` true.
14. **Fetch.** Laptop to host, the fetch script with `ID`, which writes a
    tar on standard output: the files `results.files` lists, one path
    per line relative to `jobs/ID/` (the report, the bundle, each
    member's `reports/PANEL/ROLE.egress.json` and a delegate's transcript
    segments), or else `report.json` and `results.bundle` when present.
    The laptop lands each result in its own `refs/pstack-arena/ID/ROLE`,
    checks it against the report, and saves egress records, treating a
    missing or invalid one as unavailable. A failed fetch is retried.
15. **Cleanup.** Laptop to host, the clean script with `ID` and the
    Emacs path. It waits up to 20 s for a finished runner to exit, then
    answers `running` and does nothing if it still runs. Otherwise it
    runs `+agent-shell-arena-remote-runner-clean-batch`, which removes the
    job's arena through its manifest and `refs/pstack-arena/ID/base`,
    deletes `jobs/ID/` and answers `done`. The laptop then cleans its own
    side. `code/` is never cleaned by a job.

The laptop's own record of each job, `remote.json` in its arena folder,
is a [`record`](#record) record. It never crosses the wire, but it is
versioned like one, since a restarted laptop re-attaches from it.

## Versioning

- Each file of its own carries `version`, and the laptop's files and the
  runner's also `kind`. `ready.json` carries only `version`. The
  versions each reads are in the generated schemas.
- A reader accepts every version from 1 up to its own and refuses a
  newer one, saying to update that Emacs. A laptop that updated mid-job
  therefore still reads an older runner's status and report.
- A reader ignores fields it does not know. Adding an optional field
  needs no new version; a change an older reader would misread does.
- A reader checks every field's type at the boundary and refuses the
  record naming the field, such as `status.members[0].agent`. A
  required field that is absent or null is refused.
- A job of version 1 lists `candidates` and a `judge`; the reader turns
  them into an arena's `members`. Version 2 writers write only
  `members`. A job whose members or fields do not fit its `shape` is
  refused.
- A capability a readiness file does not declare is unproved. A file
  from a setup older than a capability is refused for jobs that need
  it, saying to rerun setup; it is never read as having it.

## The host contract

A host must provide:

- Emacs 30 or newer, recorded as `ready.json`'s `emacs` tool.
- A checkout of the repository at each profile's `:host-directory`,
  with an `origin` remote the laptop's upstream commits can be fetched
  from, since the base bundle is cut against them.
- The agents' own logins on the host, set up once by the user. The
  runner checks each agent's login before any member starts, refreshes
  a short one once, and refuses the job if it still falls short. No
  credential travels with a job.
- `runner-init.el` in the state folder, configuring the agents the
  laptop's jobs name.
- The capabilities each job needs, proved by setup and declared in
  `ready.json`.

Setup must delete `ready.json` on every boot, then write it, through a
temporary file and a rename, only once its checks have run. Its shape:

```json
{"version": 1,
 "written_at": "2026-10-30T09:00:00.000Z",
 "emacs": {"path": "/usr/bin/emacs", "version": "31.1"},
 "adapters_bin": "/home/user/.local/share/pstack-arena/adapters/node_modules/.bin",
 "node": {"path": "/usr/bin/node", "version": "v24.22.1"},
 "claude_agent_acp": {"path": "/home/user/.../dist/index.js", "version": "0.95.0"},
 "codex_acp": {"path": "/home/user/.../dist/index.js", "version": "2.2.2"},
 "bwrap": {"path": "/usr/bin/bwrap", "version": "bubblewrap 0.9.0"},
 "unshare": {"path": "/usr/bin/unshare", "version": "unshare 2.39.3"},
 "slirp4netns": {"path": "/usr/bin/slirp4netns", "version": "slirp4netns version 1.2.1"},
 "capabilities": {"linux_namespaces": 1, "bwrap": 1, "slirp4netns": 1,
                  "systemd_user": 1, "git": 1, "node_adapters": 1,
                  "delegate": 1, "egress_tshark": 0}}
```

### Capabilities

Each capability's value is the version of the check that proved it,
1 for every check today, or 0 when setup ran the check and it failed. A
capability absent from the file was never checked. A value of 1 or more
proves it.

<!-- BEGIN GENERATED capabilities -->
| Capability | Proved when | To set it up |
|---|---|---|
| `linux_namespaces` | unshare --user --map-root-user --net true succeeds as the runner's user | allow unprivileged user namespaces (kernel.unprivileged_userns_clone=1, and on Ubuntu 24.04 or newer an AppArmor profile for bwrap or kernel.apparmor_restrict_unprivileged_userns=0) |
| `bwrap` | bwrap --unshare-user --unshare-pid --unshare-ipc --unshare-net --die-with-parent --ro-bind / / --dev /dev --proc /proc true succeeds | install bubblewrap and record it as ready.json's bwrap tool |
| `slirp4netns` | a command in a bwrap network namespace reaches the network through slirp4netns --configure | install slirp4netns and record it as ready.json's slirp4netns tool |
| `systemd_user` | systemd-run --user --collect --wait true and systemd-run --user --scope --collect true both succeed from a non-interactive ssh session | run a systemd user manager that outlives logins (loginctl enable-linger) and reaches ssh sessions through XDG_RUNTIME_DIR |
| `git` | git runs bundle verify, fetch from a bundle, worktree add and sparse-checkout in a scratch repository | install git |
| `node_adapters` | node, and claude-agent-acp and codex-acp in adapters_bin, each answer --version | install Node and the ACP adapters from their lock into adapters_bin, and record them as ready.json's node, claude_agent_acp and codex_acp tools |
| `delegate` | setup's delegate smoke check passes: a fixture delegate commits on a branch of a scratch checkout and its scope stops | rerun the host's setup with delegate support, or keep delegates here with :delegate (:host local) |
| `egress_tshark` | tshark captures a sandbox's traffic inside its user namespace | install tshark |

<!-- END GENERATED capabilities -->

At submission a job needs:

- `git` and `node_adapters`, always.
- `systemd_user` when the host kind launches with systemd, every kind
  without `:local`; it holds the runner's service and each delegate's
  scope.
- `delegate` for a delegate job.
- `linux_namespaces`, `bwrap` and `slirp4netns` when a member runs
  sandboxed on the Linux backend: every arena member, and a delegate
  whose policy sandboxes it, on any host but a local one on macOS.

`egress_tshark` is never needed. A measured job on a Linux host without
it runs, and its members' egress records are unavailable.

A local host is this machine: its runner starts with `nohup` and its
members use this Emacs's sandbox, so on macOS it needs only `git`,
`node_adapters` and, for a delegate, `delegate`. Its `ready.json`, in
its state folder, must still declare them.

The runner checks again before a delegate starts: `delegate` in the
readiness file it reads, a sandbox backend for a sandboxed delegate, and
`systemd-run` under systemd.

## Host kinds

A profile's `:host` is `(KIND . NAME)`, where `KIND` is a key of
`+agent-shell-arena-remote-host-kinds`. Profile validation reads that
registry without loading the remote library. Each entry is
`(KIND . PLIST)`:

| Property | Value |
|---|---|
| `:exec` | Function of `NAME` and an argument list `("sh" "-c" SCRIPT "sh" ARG...)`, returning the local command that runs it in the host's state folder, with standard input and output passed through. |
| `:label` | Function of `NAME` returning the host's name in messages. |
| `:wake` | nil, or a function of `NAME` and `CALLBACK`, which it calls with `(EVENT DETAIL NEXT)`; see below. |
| `:extend` | nil, or a function of `NAME`, seconds and `CALLBACK`, which it calls with nil or an error message once the host will live that long. |
| `:local` | Non-nil makes `NAME` an absolute local state folder: calls run there as local processes, `ready.json` is read from it, and the runner starts with `nohup`. Otherwise the runner starts with `systemd-run --user` and `ready.json` is `+agent-shell-arena-remote-ready-file`. |

Wake events: `ready` probes now; `wait` records `DETAIL` and calls `NEXT`
after 10 s; `retry` calls `NEXT` if already waiting, else probes;
`starting` notes the host was stopped and calls `NEXT` at once; `check`
reruns the submission's phase and cancel checks; `error` refuses the job
with `DETAIL`. The submission owns the timer, the wake timeout and
cancellation. A record's host kind is validated against the registry
when read, so a kind must stay registered while records name it.

### Adding a backend

A host kind supplies transport only. Three things are fixed today and a
new backend cannot change them through the registry:

1. **Launching the runner.** `+agent-shell-arena-remote--launcher` picks
   `nohup` for a `:local` kind and `systemd-run --user` for every other,
   which also holds the runner's memory limit.
2. **Containing a delegate.** A remote delegate's adapter runs in its own
   `systemd-run --user --scope` unit, and stopping it means stopping that
   unit. There is no other way to contain one.
3. **The sandbox.** Every non-`:local` kind is assumed to be Linux with the
   bwrap sandbox.

A backend without a systemd user manager is therefore refused before
anything is shipped, with `systemd_user` named as missing, rather than
half run. To support one, add `:launch` and `:contain` properties to the
host kind, each a function with systemd as the default, give the
capability check a property saying which capabilities the kind's own
launch and containment need, and extend this section and the capability
table to match.

## Record schemas

`job.json` and `report.json` are in each job's folder, `status.json` too,
`ready.json` where the host's setup writes it, and `remote.json` on the
laptop. The other kinds are nested in them. An optional field may be
absent or `null`; a boolean that is absent is false; a list that is
absent is empty.

<!-- BEGIN GENERATED types -->
| Type | Meaning |
|---|---|
| string | Any JSON string. |
| id | A string matching the Emacs regexp `` \`[[:alnum:]][[:alnum:]_-]*\' ``, safe in a path and a ref. |
| sha | A full commit id: 40 or 64 lowercase hex digits. |
| agent | An agent identifier matching `` \`[a-z0-9][a-z0-9-]*\' ``. |
| host kind | A kind registered in `+agent-shell-arena-remote-host-kinds`, by default one of `coder`, `ssh` and `local`. |
| failure kind | A provider failure kind, one of `rate_limited`, `quota_exhausted`, `overloaded`, `transport_lost`, `auth_required`, `context_exhausted`, `budget_exhausted`, `policy_denied`, `internal_error`, `provider_error`; a reader takes an unknown one as `provider_error`. |
| whole number | A JSON integer of 0 or more. |
| number | Any JSON number. |
| boolean | `true` or `false`. |
| profile | A repository profile as plain data, resolved on the laptop; its keys are the profile's own, such as `shared` and `env`. |

<!-- END GENERATED types -->

<!-- BEGIN GENERATED record job -->
#### `job`

A file of its own: `"version"` from 1 to 2 and `"kind": "job"`.

| Key | Type | Presence |
|---|---|---|
| `id` | id | required |
| `panel` | id | required |
| `shape` | one of `"arena"`, `"delegate"` | required |
| `brief` | string | required |
| `rubric` | list of strings | absent is empty |
| `timeout` | whole number | required |
| `judge_timeout` | whole number | optional, absent or null |
| `members` | list of [`request-member`](#request-member) | absent is empty |
| `delegate` | [`delegate-job`](#delegate-job) | optional, absent or null |
| `prefix` | string | required |
| `base` | sha | required |
| `prerequisite` | sha | optional, absent or null |
| `sparse` | [`sparse`](#sparse) | optional, absent or null |
| `profile` | profile | optional, absent or null |
| `repository` | string | required |
| `code` | sha | required |
| `deadline` | whole number | required |
| `ready_file` | string | optional, absent or null |
| `ready` | [`ready`](#ready) | optional, absent or null |
| `measure_egress` | boolean | optional, absent or null |

<!-- END GENERATED record job -->

<!-- BEGIN GENERATED record request-member -->
#### `request-member`

| Key | Type | Presence |
|---|---|---|
| `role` | id | required |
| `agent` | agent | required |
| `kind` | one of `"candidate"`, `"judge"`, `"delegate"` | required |
| `focus` | string | optional, absent or null |

<!-- END GENERATED record request-member -->

<!-- BEGIN GENERATED record delegate-job -->
#### `delegate-job`

| Key | Type | Presence |
|---|---|---|
| `sandbox` | boolean | absent is false |
| `identity` | [`identity`](#identity) | required |

<!-- END GENERATED record delegate-job -->

<!-- BEGIN GENERATED record identity -->
#### `identity`

| Key | Type | Presence |
|---|---|---|
| `name` | string | required |
| `email` | string | required |

<!-- END GENERATED record identity -->

<!-- BEGIN GENERATED record sparse -->
#### `sparse`

| Key | Type | Presence |
|---|---|---|
| `cone` | boolean | absent is false |
| `patterns` | list of strings | absent is empty |

<!-- END GENERATED record sparse -->

<!-- BEGIN GENERATED record status -->
#### `status`

A file of its own: `"version"` from 1 to 2 and `"kind": "status"`.

| Key | Type | Presence |
|---|---|---|
| `id` | id | required |
| `phase` | one of `"queued"`, `"preparing"`, `"candidates"`, `"capturing"`, `"judging"`, `"delegating"`, `"finalizing"`, `"finished"` | required |
| `pid` | whole number | optional, absent or null |
| `heartbeat` | number | required |
| `deadline` | number | optional, absent or null |
| `arena` | id | optional, absent or null |
| `members` | list of [`member-state`](#member-state) | absent is empty |
| `results` | boolean | absent is false |
| `error` | string | optional, absent or null |
| `logins` | list of [`login`](#login) | absent is empty |
| `shape` | one of `"arena"`, `"delegate"` | optional, absent or null |
| `transcript` | [`transcript`](#transcript) | optional, absent or null |

<!-- END GENERATED record status -->

<!-- BEGIN GENERATED record transcript -->
#### `transcript`

| Key | Type | Presence |
|---|---|---|
| `epoch` | id | required |
| `first` | whole number | required |
| `last` | whole number | required |
| `seq` | whole number | required |

<!-- END GENERATED record transcript -->

<!-- BEGIN GENERATED record cursor -->
#### `cursor`

| Key | Type | Presence |
|---|---|---|
| `epoch` | id | required |
| `segment` | whole number | required |
| `offset` | whole number | required |
| `seq` | whole number | required |

<!-- END GENERATED record cursor -->

<!-- BEGIN GENERATED record login -->
#### `login`

| Key | Type | Presence |
|---|---|---|
| `agent` | agent | required |
| `refreshed` | boolean | absent is false |
| `remaining` | number | optional, absent or null |
| `error` | string | optional, absent or null |

<!-- END GENERATED record login -->

<!-- BEGIN GENERATED record member-state -->
#### `member-state`

| Key | Type | Presence |
|---|---|---|
| `role` | id | required |
| `agent` | agent | required |
| `kind` | one of `"candidate"`, `"judge"`, `"delegate"` | required |
| `state` | one of `"pending"`, `"starting"`, `"waiting"`, `"running"`, `"done"`, `"error"`, `"timeout"` | required |
| `capture` | one of `"stopping"`, `"stopped"`, `"capturing"`, `"captured"`, `"failed"` | optional, absent or null |
| `error` | string | optional, absent or null |
| `failure_kind` | failure kind | optional, absent or null |

<!-- END GENERATED record member-state -->

<!-- BEGIN GENERATED record report -->
#### `report`

A file of its own: `"version"` from 1 to 2 and `"kind": "report"`.

| Key | Type | Presence |
|---|---|---|
| `id` | id | required |
| `arena` | id | optional, absent or null |
| `base` | sha | required |
| `rejection` | string | optional, absent or null |
| `members` | list of [`member-report`](#member-report) | absent is empty |
| `ready` | [`ready`](#ready) | optional, absent or null |
| `shape` | one of `"arena"`, `"delegate"` | optional, absent or null |
| `delegate` | [`outcome`](#outcome) | optional, absent or null |

<!-- END GENERATED record report -->

<!-- BEGIN GENERATED record outcome -->
#### `outcome`

| Key | Type | Presence |
|---|---|---|
| `branch` | string | required |
| `tip` | sha | optional, absent or null |
| `commits` | list of [`commit`](#commit) | absent is empty |
| `workspace` | string | required |
| `sandbox` | boolean | absent is false |
| `dirty_paths` | list of strings | absent is empty |
| `dirty_count` | whole number | required |
| `finalization` | one of `"none"`, `"empty"`, `"committed"`, `"blocked"` | required |
| `message_error` | string | optional, absent or null |
| `retained_reason` | string | optional, absent or null |

<!-- END GENERATED record outcome -->

<!-- BEGIN GENERATED record commit -->
#### `commit`

| Key | Type | Presence |
|---|---|---|
| `oid` | sha | required |
| `subject` | string | required |

<!-- END GENERATED record commit -->

<!-- BEGIN GENERATED record member-report -->
#### `member-report`

| Key | Type | Presence |
|---|---|---|
| `role` | id | required |
| `agent` | agent | required |
| `kind` | one of `"candidate"`, `"judge"`, `"delegate"` | required |
| `state` | one of `"pending"`, `"starting"`, `"waiting"`, `"running"`, `"done"`, `"error"`, `"timeout"` | required |
| `capture` | one of `"stopping"`, `"stopped"`, `"capturing"`, `"captured"`, `"failed"` | optional, absent or null |
| `text` | string | optional, absent or null |
| `error` | string | optional, absent or null |
| `commit` | sha | optional, absent or null |
| `stat` | string | optional, absent or null |
| `failure` | string | optional, absent or null |
| `failure_kind` | failure kind | optional, absent or null |

<!-- END GENERATED record member-report -->

<!-- BEGIN GENERATED record tool -->
#### `tool`

| Key | Type | Presence |
|---|---|---|
| `path` | string | required |
| `version` | string | required |

<!-- END GENERATED record tool -->

<!-- BEGIN GENERATED record ready -->
#### `ready`

A file of its own: `"version"` from 1 to 1, and no `"kind"`.

| Key | Type | Presence |
|---|---|---|
| `written_at` | string | optional, absent or null |
| `emacs` | [`tool`](#tool) | required |
| `adapters_bin` | string | optional, absent or null |
| `claude_agent_acp` | [`tool`](#tool) | optional, absent or null |
| `codex_acp` | [`tool`](#tool) | optional, absent or null |
| `node` | [`tool`](#tool) | optional, absent or null |
| `bwrap` | [`tool`](#tool) | optional, absent or null |
| `unshare` | [`tool`](#tool) | optional, absent or null |
| `slirp4netns` | [`tool`](#tool) | optional, absent or null |
| `capabilities` | [`capabilities`](#capabilities) | optional, absent or null |

<!-- END GENERATED record ready -->

<!-- BEGIN GENERATED record capabilities -->
#### `capabilities`

| Key | Type | Presence |
|---|---|---|
| `linux_namespaces` | whole number | optional, absent or null |
| `bwrap` | whole number | optional, absent or null |
| `slirp4netns` | whole number | optional, absent or null |
| `systemd_user` | whole number | optional, absent or null |
| `git` | whole number | optional, absent or null |
| `node_adapters` | whole number | optional, absent or null |
| `delegate` | whole number | optional, absent or null |
| `egress_tshark` | whole number | optional, absent or null |

<!-- END GENERATED record capabilities -->

<!-- BEGIN GENERATED record host -->
#### `host`

| Key | Type | Presence |
|---|---|---|
| `kind` | host kind | required |
| `name` | string | required |

<!-- END GENERATED record host -->

<!-- BEGIN GENERATED record record -->
#### `record`

A file of its own: `"version"` from 1 to 2 and `"kind": "record"`.

| Key | Type | Presence |
|---|---|---|
| `id` | id | required |
| `panel` | id | required |
| `session` | string | optional, absent or null |
| `host` | [`host`](#host) | required |
| `phase` | one of `"preparing"`, `"submitting"`, `"remote"`, `"fetching"`, `"finished"`, `"unreachable"` | required |
| `cancel` | one of `"none"`, `"pending"`, `"sent"` | required |
| `submitted` | boolean | absent is false |
| `fetched` | boolean | absent is false |
| `delivered` | boolean | absent is false |
| `deadline` | number | optional, absent or null |
| `extended` | number | optional, absent or null |
| `heartbeat` | number | optional, absent or null |
| `seen` | number | optional, absent or null |
| `restarts` | whole number | required |
| `arena` | id | optional, absent or null |
| `emacs` | string | optional, absent or null |
| `error` | string | optional, absent or null |
| `shape` | one of `"arena"`, `"delegate"` | optional, absent or null |
| `cursor` | [`cursor`](#cursor) | optional, absent or null |

<!-- END GENERATED record record -->
