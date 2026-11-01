# pstack timeouts and deadlines

What ends a pstack panel, arena or delegate when it runs too long, here
and on a remote host, and which component owns each clock. Values are the
defaults; the names are the options to read for the current ones.

## The clocks

| Clock | Value | Set by | Enforced by |
|---|---|---|---|
| Phase timeout | request `timeout`, 900 s, 1 to 3600 | `+agent-shell-pstack-panel--arm` | `+agent-shell-pstack-panel--expire` |
| Judge timeout | request `judge_timeout`, 900 s | `--judge` arms it | `--expire` |
| Delegate timeout | request `timeout`, 2700 s, covers preparation | `--fix-deadline` | `--arm` for what remains |
| Phase extension | 60 s at a time while a member runs a command | `--expire` re-arms | up to the phase limit |
| Phase limit | `max-runtime`, 7200 s, from the phase's start, never past the panel limit | `--arm` | `--expire` |
| Deadline | arena: `arena-budget`, preparation 1800 s + timeout + judge timeout + overhead 300 s; delegate: its timeout | `--fix-deadline`, once, when provisioning starts or a runner takes the lock | retries, admission; on a host the runner |
| Panel limit | deadline + `max-runtime` − timeout, or the deadline when the timeout is longer | `--fix-deadline` | `--arm`, the runner's extension |
| Horizon | panel limit + `arena-grace`, 120 s | `--horizon` | every sandbox's token check |
| Login need | horizon + `sandbox-login-margin`, 600 s | `+agent-shell-sandbox-check-logins` | admission refuses a shorter login |
| Login refresh | `sandbox-refresh-timeout`, 180 s; a waiter waits twice that | `+agent-shell-sandbox` | the refresh process |
| Provider retry | `retry-delay`, 60 s, once, only if it starts before the phase expires and the deadline | `--fail` | `--retry` |
| Egress records | 30 s after a measured arena ends | `--close-egress` | the follow-up waits no longer |
| Runner tick and heartbeat | 2 s and 10 s | `+agent-shell-arena-remote-runner-batch` | the laptop's stale check |
| Runner capture grace | deadline + 120 s | `+agent-shell-arena-remote--runner-tick` | the runner reports regardless |
| Laptop poll | 30 s, 5 s for a shown transcript, failures back off to 600 s | `+agent-shell-arena-remote--schedule` | |
| Stale runner | no heartbeat for 120 s and not alive | the poll | restart, at most twice |
| Unreachable host | no heartbeat for 900 s | the poll | the lead hears of it |
| Host wake | 600 s, checked every 10 s | `+agent-shell-arena-remote--await` | submission is rejected |
| Workspace stop time | status deadline + `arena-remote-slack`, 300 s | `+agent-shell-arena-remote--keep-awake` | Coder stops the workspace |

A bare `--` name is `+agent-shell-pstack-panel--` followed by it.

The invariant: no member runs past its panel's limit, so a login admitted
against the horizon outlasts every member and its capture. The longest
arena, at the largest timeouts, needs a login that lasts about 3.8 hours;
Claude Code's last about 8 and Codex's 240, so admission at worst
refreshes one. `+agent-shell-pstack-panel-test-login-horizon` pins this.

## What ends a run

| Scenario | Here | On a host |
|---|---|---|
| A panel member does not report in time | its phase timeout settles it as `timeout` with its partial text | same, in the runner's panel |
| A member runs a command at the timeout | the phase is extended a minute at a time, up to the phase limit | the runner also moves the deadline a minute at a time, up to the panel limit |
| The candidates finish late | the judge still gets its own timeout | the runner cancels at the deadline: candidates are captured, the judge never starts |
| Preparation and admission run long | admission refuses once the deadline has passed | same; the runner's deadline starts when it takes the lock, so a queued job loses no time |
| A login falls short of the need | refreshed once under the host's lock, then refused | same, with the host's logins |
| A login is replaced after admission | that member's sandbox refuses to start | same |
| A transient provider failure | one retry after 60 s, if it fits | same |
| Capture hangs | | the runner finishes 120 s past the deadline with an error |
| The runner dies | | restarted after 120 s without a heartbeat, twice; a third death leaves it to the unreachable check |
| The host goes silent | | after 900 s the lead hears it is unreachable, with a command to re-attach |
| The host is asleep or booting | | submission waits up to 600 s for it, then rejects |
| A delegate runs out of time | its one phase times out; preparation counted against it | same, on the host |

A local arena has no clock of its own at the deadline: its phase timers
and their shared limit end it. A host's runner adds one, so a remote
arena whose candidates run long skips its judge where a local one would
still judge.
