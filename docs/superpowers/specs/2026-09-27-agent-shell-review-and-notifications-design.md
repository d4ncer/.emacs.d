# Agent-shell review and notifications

## Purpose

Make agent-shell sessions easy to leave running and easy to review. Emacs should
alert the user when an agent finishes acting or needs input. A separate review
command should assess the active changeset against its original requirements,
ask about unclear success criteria, and present actionable findings in Emacs.

Success means the common path needs no manual diff or spec selection: start a
review from the implementation shell, inspect questions or prioritized findings
in a sidebar, send selected fixes to that shell, and run a fresh review after
the fixes. Both features must be extractable as standalone Emacs packages.

## Package boundaries

- `lisp/agent-shell-review.el` owns changeset capture, requirements discovery,
  review sessions, response parsing, and a dedicated review mode. Public symbols
  use the `agent-shell-review-` prefix. It depends on agent-shell and built-in
  Emacs libraries, but not on personal `+` helpers or agent-review.
- `lisp/agent-shell-notify.el` owns agent-shell event subscriptions and macOS
  alerts. Public symbols use the `agent-shell-notify-` prefix. It can be enabled
  without the review package.
- `modules/mod-ai.el` loads and configures both packages, adds discoverable
  bindings, and connects their optional notification hooks. Neither package
  requires the other. No new `init.el` module is needed.

The [agent-review package](https://github.com/nineluj/agent-review) informs the
interaction design: asynchronous review, navigable findings, marking, batch
fix requests, and rerun. Its code and fixed response parser are not imported.

## Review entry and changeset

`agent-shell-review` is the DWIM command. From an agent-shell buffer it always
uses that buffer as the implementation shell, even when other shells share the
project. From a source buffer it identifies the Git project and uses the sole
matching shell, or prompts when several match. From its own review buffer it
reuses the recorded project and implementation shell. If no live implementation
shell exists, review proceeds when it finds a spec or the user selects one. A
later fix request starts a new implementation shell with the requirements and
selected findings.

The command captures a review snapshot against an integration branch. A
feature branch's same-named tracking upstream is never treated as its base.
A configurable base ref takes precedence. On a local `main` or `master` branch,
its same-named tracking remote is the base for unpushed commits. Otherwise the
command uses the remote default branch, then a sole conventional `main` or
`master` branch, and prompts if the base is ambiguous. The snapshot starts at
the merge base and includes branch commits, staged and unstaged edits, and
nonignored untracked files. If no usable base exists, it captures
working-tree changes against `HEAD`. It reports an empty changeset, a missing
Git repository, or an input too large to send instead of silently reviewing an
incomplete diff.
Binary changes are listed by path and status. Each review records a fingerprint
of its snapshot so the sidebar can flag findings as stale after files change.

## Requirements discovery

With `C-u`, the command prompts for a spec file. Otherwise it reads the
implementation shell's conversation/transcript and first resolves referenced
spec paths, including absolute and home-relative paths outside the repository.
If no path resolves, it searches spec-like Markdown files under project spec
directories and configurable roots, including `~/.codex` and `~/.claude` by
default. The search skips caches, limits depth and size, and prefers candidates
related to the project. If multiple credible files remain, the user chooses.

If no spec file is found, the implementation conversation becomes the
requirements source. The reviewer receives the relevant user instructions and
context, with provenance identifying them as conversation-derived. The command
does not silently truncate requirements. If neither a readable spec nor a
usable conversation exists, the sidebar asks for a requirements source before
starting the agent. An unreadable referenced file is reported and discovery
continues with the remaining sources. Transcript access is isolated behind one
adapter so changes to agent-shell's transcript storage do not affect the rest
of the package.

## Review protocol and result

Each pass starts a fresh agent-shell session rooted at the project, using the
preferred agent config by default and a review-specific config when set. The
review request contains the changeset snapshot, requirements source, and
instructions to inspect correctness without editing files. It directs the
agent to ask clarifying questions whenever success criteria are uncertain.
When the chosen agent exposes a read-only session mode through agent-shell, the
package uses it. The prompt instruction remains necessary for other agents.

The agent must return one structured result:

- **Questions:** numbered questions with the ambiguous criterion and why it
  matters. Questions pause the review; answers go to the same reviewer session.
- **Findings:** entries with `P0`–`P3` priority, file and line where
  applicable, short title, evidence, requirement reference, and suggested fix.
  `P0` means an immediate blocker, `P1` a significant correctness problem,
  `P2` a moderate problem, and `P3` a minor problem.
- **Clear:** no actionable correctness findings against the supplied
  requirements and changeset.

The parser validates the result and accepts a plain or fenced JSON payload.
If parsing fails, the package asks the same reviewer once to reformat its
answer without redoing the analysis. If it still fails, the review enters a
failed state. The sidebar shows a brief error and a link to the reviewer shell;
the raw answer stays there. It never reports an unparsed answer as a clear
review or silently drops findings.

## Review sidebar and actions

The review buffer appears immediately in a right-side window capped at about
one third of the frame width. Starting a review does not move keyboard focus.
Its header and mode line show collection, discovery, startup, review, questions,
findings, clear, or error. The body uses compact rows that remain legible at
sidebar width, with expandable details.

Question rows allow answers to be entered and edited in Emacs. Each question
needs an answer or an explicit "unknown" before submission. Submitting the
answers continues the same reviewer session; another question round is allowed.
The review stores these answers as clarified requirements and includes them in
fresh reruns and fix requests, including requests to a replacement
implementation shell. Finding rows allow navigation to a source location,
marking and unmarking, and sending the marked set as one request to the
original implementation shell. If that shell has closed, the command starts a
new shell and includes the requirements source. Marking alone never sends a
request.

Rerun (`g`) rebuilds the changeset and requirements context and starts a fresh
reviewer session. Existing findings remain visible but stale while the new pass
runs. The sidebar offers a way to open the reviewer shell for diagnosis. A
failed review retains its error and can be rerun. The first version need not
persist review history after Emacs exits.

## Notifications

Enabling `agent-shell-notify-mode` subscribes to existing and future agent-shell
buffers. A macOS system alert is sent for a permission request, a completed
turn, or an agent error. Permission requests say approval is needed. A normal
`end_turn` says the agent is ready for input; this also covers an ordinary
question, which agent-shell does not expose as a separate event. Other stop
reasons say the agent stopped and name the reason rather than implying success.
Errors say the agent failed. The title identifies the project/session. Alerts
are suppressed only when Emacs is focused and the relevant shell or review
buffer is selected.
Repeated events for one state are coalesced. An error supersedes a completion
alert for the same turn.

Alerts use the system's `osascript` executable asynchronously. Notification
text is passed as process arguments to a fixed script, without shell or
AppleScript source interpolation. The sender is replaceable for testing or
other platforms. Alerts are informational; clicking them does not navigate to
Emacs. A notification failure is logged and does not interrupt the agent
session.

Review sessions emit specialized alerts for questions ready, findings ready,
clear review, and review failure. The integration suppresses the generic
turn-complete alert for those sessions so the user gets one alert. The two
packages communicate through optional public hooks or predicates wired in
`mod-ai.el`, preserving independent use.

## Verification and limits

Focused automated checks cover Git snapshot selection in temporary
repositories, including a pushed feature branch; spec path resolution and
conversation fallback; valid and malformed responses; clarification carryover;
stale-result detection; notification stop reasons; and duplicate suppression.
Notification delivery is tested with a mock sender. Manual checks cover a
fresh review, a question round, sending marked fixes, rerun, sidebar layout at
different frame widths, and a macOS alert. Changed Elisp is syntax-checked and
byte-compiled through `emacsclient`.

ACP agents can disregard output instructions. The single reformat attempt and
visible failure state make this explicit. An agent without a read-only mode
could still attempt edits despite the review instruction; review startup must
not itself write project files, and supported read-only modes should be used
when available.
