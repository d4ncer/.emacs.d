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
| 2. Cross-vendor panels | Partial | `how` and `interrogate` panels run as read-only sibling shells; `arena` is planned |
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
agent-shell. It covers the `how` and `interrogate` shapes in
`+agent-shell-pstack-panel-shapes`. `arena` is still planned, because its
members write code and need isolated, sandboxed worktrees. Its design is in
`docs/AGENT-SHELL-PSTACK-ARENA.md`.

### Request a panel

A lead is a shell with `+agent-shell-pstack-mode` on. Its priming prompt
carries the protocol, which names every agent in `agent-shell-agent-configs`
that has a read-only mode and tells the lead to use panels in place of the
`how` and `interrogate` skills' own subagents. Every later prompt adds one
line that restates the rule and names
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
report.

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
interrupted panel does not resume. Killing the lead stops its unfinished
panels.

Roles map to agent configs rather than model slugs; record that fork with its
reason. `lisp/+agent-shell-drive.el` holds the session start, delivery and
event watching shared with `agent-shell-projects`.

## Layer 3: views

`C-c P` in an agent shell runs `+agent-shell-pstack-panels`. It lists every
panel since Emacs started, with its id, shape, status and lead, and each
member's role, agent, state and report. `RET` visits the report or the member
or lead shell. The buffer redraws whenever a panel changes. The decision trail
and plan views are planned.

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
