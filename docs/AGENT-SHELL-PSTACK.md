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
| 2. Cross-vendor panels | Planned | Emacs runs `interrogate`, `arena` and `how` panels as sibling shells |
| 3. Views | Planned | Decision trail, plan and panel status in a side buffer |

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

## Layer 2: cross-vendor panels (planned)

Upstream panels mix model families. Ports to one harness lose that, or shell
out to other vendors' CLIs. agent-shell can start a shell for any configured
agent instead:

1. The lead writes a `panel` request (brief, roles, shape and report paths)
   to a file inbox, then ends its turn.
2. Emacs starts one shell per role from agent configs, read-only through the
   session mode and a denying permission responder, or in a worktree for
   `arena`.
3. Each member writes its report to the given path. Emacs waits for
   `turn-complete` and falls back to collected message chunks.
4. Emacs queues a follow-up prompt to the lead with the report paths. The lead
   still triages and judges.

Roles map to agent configs rather than model slugs; record that fork with its
reason. Extract the session start, await and inbox code shared with
`agent-shell-projects` before building this.

## Non-goals

- Reimplementing playbooks in Elisp.
- Routing every session into poteto-mode automatically. Upstream keeps it
  opt-in.
- Reusing `C-c C-m` or `C-c C-o`, which agent projects owns.
