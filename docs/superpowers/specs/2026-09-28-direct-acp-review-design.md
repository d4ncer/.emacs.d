# Direct ACP reviewer with main-session handoff

## Purpose

Keep the review workflow in Emacs without creating an agent-shell buffer for
the reviewer. A review starts from the implementation agent's Emacs session,
shows questions and findings in the existing sidebar, sends selected fixes back
to that exact implementation session, and permits a fresh review afterward.
The reviewer transport must use `acp.el` directly. No new implementation agent
is started by the review feature.

This design replaces the reviewer-session and fix-delivery architecture in
[the original review design](2026-09-27-agent-shell-review-design.md). The Git
snapshot, structured result, and sidebar concepts remain.

## Decision and alternatives

Use one dedicated ACP client for each review pass and a small integration
adapter to remember the implementation session. The review core has no runtime
dependency on agent-shell. The adapter in `mod-ai.el` uses agent-shell's
public insertion command to deliver fixes to the original session.

Starting a second ACP fixer would allow automatic fixing without an origin
session, but would lose the implementation conversation and need another
permission UI. Copying a fix prompt would remove all agent-shell integration,
but add a manual handoff on the common path. A single ACP process shared with
the implementation session would couple the review to the implementation
client's lifecycle and permissions. These approaches are outside this design.

## Package boundaries

- `lisp/agent-shell-review.el` remains the public coordinator, preserving
  `agent-shell-review` and its existing key binding. It depends on `acp.el`,
  the local Git/spec/protocol helpers, and built-in Emacs libraries. It does
  not require or call agent-shell.
- A small review transport helper owns ACP client creation, initialization,
  session creation, mode selection, prompts, subscriptions, cancellation, and
  shutdown. It reports events to the coordinator without rendering UI.
- `agent-shell-review-spec.el` accepts optional origin context as plain text.
  It contains no agent-shell transcript access. The Emacs adapter may supply
  text from the originating implementation session.
- `agent-shell-review-ui.el` owns the sidebar, questions, findings, marks,
  source navigation, and the diagnostic view. It calls a handoff callback
  stored with the run to send marked findings.
- `modules/mod-ai.el` owns the optional agent-shell adapter. It captures the
  exact origin buffer when a review starts there and supplies origin context
  and a send callback. Ordinary agent-shell and notification configuration
  remains separate from the review core.

The core's interactive command calls a configurable origin-provider function.
The adapter installs that function, so both `SPC l r` and
`M-x agent-shell-review` capture the caller. A review started from a source
buffer can select a matching implementation shell through the adapter; when
started from an agent-shell buffer it always captures that exact buffer.

The command can also start from a source buffer with no implementation agent.
Such a review can use a requirements file or entered requirements text. Its
findings remain inspectable; the fix request can be copied from the sidebar,
but no implementation session is created implicitly.

## Entry and requirements

The interactive entry preserves the current project and sidebar behavior.
When invoked from an agent-shell buffer, the adapter records that exact buffer
as the origin. A rerun from the sidebar carries the same origin and clarified
answers. The adapter passes the origin's conversation text to the core as
ordinary data; transcript access stays confined to the adapter. A closed
origin is never silently replaced by a different session.

An explicit file selected with `C-u` takes precedence. Otherwise requirements
discovery checks files referenced by supplied origin context and searches the
existing spec directories. If no usable file exists, usable supplied origin
context becomes the requirements source. If neither exists, prompt for a file
or multiline requirements text before starting ACP. Keep source provenance in
the run and reject unreadable or oversized input rather than truncating it.

Git base selection, snapshot contents, binary-file representation, input cap,
and fingerprinting retain the behavior specified in the original design.

## Direct ACP review session

The review has its own command, argument, environment, and optional model
configuration. The default uses the locally installed `codex-acp` and the
existing login-based authentication. These settings do not read agent-shell
configuration. The transport starts a process with `acp-make-client`, sends
`initialize`, creates a new project-rooted session, selects an advertised
read-only mode, and sends one text content block containing the review prompt.
If no supported read-only mode is available, startup reports an error before
submitting the review prompt.

The client advertises no write-text-file capability, rejects any incoming
`fs/write_text_file` request, and declines all `session/request_permission`
requests. Read-text-file requests are served only for readable files within
the project or explicitly selected requirements source; other requests fail
with an ACP error. The prompt still instructs the reviewer not to edit files.
These are protocol-level safeguards, not an operating-system sandbox: an agent
subprocess with direct filesystem access might still write outside ACP.

`session/update` agent-message chunks accumulate as the raw answer. The
`session/prompt` response is the completion signal; only `end_turn` results
are parsed. Questions, findings, and clear results use the existing validated
JSON schema. Question answers and the one permitted parse-repair request go to
the same ACP session. A failed request or process exit produces a visible error,
never a clear result. The diagnostic command `v` opens the captured raw answer,
ACP errors, and available traffic in a review diagnostic buffer, not an
agent-shell buffer.

## Main-session fix handoff

The sidebar's `S` action builds one prompt from the marked findings, original
requirements, and all clarified answers. The run retains a function supplied
at entry that sends this prompt to the exact origin implementation session.
For file-backed requirements, the prompt references the spec path and asks the
implementation agent to read it; entered or conversation criteria remain inline.
The agent-shell adapter implements that function with `agent-shell-insert`
using the captured buffer, `:submit t`, and `:no-focus t`. The core sees only
the callback; it neither locates agent-shell buffers nor starts a replacement
shell. This is the only agent-shell integration needed for the common path.

If the origin has closed, is busy, or lacks a ready session, sending reports
the condition and leaves the marks and assembled prompt available for retry or
copy. The adapter checks readiness before calling `agent-shell-insert`; success
is shown only when it returns insertion details. Marking a
finding alone never sends anything. From a review without an origin callback,
`S` opens the assembled prompt for copying instead of creating an agent.

## Sidebar, notifications, and lifecycle

The sidebar appears immediately on the right without taking focus. It uses
the normal Doom modeline, Evil normal state, and the mode's keymap; it shows
no key hints in the buffer or modeline. Redraws keep point on the selected
item. Questions can be answered and submitted; findings can be expanded,
marked, visited, and sent together. Existing findings remain visible as stale
while a rerun starts.

Each rerun cancels and shuts down the previous reviewer client before starting
a fresh one. Callbacks carry a run identity so late events cannot overwrite a
new run's sidebar. A findings, clear, or terminal error result shuts down its
ACP client after preserving diagnostics. A question round keeps the session
alive until answers, cancellation, or rerun. Hiding the sidebar does not
cancel an active review. Killing its review buffer cancels and shuts down the
client.

The review retains its status-change hook. A review notification callback can
announce questions, findings, clear, or error without requiring
`agent-shell-notify`. The Emacs adapter supplies platform delivery and ignores
alerts while the review sidebar is focused. No reviewer buffer appears in
`agent-shell-buffers` or `agent-shell-switch-buffer`.

## Verification and limits

ERT tests with a fake ACP transport cover request order, mode selection,
question continuation, result parsing, write-request denial, error and
shutdown paths, stale callbacks, and reruns. Handoff tests verify that `S`
targets the original implementation buffer and preserves marks when that
buffer is busy or closed. Requirements tests cover explicit files, supplied
origin context, and prompted fallback. An integration check confirms no
reviewer `agent-shell-mode` buffer is created. The existing Git and sidebar
tests continue to run; syntax and focused byte compilation check changed
Elisp. A manual review pass verifies the real `codex-acp` handshake and
sidebar/notification behavior in running Emacs. This check establishes whether
the configured agent advertises a usable read-only mode before the transport
migration is considered complete.

The review core no longer depends on agent-shell. Automatic handoff to the
main implementation session still requires the optional Emacs adapter because
that session is owned by agent-shell. ACP-level read-only controls cannot
guarantee process-level filesystem isolation.
