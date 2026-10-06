# Agent-shell system notifications

## Purpose

Alert the user when an agent-shell agent has finished acting or needs input.
The alert should identify the session and say whether the agent is ready,
waiting for approval, or stopped with a problem. A visible macOS alert is
sufficient; clicking it does not need to navigate to Emacs.

## Package boundary

`lisp/agent-shell-notify.el` is a standalone Emacs package. It owns agent-shell
event subscriptions, event classification, duplicate suppression, and system
alert delivery. Public symbols use the `agent-shell-notify-` prefix. It depends
on agent-shell and built-in Emacs libraries, with no references to personal
`+` helpers. `modules/mod-ai.el` enables it and sets preferences; no new
`init.el` module is needed.

## Events and alert content

Enabling `agent-shell-notify-mode` subscribes to existing agent-shell buffers
and to shells created later. It removes subscriptions on buffer cleanup or mode
disable. It sends a system alert for these states:

- A `permission-request` says approval is needed.
- A `turn-complete` with `end_turn` says the agent is ready for input. This
  includes a free-form question, which agent-shell does not expose as a
  separate event, as well as a finished task.
- A `turn-complete` with another stop reason says the agent stopped and names
  the reason; it does not imply successful completion.
- An `error` says the agent failed and needs inspection.

The alert names the project and session. Repeated events for one state are
coalesced, and an error supersedes a completion alert for the same turn.
No alert is shown when Emacs has focus and the relevant shell buffer is
selected. A visible but unselected buffer does not suppress the alert.

## Delivery

The macOS sender runs `osascript` asynchronously. It passes notification text
as process arguments to a fixed script, without a shell or AppleScript source
interpolation. The sender is replaceable for tests or other platforms.
Notification delivery never blocks an agent session. A failed delivery is
logged in Emacs. Alerts have no click-through behavior.

## Verification

Focused automated checks cover attachment to existing and future shells,
subscription cleanup, mapping each event and stop reason to its message,
focus suppression, and duplicate suppression. A mock
sender verifies delivery without displaying macOS alerts. Manual checks cover
permission, completed-turn, and error alerts on macOS, plus
delivery failure. Changed Elisp is syntax-checked and byte-compiled through
`emacsclient`.
