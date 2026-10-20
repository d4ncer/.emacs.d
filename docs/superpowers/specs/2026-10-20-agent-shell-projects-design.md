# Agent shell projects v1 proposal

Status: Implemented as `+agent-shell-projects` on `feat/agent-shell-projects`.
See the [usage guide](../../AGENT-SHELL-PROJECTS.md) and
[implementation record](../plans/2026-10-20-agent-shell-projects.md).

## Problem

Reviewing agent-authored documents and code currently requires moving between
GitHub, source files, and agent-shell conversations. Feedback loses its
connection to the relevant text, explanations are difficult to recover, and
focused discussions do not have a reliable route back into authoring. Reviewing
files individually also obscures how a code changeset fits together.

An Emacs project workspace should bring artefacts, annotations, authoring, and
focused questions together. The user and agent can revise the proposal and
implementation concurrently as their understanding develops.

## Success criteria

1. The project home shows artefact names and a binary indicator of new material
   for the user to review. Agent references open actual shell buffers and show
   live status.
2. The user can annotate selected prose or code, ask a fresh agent a focused
   question, and explicitly capture an editable outcome as an annotation.
3. Agents can raise annotations, reply to feedback, and report progress through
   the same application model. The user controls resolution and reopening.
4. Feedback is submitted in batches. The author revises immediately and returns
   responses and changes for review. Further rounds can add comments and side
   questions while retaining unresolved feedback from older versions.
5. Comments preserve their original passage and revision. Ordinary edits and
   identifiable moves retain their attachment; uncertain matches remain visible
   for reassessment.
6. Opening an artefact shows its current version and a sidebar. The user can
   edit directly and browse previous and next saved versions. Opening or
   browsing alone does not mark the artefact reviewed.
7. Code review connects a summary, a behavior and decision walkthrough, and a
   Magit-style annotated diff for the same captured state.
8. Projects, saved versions, annotations, and review checkpoints survive an
   Emacs restart. Shell associations can be reconnected.

Review explanations cover the problem, success criteria, constraints and their
relative priorities or actual weights, alternatives considered, accepted
tradeoffs, how changes fit together, and validation. Recorded decisions,
inferences, and unanswered questions remain distinguishable. Reconstructed
explanations do not claim to be the original author's recorded intent.

## Scope

### Projects and agents

A project has a user-visible name, artefacts, and a collection of agent
associations. Its name is stored and editable; an initial suggestion can come
from a proposal title, Org heading, or filename. The project name is independent
of a directory name and an evolving agent session title.

V1 has one selected authoring agent receiving feedback. Its association points
to an agent-shell buffer and persists enough session information to reconnect
or select a replacement. Starting fresh and continuing an existing shell are
both supported. Side conversations use fresh shells. The model accommodates
multiple associated agents without defining multi-agent authoring orchestration
in v1.

### Document artefacts

Supported targets are Markdown files, Org files, and individual Org subtrees.
Prose inside an agent-shell conversation can be selected and extracted into a
Markdown file, an Org file, or a heading in an existing Org file. The saved
destination becomes the artefact that is reviewed and revised.

A document project can stand alone. A code project includes its stable proposal
and can include related specifications and other documents. Feedback can affect
several artefacts in one authoring round.

### Code artefacts

A code project attaches to an existing Git workspace. A normal checkout and a
linked worktree are supported equally. The user manages the branch or worktree.

The code artefact covers the workspace changes since a chosen base revision:
committed changes, staged and unstaged changes, and new non-ignored files.
Contributions from associated agents and the user's edits are included. Project
membership does not depend on identifying which agent wrote each hunk.

The base revision defines the whole solution diff. Review checkpoints separately
define changes since the user's last review. Git commits and review checkpoints
do not need to coincide. File contents required for a review are captured
without staging, committing, or checking out source changes.

### New and existing work

New code work begins with a stable proposal covering the problem, success
criteria, scope, exclusions, possible solutions and tradeoffs, and a
recommendation. Proposal review and implementation may overlap.

Existing documents and code can be enrolled. The agent assembles the proposal
and code explanations from available material, identifies inferred intent and
missing context, and lets the user correct them through annotation.

### Deferred capabilities

- Whiteboards and external artefacts.
- A UI for reopening a side conversation from an annotation.
- Automatic annotation capture during side conversations.
- A visual undo tree, history branching UI, and history restoration controls.
- Multi-agent task scheduling and feedback routing among several authors.

## Project home

The home is inspired by Magit status, with contextual actions and navigation.
Its minimal presentation is:

```text
Portable thumbnails

*Codex @ portable* · busy

● Thumbnail proposal
  Image specification
● Implementation
```

An accent-coloured icon means there is new material for the user to review. A
row without the icon has no new material. Type, revision numbers, counts, and
the reason for attention stay out of this list. The application retains those
details for the artefact view and actions.

The attention state combines new source content and relevant annotation
activity since the acknowledged checkpoint. An open comment awaiting the agent
is not, by itself, new material for the user. Submission or marking reviewed
acknowledges the captured material; events arriving afterward remain new.

Clicking an artefact or pressing RET opens its current version and sidebar.
Agent names open their actual shells. Their statuses update live without
moving point. A disconnected association offers reconnection or replacement.

The project transient contains project operations: enrol artefacts, extract
conversation prose, choose an author, submit feedback, mark reviewed, refresh,
and enter code review. Code summary, walkthrough, annotations, and history do
not become permanent sections on the home.

## Artefact buffer and sidebar

Opening captures the current artefact state for stable inspection. Subsequent
agent edits produce an update indicator; the user chooses when to refresh.
Version information belongs in the artefact modeline. Direct editing uses the
ordinary Markdown, Org, or source editing experience in the main window.

The sidebar has two areas:

1. Agent activity: relevant shell references, live status, and explicitly
   reported current work. More detail and the complete shell are available on
   demand. Activity text does not invent the agent's current task.
2. Annotation threads: passage excerpts in document order, expandable to show
   the comment and replies. A label is the passage the thread concerns, rather
   than a category such as compatibility or alternatives.

Selecting a thread visits and highlights its passage. Selecting an annotation
in the document opens its thread. Moving through the document can highlight
nearby threads while preserving the user's expansion and scroll choices.
Resolved comments are available through a filter. The sidebar can be hidden,
resized, and restored, retaining state per artefact.

Embark supplies actions on targets:

| Target | Example actions |
| --- | --- |
| Artefact | Open, compare, browse history |
| Selected passage | Add comment, ask a fresh agent |
| Annotation | Visit passage, reply, ask a fresh agent, resolve or reopen |
| Agent reference | Visit shell |

The existing `C-.` interaction exposes contextual actions. RET retains a direct
default action. Composing a comment or reply opens an editor associated with
the selection or thread. The sidebar does not need a permanent action toolbar.

## Annotation and authoring cycle

An annotation records a stable ID, author, target artefact and original
snapshot, selected range, original quotation and surrounding text, body,
replies, submission information, and open or resolved state. Attachment health
is independent of resolution. Only user operations resolve or reopen comments.

The user can add comments directly. Associated agents can raise questions,
observations, and changed assumptions using the same annotation model. Replies
stay with their thread. New concerns become new annotations.

Feedback submission records a checkpoint using the versions actually reviewed,
even if the author has already produced newer source contents. It includes the
specific comments and replies in the batch and their original target snapshots.
Unopened artefacts keep their earlier acknowledgement. The author receives the
feedback and revises the current work, then returns a response for each item
and links to resulting changes or validation. An explanation, disagreement, or
question can be a response when no source change was made.

After successful submission, the captured state becomes the latest reviewed
checkpoint. A pass with no feedback can explicitly mark the captured state
reviewed. New activity during submission is not acknowledged accidentally.
Unresolved comments retain their original context across all subsequent
checkpoints. The user inspects responses, resolves items, follows up, or starts
another round.

For example, a v4 artefact can be compared with its last reviewed v3 while an
open annotation still refers to a passage originally reviewed at v1. The
annotation's age does not change the comparison baseline.

Proposal and implementation continue evolving during this cycle. Changes to
success criteria, priorities, or recommendations should be recorded in the
proposal and surfaced by an author annotation when user input is needed.

## Side questions

1. Select text and invoke Embark's ask action.
2. Start a fresh shell. Configuration selects either a default agent or a
   prompt for agent selection, following agent-shell conventions.
3. Automatically supply the question, passage, exact artefact snapshot,
   proposal, and relevant project decisions. The agent can investigate related
   project material.
4. Discuss the question in that shell.
5. Invoke Add outcome as comment. The side agent drafts a concise annotation,
   the user can edit it, and an explicit action attaches it to the original
   passage. Capture can happen several times within one conversation.
6. Captured feedback joins a batch for later submission to the author.

The default selection policy inherits agent-shell's preferred agent. An
extension setting can supply another default or always ask the user to choose.
When no default is available, use the existing chooser.

Context construction is a replaceable function with explicit project,
artefact, snapshot, selection, and question inputs. Its output feeds session
priming. Changing context contents or format does not require modifying session
creation or annotation capture. The same separation applies to author feedback
context and imported-work reconstruction.

## Saved versions and attachment tracking

V1 offers previous version, next version, and return to current, following the
git-timemachine interaction. Browsing previews saved contents and associated
annotations; it does not edit source or change review acknowledgement.

Saved snapshots preserve exact reviewed contents. They are taken at meaningful
boundaries such as annotation, review capture, and author handoff, with identical
contents shared. Per-keystroke undo is left to normal Emacs editing. Stored
review history is independent of buffer undo history and Git commits.

Markers track straightforward edits while a buffer is live. Mapping between
saved snapshots uses differences and quotations with surrounding text. A
confidently identifiable move can retain its attachment. A substantial rewrite,
deletion, or ambiguous match preserves the annotation and original excerpt for
reassessment rather than attaching it to an arbitrary passage. The user can
reattach it through an annotation action.

## Code review workspace

Review code is a project transient action. It opens three connected views of
one captured workspace state:

| View | Contents |
| --- | --- |
| Summary | Problem, outcome, success criteria, governing constraints and priorities, major tradeoffs, validation and gaps |
| Walkthrough | Ordered behaviors and decisions, their contribution to the outcome, rationale, alternatives, proposal links, code links, and validation |
| Annotated diff | Project changes in expandable Magit-style files and hunks, with annotations |

Cycling views retains position and follows the current behavior or decision
where links exist. Walkthrough links can focus the diff on related hunks across
files; the whole project diff remains accessible. Back navigation returns to
the originating explanation. All three views support annotations and side
questions. The views do not add artefact rows to the home.

Comparison can show the full solution against the Git base or changes since
the last reviewed checkpoint. A diff annotation identifies its side and source
snapshot, including deleted code. Direct editing visits the corresponding
current source location. Explanations are maintained against their captured
state; newer source contents cannot silently inherit an older explanation.

## Technical recommendation

Keep project metadata, immutable snapshots, annotations, and checkpoints in a
small local store. Use existing Emacs libraries for the UI: Magit sections,
Transient, Embark, side windows, ordinary editing modes, and diff facilities.
Use public agent-shell interfaces for shell association, status, events, and
prompt delivery. Repository activation belongs in `modules/mod-ai.el` and
preserves deferred loading.

The installed agent-shell provides `agent-shell-start` for starting a shell
with a chosen configuration, `agent-shell-select-config` for selection,
`agent-shell-status` for live state, `agent-shell-subscribe-to` and
`agent-shell-unsubscribe` for events, and `agent-shell-insert` for prompt
delivery. Keep these interactions behind the extension's agent adapter.

The store uses a JSON manifest per project and content-addressed text
snapshots under a configurable directory, initially
`var/agent-shell-projects/`. Atomic metadata replacement and stable IDs make
records durable without adding commits to the source repository. Code capture
stores changed source and required base contents rather than copying a whole
repository.

A project contains artefact references and agent associations. A checkpoint
maps artefact IDs to exact snapshots and acknowledged activity. Code snapshots
record the Git base and required workspace contents. Annotation records retain
their original anchors and current attachment information. Short-lived side
question state records its original selection and capture requests.

Agent annotation, reply, progress, and explanation operations enter through a
structured application interface. They carry stable IDs and explicit artefact
and snapshot references. Source authoring continues through the agent's normal
editing tools. Supported agent operations do not include resolution. Prose
streaming is not treated as an application operation.

## Alternatives and tradeoffs

| Choice | Benefit | Cost |
| --- | --- | --- |
| Existing workspace scope | Fits ordinary branches and user-managed worktrees; includes human refinements | The user separates unrelated projects using their own workspaces |
| Per-agent edit attribution | Can separate contributions sharing a workspace | Requires reliable provider-specific capture and historical attribution; superseded by the workspace decision |
| Snapshots and anchors | Preserve original evidence and support native editing | Mapping substantial rewrites still needs reassessment |
| Git commit per review revision | Reuses committed history | Couples review checkpoints to source repository operations |
| Explicit side-question capture | User controls considered feedback; several outcomes can be captured | Requires a capture action and draft review |
| Automatic capture | Preserves intermediate insights with less interaction | Needs provisional, superseded, and consolidated annotation handling; deferred |

The proposed priority order is preserving feedback and review evidence, keeping
the Emacs interaction small, supporting the iterative authoring loop, then
adding automation. No numerical constraint weights were established.

## Implemented technical choices

- Code base selection: prompt with HEAD as the default and resolve the selected
  revision to a commit. Imported work requires choosing its intended branch
  point or target base explicitly; no tracking branch is assumed.
- Org subtree identity: retain an existing Org ID or create one and save at
  enrolment. Lookup ignores review projections and searches native sources.
- Agent operation transport: agents atomically deposit structured JSON files
  in a per-user writable inbox. Emacs polls, validates and commits each operation
  atomically, returns a result file, and deduplicates operation IDs. This avoids
  requiring agents to access the Emacs server socket. Lisp entry points and
  operation handlers remain independent of transport and context construction.
- Agent requests and completion: correlate draft and capture requests with
  their side conversation and snapshot, including interruption, retries, and
  failed or empty results. Ordinary turn completion is not proof of a valid
  annotation or explanation.
- Editing: keep stable review projections separate from ordinary source buffers.
  Require saving source edits before feedback submission or live-code comments.
  Preserve unsaved document edits and an immutable disk copy on overlap; offer
  a native diff for user reconciliation. History browsing never rewrites source.
- Workspace capture: detect files changing during capture, handle renames and
  deletions, and represent binary changes without promising inline review of
  non-text artefacts. Recheck the change list, bytes and modes before capture,
  retrying a moving workspace up to three times.

## Verification

Focused ERT checks should exercise durable snapshot and checkpoint behavior,
annotation survival across edits, user-only resolution, submission boundaries,
and stale-response correlation. Git fixtures must cover an ordinary checkout
and a linked worktree, including committed, staged, unstaged, new, renamed, and
deleted files. Integration checks cover Embark targets, shell event lifecycle,
and deterministic context builders.

A manual acceptance scenario starts from an existing Markdown proposal,
extracts conversation prose into an Org heading, adds direct feedback, captures
an editable side-question outcome, submits a batch while authoring continues,
and reviews a new revision with an unresolved older annotation. A code scenario
cycles through the three views and edits source. Restarting Emacs must preserve
project state and allow shell reconnection.
