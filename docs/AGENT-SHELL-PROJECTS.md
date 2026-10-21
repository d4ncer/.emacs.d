# Agent shell projects

An Emacs workspace for reviewing and revising documents and code with
agent-shell. Projects keep saved versions, anchored discussion, and review
checkpoints independently of Git commits.

## Start a project

1. Use `SPC aP` (`C-SPC aP` in insert state), or
   `M-x +agent-shell-projects`. Choose an existing project or create a named one.
2. Press `?` for the project transient. Choose `f` to enrol a Markdown or Org
   document. With a prefix argument, choose an existing Org heading instead.
3. Choose `a` to associate an existing agent-shell buffer or start a fresh
   author. A prefix argument to `+agent-shell-projects-choose-author` starts
   fresh directly.
4. Press `RET` on an artefact to open its current saved contents and sidebar.
   Git enrolment returns immediately; its first capture runs in a child Emacs
   process. The sidebar shows progress and a **Cancel** action. When the first
   capture finishes, a visible waiting view is filled without selecting it.

Home shows the project name, linked author buffers with live status, and
artefact names. The coloured dot means there is new material to review.
Opening an artefact does not clear that indicator. Use **Submit feedback** or
**Mark reviewed** when you finish a pass.

A project may contain documents alone. For code, enrol a stable proposal and
choose it with transient `p`, then enrol the Git workspace with `w`. Work in
your existing checkout or worktree; the extension does not create branches or
worktrees. The requested base defaults to `HEAD`; choose the intended base
explicitly when importing already committed work, for example a branch point
or `main`. The resolved commit is retained as the solution base.

## Review and revise

Select a passage and use `C-.` for Embark:

| Target | Action |
| --- | --- |
| Selected passage | `c` compose comment; `q` ask a fresh agent |
| Annotation or sidebar thread | `RET` visit passage; `r` reply; `s` resolve/reopen; `a` reattach; `q` ask |
| Artefact on home | `RET` open; `h` history; `d` compare |
| Linked shell | Existing Embark buffer actions |

Finish a comment or reply with `C-c C-c`. It is saved as feedback for the next
batch. The sidebar shows current agent activity followed by expandable comment
threads in passage order. Use `TAB` to expand a thread, `f` to include resolved
threads, and `C-.` for its actions. Clicking a highlighted passage opens its
thread. Uncertain, changed, or deleted attachments are labelled; resolution
remains a separate choice that only you can make.

Use transient `s` to send all new comments and replies to the author. The agent
can revise source immediately and respond through the shared annotation API.
Your viewed revision stays stable until you refresh it. Opening code and
cycling its views use saved captures and do not run Git. Review the responses
and changes, add further feedback, and submit another batch. Unresolved
annotations from earlier versions remain available throughout this loop.

Submitting acknowledges the versions you actually viewed, even if source has
advanced. Unopened artefacts keep their previous checkpoint. Use transient
`m` for a review pass without new feedback. Merely resolving a comment does
not acknowledge a whole revision.

## Source editing and history

| Key in an artefact | Action |
| --- | --- |
| `C-c C-e` | Edit live source in its ordinary native mode |
| `C-c C-p` / `C-c C-n` | Previous / next saved version |
| `C-c C-g` | Capture and show current contents |
| `C-c C-h` | Project home |
| `C-c C-m` | Project transient |

Normal and motion states also bind `C-p` / `C-n` for history. The modeline
shows version position; home deliberately omits it. History navigation never
replaces the source file. Saving a source buffer captures a new version.

Save your source edits before submitting feedback or annotating live code.
Code saves queue a capture without delaying the save. Wait for that capture
before attaching a live-source selection; saved diff passages remain available
while capture is running. `C-c C-g` requests a fresh capture and updates the
view when it finishes, provided you are still viewing the same revision.
If an agent changes disk contents while a document buffer has unsaved edits,
both copies are preserved: your buffer stays intact and the disk copy is saved
in the local store. The sidebar offers a native diff against disk so you can
reconcile and save. Unmodified stale buffers are refreshed automatically.
Deleting a source file leaves its previous snapshots available.
If an enrolled Org heading disappears, its last saved contents remain openable
and the sidebar identifies the missing source.

Org subtree enrolment adds an `ID` property and saves the file. That ID tracks
the heading across renames and moves; it is independent of its heading text.

## Side questions and extracted prose

Embark `q` starts a fresh agent with the selected passage, its exact saved
version, the proposal, and project discussion supplied automatically. Discuss
the question normally. Use `C-c C-o` in that shell, or transient `r`, to
explicitly request an outcome draft. Edit the resulting composer and press
`C-c C-c` to attach it as a comment on the original selection. You may capture
several outcomes from one conversation. Closing a composer does not attach it;
capture again to reopen a ready draft.

Select prose in an agent-shell conversation and run
`M-x +agent-shell-projects-extract`. Choose a Markdown file, an Org file, or
an existing/new heading in an Org file. The saved destination is enrolled.
Existing file contents are appended to, using the ordinary source buffer.
Markdown extraction to Org converts basic headings, links, emphasis, inline
code, and fenced blocks. Complex Markdown may need editing afterward.

Side-conversation reopening is outside v1; captured comments retain their
original context. Transient `k` cancels a queued delivery or pending generated
result. Late results for cancelled requests are rejected.

## Code review

Transient `c` opens code review. Generate explanations with `G` when needed.
The three views share one immutable capture:

| Key | Action |
| --- | --- |
| `C-c C-v` | Cycle summary → walkthrough → diff |
| `TAB` | Expand/collapse a Magit-style section |
| `RET` | Follow a link, or edit source at a diff line |
| `M-,` | Return to the previous review location |
| `C-c C-d` | Compare whole solution / changes since last reviewed capture |
| `C-c C-a` | Restore the whole diff after following a focused link |

An open comparison retains its baseline when feedback delivery or **Mark
reviewed** advances the checkpoint. Refresh explicitly to compare against the
new checkpoint. Newly returned explanations update visible summary and
walkthrough views without selecting windows. Repeating **Generate explanations**
reuses an existing result or pending request for that version.

Files and hunks start collapsed. Expanding a file prepares its diff in a child
process; expanding a hunk loads its first 200 lines. **Load more lines** adds
the next block. Large file and hunk lists also have continuation buttons.
Visiting an annotation loads the necessary passage incrementally. Searches
operate on the diff content currently loaded into the buffer.

Capture requests are debounced and combined per workspace, including requests
for different bases. Immutable base objects are read in batches and cached.
Git's configured filesystem monitor is respected. The worker rechecks source
consistency before publishing a capture; expensive scans remain outside the
interactive editor. Superseded results cannot replace newer requested work.
Cancel through the sidebar or transient `C`; the previous capture remains usable
after cancellation, timeout, or failure. Refresh retries a failed capture.

Summary and walkthrough explain outcomes, constraints and priorities or actual
weights, alternatives and tradeoffs, relationships among changes, and
validation evidence or gaps. Walkthrough entries distinguish recorded
decisions from inferred intent and link to the captured proposal and related
file hunks. View positions are retained while navigating. Explanations are tied
to exact code and proposal captures; an older explanation is not relabelled as
current after source changes.
These are agent-authored explanations. Unrecorded priorities or weights cannot
be recovered with certainty; inferred intent must be identified as such.

Diff capture includes committed, staged, unstaged, and non-ignored new files
since the chosen base, regardless of who authored them. It records renames,
deletions, binary files, modes, and symlinks. It does not stage or commit changes.
Binary contents are retained, with a binary-change notice in the diff.
Select lines on either side, including removed text, to annotate them.

For existing work without a proposal, transient `P` asks the author to
reconstruct one. Review and edit the draft before saving; inferred intent and
questions should remain explicit. Proposal and implementation can then evolve
together through the ordinary feedback loop.
Wait for enrolled artefacts to complete their first capture before requesting
reconstruction. A failed context provider leaves no incomplete queued request.

## Configuration and extension points

```elisp
(with-eval-after-load '+agent-shell-projects
  ;; 'default inherits agent-shell-preferred-agent-config when config is nil.
  ;; Set policy to 'ask to choose a configuration for each fresh shell.
  (setq +agent-shell-projects-agent-policy 'default
        +agent-shell-projects-agent-config nil))
```

`+agent-shell-projects-context-function` receives `(PROJECT REQUEST)` and
returns the complete prompt string. The default is
`+agent-shell-projects-default-context`; replace or wrap it to change both
context selection and prompt format in one place. Context contains immutable
document text and actual saved before/after file contents for code captures.
`+agent-shell-projects-send-function` is the delivery adapter, called with
`(SHELL TEXT)`; return non-nil only after acceptance. Busy shells, disconnected
shells, and shells containing unsent input retain queued requests for retry.

`+agent-shell-projects-capture-delay` defaults to 0.3 seconds.
`+agent-shell-projects-worker-limit` defaults to two concurrent workers, with
capture requests taking priority over queued diff preparation.
`+agent-shell-projects-worker-timeout` defaults to 300 seconds per running job.
Workers use this Emacs executable in batch mode; no additional runtime is needed.
Streamed text and tool chunks cause no project writes or review refreshes.
Other UI updates are coalesced and never reopen hidden sidebars.

`+agent-shell-projects-directory` defaults to
`var/agent-shell-projects/` beneath this Emacs directory. It contains atomic
JSON project manifests and immutable content-addressed snapshots. Keep that
directory to retain history across Emacs restarts. Shell associations reconnect
when matching session buffers are available; select a replacement author
through `a` otherwise. Already queued requests retain their recipient; cancel
and resubmit them if replacing that author.

`+agent-shell-projects-inbox-directory` defaults to a per-user temporary
directory so locally sandboxed agents can write operations. Use a shared
writable location if your agent has different filesystem access. The persistent
store is independent of this inbox. The global
`+agent-shell-projects-mode` polls it and maintains live agent subscriptions;
opening a project enables the mode. Disable the mode to stop both.

### Agent operation API

The automatic prompt supplies project, agent, artefact, version, annotation,
and request IDs. An agent writes a complete JSON object to a temporary file,
then atomically renames it to a unique `*.json` file in the supplied inbox:

```json
{
  "id": "unique-operation-id",
  "project": "project-id-from-context",
  "agent": "agent-id-from-context",
  "op": "reply",
  "annotation": "annotation-id-from-context",
  "body": "Revised the fallback behavior; verified both error cases."
}
```

| Operation | Additional fields |
| --- | --- |
| `annotate` | `artefact`, `version`, `start`, `end`, `body`; optional `context` for a diff or explanation |
| `reply` | `annotation`, `body` |
| `progress` | `body` |
| `draft` | `request`, `body` for a pending outcome or proposal request |
| `explanation` | `request`, `artefact`, `version`, `summary`, `walkthrough` |

Document offsets are zero-based character offsets into the supplied immutable
text. Diff context includes `space: "file"`, `path`, `side: "old" | "new"`,
`comparison`, and the original `text`. Explanation requests describe their
walkthrough schema in the prompt, including links and validation evidence.
Only author-role agents may create annotations directly. Side agents produce
drafts for explicit capture. Neither role can resolve or reopen annotations.

The extension validates and commits an operation atomically, deduplicates by
operation ID, writes `<input>.result` with `ok` and `result` or `error`, then
renames the input to `<input>.processed`. Retrying an accepted ID returns its
original result. Failed operations leave project state unchanged. This API is
also available in Lisp as `+agent-shell-projects-apply-operation`;
`+agent-shell-projects-operation-handlers` supports additional operation types.

## Verification

Run from the configuration root:

```sh
emacs -Q --batch -l test/run-agent-shell-projects.el
emacs -Q --batch -l test/agent-shell-projects-startup-test.el
```

The suite uses disposable documents, repositories and worktrees. It includes
real workers behind deliberately slow Git commands, an editor heartbeat and
editing checks during capture, cancellation and timeout, job coalescing and
stale-result rejection, batched base caching, bounded diff expansion, and hidden
window behavior with normal UI hooks. Its local
ACP fixture exercises real agent-shell process delivery and structured results
without contacting a model provider. It also checks strict byte compilation.
The startup check verifies deferred activation and native General/Evil keys.
Workflow regressions also check explanation arrival, historical code and
proposal context, repeat generation, missing Org headings, request failures,
and comparison stability while review checkpoints advance.
