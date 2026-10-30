# UI guidelines for this configuration's screens

These rules apply to every screen this configuration draws: views such as
the pstack panels view (`C-c P`), the buffer and mode-line annotations it
adds to agent-shell, and inline renderings such as the pstack request card.
They are adapted from the interaction design the user agreed for another
of their Emacs projects, and they describe how the user works: keyboard
only, in Evil normal state, scanning first and opening detail on demand.

## Information

1. **Show the minimum needed to decide.** A screen leads with what the user
   acts on: the subject, its state, and the one fact that decides what to
   do next. Supporting detail (paths, ids, reports, full text, timestamps)
   is folded and expands in place.
2. **One line per item when scanning.** An item's heading fits on one
   line. Its body is folded by default unless it needs the user.
3. **Long text shows an excerpt.** A report, brief or follow-up shows a
   meaningful first line or two with explicit expansion, never the whole
   text inline by default.
4. **Ids and internal names stay out of the way.** Show an id only where the
   user needs it to act, such as a branch to review, and never in mode lines
   or headers. Status annotations say what the state is ("panel"), not
   which internal object holds it.
5. **Truthful, visible progress.** Every running state is shown, however
   short, in user terms ("Judging", "On host", "Waiting for your review").
   Progress never shows a finished step as current work.

## Keys and actions

6. **Keyboard only, each key shown once.** A screen has one Actions block,
   grouped by purpose (for example Act, Navigate, Manage), listing each key,
   its label and what it acts on, with a dim effect line where the effect is
   not obvious. `?` lists every key for the item at point. Headers, footers
   and item bodies carry no key hints, and there are no rows of buttons.
7. **Keys act on the item at point.** Single-letter keys act on the item
   point is in. Keys that act on the whole screen use a distinct chord or
   capital letter and say so in the Actions block. `RET` runs the most
   useful action for the item, and on an Actions row runs that action.
8. **Evil gets the screen's keys.** Bind single-letter keys for Evil normal
   and motion states on the screen's own mode, through `:general` in the
   module that configures it, so Evil's defaults (such as `D` or `x`) never
   win. Inside editable text they step aside.
9. **Show only actions that apply.** An action that cannot run for the item
   at point is not offered, or refuses with its reason.

## Navigation and place

10. **Move between logical items.** `C-n`/`C-p` (and `n`/`p` where the screen
    is read-only) move between items, including long or expanded ones.
    `TAB` folds the item at point.
11. **Place survives redraws.** A refresh keeps point on the same item and
    column, and keeps what was folded. Leaving and returning restores the
    place.

## Appearance

12. **Theme faces only.** Every visual state is a named face inheriting a
    theme face (`success`, `warning`, `error`, `shadow`, font-lock faces,
    `help-key-binding`), never a raw colour, so screens work in light and
    dark themes.
13. **State by shape as well as colour.** A state carries a glyph and a word,
    for example `● RUNNING`, `◆ NEEDS YOU`, `✓ DONE`, `✗ FAILED`, so it reads
    without colour.
14. **Rules and indentation give structure.** Separate items with a thin
    rule, put the heading on one line, and indent the body with detail labels
    in a dim column.

## Reference sketch: pstack panels view

`C-c P` is the reference screen. Other screens start from its pieces: a
summary line, the one Actions block, and one card per item after a rule.

```text
pstack panels · 1 running, 1 done

Actions
Act
  RET    Visit            the item at point
  c      Cancel           the running panel at point
  x      Clean up         the finished panel's workspace, or the record at point
  l      Request log      the lead of the item at point
Navigate
  n / p  Next / previous  item
  TAB    Fold / unfold    the item at point
Manage
  g      Refresh          the whole view
  D      Clear delivered  every delivered panel
                          Asks before removing a workspace, checkout or branch it still holds.
? keys here · q quit

────────────────────────────────────────────────────────────
● RUNNING  delegate · ui refinement · 15m
    builder  codex  ● running
      Branch     pstack/delegate/…
    Workspace  ~/.local/state/pstack-arena/…/
────────────────────────────────────────────────────────────
✓ DONE  how · ui refinement · 2h
    data-flow  codex        ✓ done
    risks      claude-code  ✓ done
    Follow-up  delivered 2h ago
```

The card heading carries the state, the shape and the lead, which is all
the user needs to choose. Members, branch and workspace follow because the
user acts on them; the follow-up is folded. The panel id appears only in
follow-up buffers and prompts that must name one panel.

## Checking a change

For each user-facing change, compare it with these rules and with at least
one other screen in this configuration, keep keys and their meanings
consistent across screens, and verify it in a real Emacs (point after a
refresh, Evil keys, folding) as well as in tests.
