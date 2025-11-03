# Emacs Configuration Agent Guide

## Commands
- **Compilation/Build**: `SPC C` (compile), `SPC p c` (projectile-compile-project)
- **Tests**: `SPC p t` (projectile-test-project)
- **Go**: `SPC m t t` (current function), `SPC m t p` (package), `SPC m t P` (nested packages)
- **Rust**: Default compile: `cargo build`; use rust-hydra for `cargo-process-test`
- **Python**: `SPC m t` runs `python -m unittest`
- **JavaScript/TypeScript**: `SPC m t` runs `jest`
- **Docker**: `SPC m b` (build), `SPC m t` (test)

## Architecture
- **Package Manager**: Elpaca (successor to straight.el)
- **Core Structure**: `init.el` (main), `early-init.el` (early boot), `lisp/` (utilities), `config/` (feature configs)
- **Key Libraries**: Evil (vim emulation), General (keybindings), Org-mode + Org-roam (notes), Eglot (LSP)
- **Leader Key**: `SPC` in normal/motion, `C-SPC` in insert mode
- **Local Leader**: `,` for mode-specific commands

## Style Guidelines
- **Lexical Binding**: Always use `;;; -*- lexical-binding: t; -*-`
- **Naming**: Use `+` prefix for all custom expressions
- **Package Declaration**: Use `:ensure t` with elpaca, prefer `:after-call` for deferred loading
- **Keybindings**: Define through General, use `which-key` descriptions (`:wk`)
- **Hooks**: Use custom hooks like `+first-file-hook`, `+first-buffer-hook` for performance
- **Comments**: Minimal code comments, prefer docstrings for functions
- **Formatting**: 80-column width, no tabs (spaces only)

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for AL

### Why bd?

- Dependency-aware: Track blockers and relationships b
- Git-friendly: Auto-syncs to JSONL for version contro
- Agent-optimized: JSON output, ready work detection,
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --j
bd create "Issue title" -p 1 --deps discovered-from:bd
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked is
2. **Claim your task**: `bd update <id> --status in_pr
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-fro
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issu

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s d
- Imports from JSONL when newer (e.g., after `git pull
- No manual export/import needed!

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI comm

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and QUICKSTART.md.
