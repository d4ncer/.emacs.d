# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads)
for issue tracking. Use `bd` commands instead of markdown TODOs.
See AGENTS.md for workflow details.

## Overview

This is a sophisticated Emacs configuration that requires Emacs 30+ and uses a
**modular architecture** with 22 focused modules. The configuration emphasizes
performance, evil-mode vim emulation, and extensive language support.

## Architecture

### Modular Structure

The configuration uses a modular architecture where `init.el` (120 lines) serves as a minimal
bootstrap that loads 22 domain-specific modules from `modules/`:

**Core & Infrastructure:**
- `mod-keybindings.el` - Leader key (SPC) bindings and General.el setup (loads first)
- `mod-core.el` - Core hooks system, transient hooks, lifecycle hooks
- `mod-core-extras.el` - Additional utilities (file management, system integration)

**User Interface:**
- `mod-evil.el` - Vim emulation (evil, evil-collection, evil-surround)
- `mod-completion.el` - Completion framework (vertico, consult, embark, corfu)
- `mod-ui.el` - Themes, fonts, visual enhancements
- `mod-windows.el` - Window management, display-buffer rules

**Editing & Navigation:**
- `mod-editor.el` - Editing utilities (puni, undo-fu, spell-fu, which-key)
- `mod-nav.el` - Navigation tools (avy, ace-window)

**Development Tools:**
- `mod-project.el` - Project management (projectile)
- `mod-git.el` - Version control (magit, git-timemachine, browse-at-remote)
- `mod-lsp.el` - Language servers (eglot, flymake, treesit-auto)
- `mod-format.el` - Code formatting (apheleia)
- `mod-debug.el` - Debugger configurations

**Applications:**
- `mod-dired.el` - File manager enhancements
- `mod-eshell.el` - Emacs shell configuration
- `mod-ai.el` - LLM integration (gptel with Claude Sonnet 4)

**Content:**
- `mod-languages.el` - All programming languages (Lisp, TS/JS, Python, Elixir, etc.)
- `mod-org.el` - Org mode (deferred), org-node, org-modern
- `mod-text.el` - Text modes, templates, PDF viewer
- `mod-docs.el` - Documentation systems (help, info, man, eldoc)
- `mod-misc.el` - Visual enhancements (paren, breadcrumb, hideshow)

**Full module list and details:** See `docs/MODULES.md`

### Core Files

- `init.el` - Bootstrap (120 lines): version check, elpaca setup, module loading
- `early-init.el` - Early initialization (UI, themes, native compilation)
- `elpaca-bootstrap.el` - Package manager bootstrap

### Directory Structure

**Active Directories:**
- `modules/` - 22 modular configuration files (NEW - core of architecture)
- `lisp/` - Custom utility libraries (+ prefixed helper files)
- `templates/` - File templates for auto-insert (*.eld files)
- `ligatures/` - Font ligature configurations
- `docs/` - Documentation (MODULES.md architecture reference)
- `codev/` - Development specs, plans, reviews (SPIDER-SOLO protocol)

### Active Files in `lisp/`

Custom utility libraries (14 files):
- `+load-incrementally.el` - Performance optimization system
- `+theme.el` - Theme management
- `+corelib.el` - Core macros and utilities
- `+window.el`, `+roam.el`, `+edit-cmds.el` - Custom functionality
- `+eshell.el`, `+pulsar.el`, `+avy.el`, `+evil-collection.el`, `+elisp.el` - Mode extensions
- `+file-templates.el` - Template system

### Key Architectural Patterns

- **Modular Loading**: Each module is explicitly loaded via `(require 'mod-name)` in init.el
- **Load Order**: Keybindings load first (General.el setup), then core, then features
- **Transient Hooks**: `+first-input-hook`, `+first-file-hook`, `+first-buffer-hook` for deferred loading
- **Custom Lifecycle Hooks**: `+switch-buffer-hook`, `+switch-frame-hook`, `+switch-window-hook`
- **Expensive Package Tracking**: `+expensive-packages` list prevents accidental eager loading (org, org-roam, forge)
- **Incremental Loading**: `+load-incrementally` system for performance

## Package Management

- **Primary**: Elpaca (bootstrapped via `elpaca-bootstrap.el`)
- **Integration**: use-package with elpaca integration
- Use `:ensure t` for elpaca packages
- Prefer `:after-call` for deferred loading to improve startup performance
- **Critical**: Never eagerly load `org`, `org-roam`, `org-agenda`, or `forge`

## Development Workflow

### Making Configuration Changes

1. **Module-specific changes**: Edit the appropriate module in `modules/`
   - Find the right module using `docs/MODULES.md` as reference
   - Example: Evil config → `modules/mod-evil.el`

2. **Custom utilities**: Create or edit files in `lisp/` with `+` prefix

3. **Keybindings**: Edit `modules/mod-keybindings.el` (loads first, contains all SPC bindings)

4. **New modules**:
   - Create in `modules/` following existing patterns
   - Add `(require 'mod-name)` to `init.el` in appropriate load order
   - Update `docs/MODULES.md`

5. **File templates**: Add to `templates/` directory (*.eld format)

6. **Testing**: Test changes incrementally due to dependency chain

### Development Commands

**General:**
- `SPC C` - compile
- `SPC p c` - projectile-compile-project
- `SPC p t` - projectile-test-project

**Language-Specific Testing:**
- **Go**: `SPC m t t` (function), `SPC m t p` (package), `SPC m t P` (nested)
- **Rust**: Use rust-hydra for `cargo-process-test`; default: `cargo build`
- **Python**: `SPC m t` runs `python -m unittest`
- **JavaScript/TypeScript**: `SPC m t` runs `jest`
- **Docker**: `SPC m b` (build), `SPC m t` (test)

### Key Bindings System

- **Leader Key**: `SPC` in normal/motion states, `C-SPC` in insert mode
- **Local Leader**: `,` for mode-specific commands
- Uses General.el (configured in `mod-keybindings.el`)
- Include `:wk` descriptions for which-key integration

## Coding Standards

### File Headers

Always include lexical binding header:
```elisp
;;; filename.el --- Description -*- lexical-binding: t; -*-
```

### Naming Conventions

- Use `+` prefix for all custom functions, variables, and expressions
- Use `rk-` prefix for personal configuration modules (legacy, avoid in new code)
- Module names: `mod-<domain>.el` (e.g., `mod-completion.el`)
- Keep function names descriptive and namespace-aware

### Module Structure

Follow this template for new modules:
```elisp
;;; mod-name.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; Detailed description of what this module provides.
;; List major packages included.

;;; Code:

;; Require helper libraries if needed
(eval-and-compile
  (require '+corelib))  ; If using custom macros

;; Package configurations
(use-package some-package
  :ensure t
  :config
  ...)

(provide 'mod-name)
;;; mod-name.el ends here
```

### Package Configuration

- Use `use-package` with elpaca integration
- Prefer `:after-call` over `:hook` for better performance
- Always include `:commands` for autoloaded functions
- Use `:preface` for helper functions needed at compile time

### Performance Considerations

- **Never load expensive packages eagerly**: `org`, `org-roam`, `org-agenda`, `forge`
- Use transient hooks for one-time initialization
- Leverage the custom hook system for deferred loading
- Org mode uses `:after-call +first-file-hook` to stay deferred
- Set `gc-cons-threshold` appropriately in performance-critical code

## Theme and UI

- Uses Modus themes with custom `+theme` system
- JetBrainsMono Nerd Font for code, Helvetica Neue for variable-pitch
- Nano-emacs inspired minimal grayscale aesthetic
- Custom frame parameters for padding and borders
- Configuration in `mod-ui.el`

## Key Libraries in Use

- **Evil**: Vim emulation (`mod-evil.el`)
- **General**: Keybinding framework (`mod-keybindings.el`)
- **Org + org-node**: Note-taking and knowledge management (`mod-org.el`)
- **Eglot**: LSP client (`mod-lsp.el`) - preferred over lsp-mode
- **Projectile**: Project management (`mod-project.el`)
- **Magit**: Git interface (`mod-git.el`)
- **Which-key**: Key discovery (`mod-editor.el`)
- **Avy**: Jump navigation (`mod-nav.el`)

## Language Support

Language support is configured in `modules/mod-languages.el`:
- **Lisp/Elisp**: Enhanced indentation, eval utilities, prettify-symbols
- **TypeScript/JavaScript**: eglot, sibling file rules for tests
- **Python, Elixir, Erlang**: Custom LSP configurations
- **Data formats**: JSON, YAML (with eglot)
- **Tree-sitter**: 11 mode remappings to tree-sitter equivalents
- All languages use eglot for LSP integration

## Major Package Categories

1. **Package Management**: Elpaca, use-package integration
2. **Evil/Vim Emulation**: evil, evil-collection, evil-surround, evil-escape
3. **Completion**: vertico, marginalia, consult, embark, corfu, cape
4. **UI Enhancement**: nano-modeline, ligatures, pulsar, hl-todo, indent-bars
5. **Programming**: eglot, flymake, treesit-auto, apheleia
6. **Navigation**: avy, ace-window, projectile
7. **Git**: magit, git-timemachine, browse-at-remote
8. **AI Integration**: gptel (Claude Sonnet 4)
9. **File Management**: dired, dirvish, diredfl
10. **Org Mode**: org-node, org-modern

## Codev Methodology

This project uses the Codev context-driven development methodology.

### Active Protocol

- Protocol: SPIDER-SOLO (single-agent variant, Zen MCP not available)
- Location: codev/protocols/spider-solo/protocol.md

### Directory Structure

- Specifications: codev/specs/
- Plans: codev/plans/
- Reviews: codev/reviews/
- Resources: codev/resources/
- Protocol templates: codev/protocols/spider-solo/templates/

### Usage

Follow the SPIDER-SOLO protocol for all development tasks:
1. Create specifications in codev/specs/
2. Generate plans in codev/plans/
3. Conduct reviews in codev/reviews/
4. Reference protocol at codev/protocols/spider-solo/protocol.md

## Git Commit Message Style

### Format Structure

```
[scope] Short imperative summary

Optional detailed explanation (separated by blank line)
- Bullet points for multiple changes
- Additional context or reasoning
```

### Rules

**Scope Prefix:**
- Always use `[scope]` prefix in **lowercase**
- Common scopes:
  - `[refactor]` - Major structural changes
  - `[misc]` - Miscellaneous/cleanup
  - Package/feature names: `[gptel]`, `[eldoc-box]`, `[eglot]`, etc.
  - Domain areas: `[ai]`, `[core]`, `[search]`, `[display]`, `[init]`, `[help]`, `[evil]`

**Summary Line:**
- **Imperative mood**: "Add", "Fix", "Clean up", "Remove", "Setup", "Turn on"
- **Lowercase** after scope prefix
- **Concise** - usually < 50 chars total
- **No period** at end

**Body (Optional):**
- **Blank line** after summary
- **Bullet points** (`-`) for multiple items
- **Casual/conversational tone** acceptable
- Context about tools/collaborators when relevant
- Brief explanation of why/what (when non-obvious)

**What NOT to Include:**
- ❌ NO "🤖 Generated with Claude Code" footers
- ❌ NO "Co-Authored-By" trailers
- ❌ NO formal/corporate tone

**Examples:**

Simple:
```
[misc] Cleanup
```

With body:
```
[refactor] Refactor config into modules

- Use codev's SPIDER SOLO protocol to spec, plan, and implement a refactor
- Split up config into modules to make init.el smaller and cleaner
- Spec, plan, and review of impl available in codev/
```

Casual tone acceptable:
```
[modeline] Fix flymake + modeline

SOMETIMES YOU JUST GOTTA WRITE CODE YOURSELF
```

# important-instruction-reminders

Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
