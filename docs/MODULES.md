# Emacs Configuration Module Architecture

This document describes the modular architecture of this Emacs configuration after the Phase 3 refactoring (Spec 0002).

## Overview

The configuration has been refactored from a monolithic 3,042-line `init.el` into 22 focused modules, reducing `init.el` to just 120 lines (96% reduction). Each module contains related functionality and is loaded in a specific order to manage dependencies.

## Module Loading Order

Modules are loaded in the following order from `init.el`:

1. **mod-keybindings.el** - Must load first to set up General.el and leader key
2. **mod-core.el** - Core infrastructure and custom hooks
3. **mod-evil.el** - Vim emulation
4. **mod-completion.el** - Completion framework
5. **mod-ui.el** - Themes and visual settings
6. **mod-windows.el** - Window management
7. **mod-editor.el** - Editing utilities
8. **mod-nav.el** - Navigation tools
9. **mod-project.el** - Project management
10. **mod-git.el** - Version control
11. **mod-lsp.el** - LSP and tree-sitter
12. **mod-format.el** - Code formatting
13. **mod-debug.el** - Debugging tools
14. **mod-dired.el** - File manager
15. **mod-eshell.el** - Emacs shell
16. **mod-ai.el** - AI/LLM integration
17. **mod-languages.el** - Programming languages
18. **mod-org.el** - Org mode (deferred)
19. **mod-misc.el** - Miscellaneous utilities
20. **mod-docs.el** - Documentation systems
21. **mod-text.el** - Text modes and templates
22. **mod-core-extras.el** - Additional core utilities

## Module Details

### mod-keybindings.el
**Purpose:** Leader key bindings and keybinding infrastructure

**Key Components:**
- General.el package setup
- SPC leader key configuration (all major keybindings)
- Local leader (`,`) macro definition
- Requires: `+window`, `+roam`, `+edit-cmds` helper libraries

**Why First:** Must load before other modules to establish the keybinding system that other modules may use.

### mod-core.el
**Purpose:** Core infrastructure and fundamental utilities

**Key Components:**
- Custom hook system (`+first-input-hook`, `+first-file-hook`, `+first-buffer-hook`)
- Lifecycle hooks (`+switch-buffer-hook`, `+switch-frame-hook`, `+switch-window-hook`)
- Which-key (key discovery)
- Performance optimization settings
- Requires: `+corelib` for custom macros

### mod-evil.el
**Purpose:** Vim emulation via Evil mode

**Key Components:**
- Evil core configuration
- Evil collection (vim bindings for built-in modes)
- Evil-surround, evil-escape, evil-iedit
- Custom evil integrations

### mod-completion.el
**Purpose:** Completion framework

**Key Components:**
- Vertico (minibuffer completion UI)
- Marginalia (completion annotations)
- Consult (enhanced commands)
- Embark (contextual actions)
- Corfu (in-buffer completion)
- Cape (completion-at-point extensions)
- Orderless (flexible matching)

### mod-ui.el
**Purpose:** Visual appearance and themes

**Key Components:**
- Modus themes with custom theme system
- Nano-modeline (minimal modeline)
- Font configuration (JetBrainsMono Nerd Font, Helvetica Neue)
- Ligatures support
- Pulsar (visual feedback)
- HL-todo (TODO/FIXME highlighting)
- Indent-bars (indentation guides)

### mod-windows.el
**Purpose:** Window and buffer management

**Key Components:**
- Window manipulation utilities
- Buffer display rules
- Popper (popup management)
- Custom window commands from `+window` library

### mod-editor.el
**Purpose:** Text editing utilities

**Key Components:**
- Puni (structured editing)
- Multiple-cursors
- Expand-region
- Visual-regexp
- Spell-fu (spell checking)
- Whitespace management
- Undo-fu (linear undo)
- ws-butler (smart whitespace trimming)

### mod-nav.el
**Purpose:** Navigation tools

**Key Components:**
- Avy (jump to visible text)
- Ace-window (window selection)
- Custom avy actions from `+avy` library
- Pulsar integration for visual feedback

### mod-project.el
**Purpose:** Project management

**Key Components:**
- Projectile configuration
- Custom project switch action
- Integration with magit/dired

### mod-git.el
**Purpose:** Git and version control

**Key Components:**
- Transient (command menus)
- Magit (Git porcelain)
- Git-timemachine (file history browsing)
- Browse-at-remote (GitHub integration)
- VC (built-in version control)
- Nano-modeline integration for git-timemachine

### mod-lsp.el
**Purpose:** Language Server Protocol and syntax tools

**Key Components:**
- Eglot (LSP client)
- Flymake (on-the-fly syntax checking)
- Flymake-posframe (error display)
- Treesit-auto (tree-sitter mode setup)
- Eldoc-box (documentation display)

### mod-format.el
**Purpose:** Code formatting

**Key Components:**
- Apheleia (async formatting)
- Format-on-save configuration

### mod-debug.el
**Purpose:** Debugging tools

**Key Components:**
- Debug package configuration
- Custom debugger UI with header-line keybindings

### mod-dired.el
**Purpose:** File manager

**Key Components:**
- Dired and dired-aux
- Wdired (editable dired)
- Dirvish (modern dired UI, disabled)
- Nerd-icons-dired (icons, disabled)
- Diredfl (font-lock for dired)

### mod-eshell.el
**Purpose:** Emacs shell

**Key Components:**
- Eshell configuration
- Custom eshell library (`+eshell`)

### mod-ai.el
**Purpose:** AI/LLM integration

**Key Components:**
- GPTel (LLM client for Claude Sonnet 4)
- Custom helper functions:
  - `+gptel-explain-code`
  - `+gptel-explain-emacs-error`
  - `+gptel-request-with-buffer` macro
- Visual-line mode and markdown integration
- Pulsar feedback for LLM requests

### mod-languages.el
**Purpose:** Programming language configurations (consolidated)

**Key Components:**
- **Lisp/Elisp:** Enhanced indentation, eval utilities, prettify-symbols
- **Data formats:** YAML, JSON (with eglot)
- **TypeScript/JavaScript:** eglot, sibling file rules
- **Configuration:** conf-mode with eldoc
- **Markdown:** Code block fontification
- **Elixir/Erlang:** Custom LSP setup, test/impl navigation
- **Tree-sitter remapping:** 11 mode remappings to tree-sitter equivalents
- Requires: `+corelib`, `+elisp` libraries

### mod-org.el
**Purpose:** Org mode and knowledge management

**Key Components:**
- Org (deferred with `:after-call +first-file-hook`)
- Org-node (knowledge management)
- Org-modern (modern styling)

**Critical:** Org remains deferred to preserve startup performance.

### mod-misc.el
**Purpose:** Miscellaneous visual enhancements

**Key Components:**
- Hideshow (code folding)
- Page-break-lines (^L display)
- Hide-mode-line (conditional modeline hiding)
- Paren (show-paren-mode)
- Paren-face (parenthesis dimming)
- Breadcrumb (navigation breadcrumbs)

### mod-docs.el
**Purpose:** Documentation and help systems

**Key Components:**
- Help, help-fns, help-mode
- Eldoc (inline documentation)
- Info (texinfo manual reader)
- Man (manpage reader)
- Imenu (buffer navigation)

### mod-text.el
**Purpose:** Text modes and file templates

**Key Components:**
- Text-mode configuration
- Tempel (snippet system)
- Autoinsert (file template insertion)
- Custom file templates (`+file-templates`)
- PDF-tools (PDF viewer with evil bindings)

### mod-core-extras.el
**Purpose:** Additional core utilities and system integration

**Key Components:**
- **File management:** files, tramp, uniquify, find-sibling-rules
- **Performance:** bidi settings, scrolling, redisplay
- **Profiler:** Custom profiler utilities
- **System integration:** server, envrc, exec-path-from-shell, mise
- **Utilities:** goto-addr, better-jumper (disabled), string-inflection, so-long
- **Search:** grep, wgrep, xref
- **Built-ins:** paragraphs, indent, replace, proced
- **Input decode map:** C-i/C-m configuration
- **Trusted content:** Security paths

## Design Patterns

### Deferred Loading
Many packages use deferred loading patterns:
- `:after-call +first-file-hook` - Load on first file
- `:after-call +first-buffer-hook` - Load on first buffer
- `:after-call +first-input-hook` - Load on first input
- `:hook` - Load on specific mode activation

### Custom Hooks
The configuration uses custom lifecycle hooks defined in `mod-core.el`:
- `+first-input-hook` - Runs once on first user input
- `+first-file-hook` - Runs once when first file is opened
- `+first-buffer-hook` - Runs once when first buffer is created
- `+switch-buffer-hook` - Runs when switching buffers
- `+switch-frame-hook` - Runs when switching frames
- `+switch-window-hook` - Runs when switching windows

### Helper Libraries
Several `lisp/+*.el` helper libraries provide utilities:
- `+corelib.el` - Core macros and utilities
- `+window.el` - Window management functions
- `+roam.el` - Org-roam utilities
- `+edit-cmds.el` - Editing commands
- `+eshell.el` - Eshell extensions
- `+avy.el` - Avy custom actions
- `+evil-collection.el` - Evil integrations
- `+elisp.el` - Elisp utilities
- `+file-templates.el` - Template system
- `+pulsar.el` - Pulsar extensions
- `+theme.el` - Theme management

### Macro Availability
Modules using custom macros from `+corelib` (like `delq!`, `add-hook!`, `setq-hook!`) include:
```elisp
(eval-and-compile
  (require '+corelib))
```

## init.el Structure

The final `init.el` is minimal and focused:

```elisp
;; 1. Version check (Emacs 30+ required)
;; 2. Elpaca bootstrap
;; 3. Load-path setup (lisp/ and modules/)
;; 4. Module loading (22 requires)
;; 5. Post-init setup:
;;    - keychain-environment
;;    - Startup time reporting
;;    - private-config (if exists)
```

## Maintenance Guidelines

### Adding New Configuration
1. Identify the appropriate module or create a new one
2. Keep modules under 500 lines (ideally < 300)
3. Use `:after-call` for deferred loading when possible
4. Update this document when adding modules

### Module Naming Convention
- Prefix: `mod-`
- Name: Descriptive of domain (e.g., `mod-git`, `mod-completion`)
- File: `modules/mod-<name>.el`

### Load Order Considerations
- Keybindings must load first (General.el dependency)
- Core infrastructure loads early (provides hooks and utilities)
- Deferred modules (like org) can load late
- Dependencies matter: if module A uses features from module B, B loads first

## Performance Characteristics

### Startup Optimization
- Expensive packages (org, org-roam) are deferred
- Tree-sitter modes are auto-configured
- Custom hooks enable precise loading timing
- Native compilation is enabled (early-init.el)

### Incremental Loading
The `+load-incrementally` system (from `+load-incrementally.el`) allows packages to be loaded gradually during idle time, reducing perceived startup latency.

## Migration Notes

### From Monolithic init.el
This configuration was refactored from a 3,042-line monolithic `init.el` through 22 phases:
- Phases 0-6: Foundation (core, evil, completion, UI, windows, editor)
- Phases 7-17: Features (nav, project, git, LSP, format, debug, dired, eshell, AI, languages, org)
- Phases 18-22: Finalization (misc, docs, text, keybindings, core-extras)

### Bug Fixes During Refactoring
Several bugs were discovered and fixed:
1. **Error symbols:** Added missing `+corelib-hook-error` definition
2. **when-let* syntax:** Fixed multiple malformed bindings (requires double parentheses)
3. **Markdown predicate:** Added `fboundp` guard for `markdown-code-block-at-point-p`

## References

- **Specification:** `codev/specs/0002-modular-init-refactor.md`
- **Protocol:** SPIDER-SOLO (Codev methodology)
- **Helper libraries:** `lisp/+*.el`
- **Module directory:** `modules/`
