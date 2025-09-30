# Plan: Modular Init.el Refactor

## Metadata
- **ID**: 0002-modular-init-refactor
- **Status**: draft
- **Specification**: [codev/specs/0002-modular-init-refactor.md](../specs/0002-modular-init-refactor.md)
- **Created**: 2025-09-30

## Executive Summary

This plan implements **Approach 2: Incremental Module Extraction** from the specification. We will extract configuration from init.el into 26 focused modules, one phase at a time, maintaining a working Emacs configuration throughout.

The approach prioritizes safety through incremental validation. Each phase extracts 2-4 related modules, replaces that section in init.el with `(require 'mod-*)` statements, tests the configuration, and commits before moving to the next phase. This ensures Emacs remains usable at every step.

The extraction order follows the dependency chain: core infrastructure first, then foundational packages (evil, completion, UI), followed by feature-specific modules (LSP, git, org), and finally keybindings which reference all packages.

## Success Metrics

From specification:
- [ ] All functionality from init.el preserved in modules
- [ ] init.el reduced to < 100 lines (from 3,042 lines)
- [ ] Each module is < 500 lines (ideally < 300, except mod-keys.el at ~450)
- [ ] Emacs starts without errors
- [ ] All existing keybindings work identically
- [ ] No performance regression (startup time within 5% of current)
- [ ] Each module has clear header documentation
- [ ] Legacy config/ and hacks/ directories can be safely removed
- [ ] All tests pass (byte-compilation succeeds for all modules)
- [ ] Private config override works (~/private loads after all modules)
- [ ] Startup time message displays correctly

Implementation-specific metrics:
- [ ] Each phase completed with zero errors
- [ ] Each module byte-compiles without warnings
- [ ] Git history preserved with detailed commit messages
- [ ] Each phase takes < 30 minutes to complete and validate

## Phase Breakdown

### Phase 0: Preparation and Baseline
**Dependencies**: None
**Status**: pending

#### Objectives
- Establish baseline measurements for comparison
- Create modules/ directory structure
- Back up current configuration
- Document current startup time and behavior

#### Deliverables
- [ ] modules/ directory created
- [ ] Baseline startup time recorded
- [ ] Backup of current init.el
- [ ] Git branch created: `spider/0002-modular-init-refactor/phase-0-prep`

#### Implementation Details

1. Create modules directory:
   ```bash
   mkdir -p ~/.emacs.d/modules
   ```

2. Measure baseline startup time:
   ```bash
   emacs --batch --eval "(message \"Startup time: %.3fs\" (float-time (time-subtract after-init-time before-init-time)))"
   ```

3. Create git branch:
   ```bash
   git checkout -b spider/0002-modular-init-refactor/phase-0-prep
   ```

4. Add modules/ to .gitignore temporarily (remove after first module created):
   ```
   # We'll track modules individually as they're created
   ```

5. Add modules/ to load-path in init.el (at top, after require +corelib):
   ```elisp
   (add-to-list 'load-path (file-name-concat user-emacs-directory "modules/"))
   ```

#### Acceptance Criteria
- [ ] modules/ directory exists
- [ ] Baseline startup time documented in commit message
- [ ] Current init.el works identically
- [ ] Git branch created

#### Test Plan
- **Manual Testing**: Launch Emacs, verify no errors, verify startup time displayed

#### Rollback Strategy
Delete modules/ directory and checkout main branch.

#### Risks
- **Risk**: Accidentally modifying init.el during prep
  - **Mitigation**: Only add load-path, no other changes

---

### Phase 1: Core Infrastructure
**Dependencies**: Phase 0
**Status**: pending

#### Objectives
- Extract core settings, hooks system, and package manager setup
- Establish foundation that all other modules depend on

#### Deliverables
- [ ] mod-core.el created (~200 lines)
- [ ] mod-packages.el created (~20 lines)
- [ ] init.el updated with requires
- [ ] Tests pass: Emacs starts, hooks work, elpaca works

#### Implementation Details

**mod-core.el** extracts from init.el:
- Lines 11-15: GC threshold, emacs-start-time
- Lines 21-24: +lisp-dir, load-path, require +corelib and +load-incrementally
- Lines 26-32: Directory variables (org-directory, +cloud-dir, etc.)
- Lines 39-45: Expensive package check
- Lines 47-98: Custom hooks system (transient and lifecycle hooks)
- Lines 100-145: Escape key system
- Lines 518-599: Basic editing settings (fill-column, scrolling, cursor, minibuffer, etc.)

**mod-packages.el** extracts from init.el:
- Lines 36-37: elpaca-use-package setup

Note: elpaca-bootstrap.el remains a separate file (loaded before modules).

**init.el modifications**:
```elisp
;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

;; Bootstrap elpaca package manager
(load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

;; Load modules
(add-to-list 'load-path (file-name-concat user-emacs-directory "modules/"))

(require 'mod-core)
(require 'mod-packages)

;; [REST OF INIT.EL REMAINS FOR NOW]
```

#### Acceptance Criteria
- [ ] mod-core.el provides 'mod-core
- [ ] mod-packages.el provides 'mod-packages
- [ ] Emacs starts without errors
- [ ] +first-input-hook works (test by adding a hook)
- [ ] Expensive package check works (verify warning if org loaded)
- [ ] Escape key works (test ESC in various contexts)
- [ ] Byte-compilation succeeds for both modules

#### Test Plan
- **Unit Tests**:
  - Test that +first-input-hook runs on first keystroke
  - Test that +escape works in minibuffer
- **Integration Tests**:
  - Start Emacs and verify no errors
  - Check that org-directory is set correctly
- **Manual Testing**:
  - Press ESC in various contexts
  - Verify scrolling behavior
  - Check minibuffer cursor behavior

#### Rollback Strategy
Remove `(require 'mod-core)` and `(require 'mod-packages)` from init.el, restore extracted lines.

#### Risks
- **Risk**: Hooks system fails to initialize, breaking deferred loading
  - **Mitigation**: Test thoroughly, hooks are critical infrastructure

---

### Phase 2: Evil Mode
**Dependencies**: Phase 1
**Status**: pending

#### Objectives
- Extract all evil mode configuration into single module
- Ensure vim emulation works identically

#### Deliverables
- [ ] mod-evil.el created (~150-200 lines)
- [ ] init.el updated
- [ ] Evil mode works identically

#### Implementation Details

**mod-evil.el** extracts all evil-related use-package declarations:
- evil (core)
- evil-collection
- evil-surround
- evil-escape
- evil-goggles
- evil-numbers
- evil-visualstar
- evil-lion
- evil-nerd-commenter
- evil-org (will stay here, not in mod-org, as it's evil integration)

Search init.el for all `(use-package evil` patterns and extract.

**Conservative approach**: Extract exactly as-is, no modifications.

#### Acceptance Criteria
- [ ] mod-evil.el byte-compiles without warnings
- [ ] Evil mode activates on startup
- [ ] Evil keybindings work (test hjkl navigation)
- [ ] Evil-surround works (test cs"')
- [ ] Evil-collection integrations work (test in dired, magit)

#### Test Plan
- **Manual Testing**:
  - Open file, test vim motions (hjkl, w, b, etc.)
  - Test evil-surround (cs"')
  - Test evil modes in dired
  - Test visual mode selections

#### Rollback Strategy
Remove `(require 'mod-evil)`, restore evil configurations to init.el.

#### Risks
- **Risk**: Evil-collection integrations break
  - **Mitigation**: Test common modes (dired, magit) after extraction

---

### Phase 3: Completion Infrastructure
**Dependencies**: Phase 2
**Status**: pending

#### Objectives
- Extract completion system (vertico, consult, corfu, etc.)
- Ensure completion works in all contexts

#### Deliverables
- [ ] mod-completion.el created (~250-300 lines)
- [ ] Completion works identically

#### Implementation Details

**mod-completion.el** extracts:
- vertico
- vertico-posframe or vertico-buffer
- consult
- consult-*-xref variants
- embark
- embark-consult
- marginalia
- orderless
- corfu
- corfu-popupinfo
- cape
- completion-at-point configuration

#### Acceptance Criteria
- [ ] M-x (execute-extended-command) uses vertico
- [ ] consult-ripgrep works
- [ ] Completion-at-point works (corfu)
- [ ] Embark actions work (C-. or embark-act key)

#### Test Plan
- **Manual Testing**:
  - M-x and verify vertico interface
  - consult-ripgrep in project
  - Trigger completion in code buffer
  - Test embark-act on a completion candidate

#### Rollback Strategy
Remove `(require 'mod-completion)`, restore to init.el.

#### Risks
- **Risk**: Completion breaks in specific modes
  - **Mitigation**: Test in multiple major modes

---

### Phase 4: UI and Themes
**Dependencies**: Phase 3
**Status**: pending

#### Objectives
- Extract UI configuration (themes, fonts, visual enhancements)
- Ensure visual appearance is identical

#### Deliverables
- [ ] mod-ui.el created (~200-250 lines)
- [ ] Visual appearance unchanged

#### Implementation Details

**mod-ui.el** extracts:
- Theme system (modus themes, +theme utilities)
- Font configuration (JetBrainsMono, Helvetica Neue)
- ligature-mode
- nano-modeline
- pulsar (from lisp/+pulsar.el integration)
- hl-todo
- rainbow-delimiters
- indent-bars (if present)
- Frame parameters and visual settings
- Custom faces from lines 519-526

Load +theme.el and +pulsar.el from lisp/ as needed.

#### Acceptance Criteria
- [ ] Theme loads correctly
- [ ] Fonts render correctly (monospace for code, variable-pitch if used)
- [ ] Ligatures work (test ->, =>, etc.)
- [ ] Modeline displays correctly
- [ ] Custom faces applied (region highlight, etc.)

#### Test Plan
- **Manual Testing**:
  - Verify theme is applied
  - Check font rendering
  - Check ligatures in code
  - Verify modeline appearance
  - Test custom faces (select region, etc.)

#### Rollback Strategy
Remove `(require 'mod-ui)`, restore to init.el.

#### Risks
- **Risk**: Font configuration fails on startup
  - **Mitigation**: Fonts are already installed, just moving config

---

### Phase 5: Window Management
**Dependencies**: Phase 4
**Status**: pending

#### Objectives
- Extract window management and display-buffer configuration
- Ensure popup windows behave identically ("The Dark Pit")

#### Deliverables
- [ ] mod-windows.el created (~150-200 lines)
- [ ] display-buffer rules work identically

#### Implementation Details

**mod-windows.el** extracts from lines 2649-2810:
- display-buffer-alist configuration
- Side window rules (top, left, right, bottom)
- Window management functions from +window.el
- Window keybindings will be in mod-keys.el, but window utility functions here

Require +window.el from lisp/.

#### Acceptance Criteria
- [ ] Help buffers display in correct location
- [ ] Compilation buffers use side windows
- [ ] Debugger buffers display correctly
- [ ] Window dedication works

#### Test Plan
- **Manual Testing**:
  - Open help buffer (C-h f), verify placement
  - Run compilation, verify side window
  - Test window splitting and navigation

#### Rollback Strategy
Remove `(require 'mod-windows)`, restore display-buffer-alist to init.el.

#### Risks
- **Risk**: Display buffer rules are complex and could break popup placement
  - **Mitigation**: Test extensively with different buffer types

---

### Phase 6: Editor Utilities
**Dependencies**: Phase 5
**Status**: pending

#### Objectives
- Extract core editing utilities and enhancements
- Ensure editing experience is identical

#### Deliverables
- [ ] mod-editor.el created (~300-350 lines)
- [ ] All editing utilities work

#### Implementation Details

**mod-editor.el** extracts:
- puni (structural editing)
- undo-tree or undo-fu
- ws-butler (whitespace management)
- visual-fill-column
- expand-region
- auto-revert-mode
- saveplace
- savehist
- recentf
- ibuffer
- bookmark
- which-key
- helpful
- hydra (if present)
- highlight-thing (from "CHRIS CONFIG" lines 2813-2841)
- spell-fu / spell-checking

Require +edit-cmds.el from lisp/.

#### Acceptance Criteria
- [ ] Structural editing works (puni slurp, barf, etc.)
- [ ] Undo tree accessible
- [ ] which-key displays on partial key sequences
- [ ] helpful provides enhanced help
- [ ] highlight-thing highlights symbol at point
- [ ] File history preserved (saveplace, recentf)

#### Test Plan
- **Manual Testing**:
  - Test puni operations in elisp
  - Test undo/redo
  - Trigger which-key (press SPC and wait)
  - Test helpful-callable
  - Verify highlight-thing in prog-mode
  - Close/reopen file, verify point restored

#### Rollback Strategy
Remove `(require 'mod-editor)`, restore to init.el.

#### Risks
- **Risk**: highlight-thing configuration is complex
  - **Mitigation**: Extract carefully, test in multiple modes

---

### Phase 7: Navigation
**Dependencies**: Phase 6
**Status**: pending

#### Objectives
- Extract navigation tools (avy, ace-window, etc.)

#### Deliverables
- [ ] mod-nav.el created (~100-150 lines)

#### Implementation Details

**mod-nav.el** extracts:
- avy (require +avy.el from lisp/)
- ace-window
- winum (if present)
- link-hint

#### Acceptance Criteria
- [ ] Avy jump works (avy-goto-char, avy-goto-line)
- [ ] ace-window works
- [ ] Navigation feels identical

#### Test Plan
- **Manual Testing**:
  - Test avy-goto-char
  - Test ace-window switching

#### Rollback Strategy
Remove `(require 'mod-nav)`, restore to init.el.

#### Risks
- **Risk**: Low risk, navigation tools are independent

---

### Phase 8: Project Management
**Dependencies**: Phase 7
**Status**: pending

#### Objectives
- Extract projectile and project.el configuration

#### Deliverables
- [ ] mod-project.el created (~100-150 lines)

#### Implementation Details

**mod-project.el** extracts:
- projectile
- project.el enhancements
- Custom projectile configuration

#### Acceptance Criteria
- [ ] Projectile commands work (projectile-find-file, etc.)
- [ ] Project switching works
- [ ] Project compilation works

#### Test Plan
- **Manual Testing**:
  - Open project, test projectile-find-file
  - Test projectile-compile-project

#### Rollback Strategy
Remove `(require 'mod-project)`, restore to init.el.

#### Risks
- **Risk**: Projectile indexing issues
  - **Mitigation**: Test in existing project

---

### Phase 9: Git and Version Control
**Dependencies**: Phase 8
**Status**: pending

#### Objectives
- Extract magit and git-related packages

#### Deliverables
- [ ] mod-git.el created (~150-200 lines)

#### Implementation Details

**mod-git.el** extracts:
- magit
- magit-todos
- git-timemachine
- browse-at-remote
- git-link
- forge (MUST be deferred!)

#### Acceptance Criteria
- [ ] magit-status works
- [ ] magit operations work (stage, commit, push)
- [ ] git-timemachine works
- [ ] browse-at-remote works

#### Test Plan
- **Manual Testing**:
  - Open magit-status
  - Test staging and committing
  - Test git-timemachine on a file
  - Test browse-at-remote

#### Rollback Strategy
Remove `(require 'mod-git)`, restore to init.el.

#### Risks
- **Risk**: Forge might load eagerly
  - **Mitigation**: Ensure :after-call or :defer is used

---

### Phase 10: LSP and Tree-sitter
**Dependencies**: Phase 9
**Status**: pending

#### Objectives
- Extract eglot, flymake, and tree-sitter configuration

#### Deliverables
- [ ] mod-lsp.el created (~150-200 lines)

#### Implementation Details

**mod-lsp.el** extracts:
- eglot
- flymake
- treesit-auto
- tree-sitter mode associations
- eglot server configurations (Elixir elixir-ls, etc.)

#### Acceptance Criteria
- [ ] Eglot connects to language servers
- [ ] Flymake shows errors
- [ ] Tree-sitter modes activate

#### Test Plan
- **Manual Testing**:
  - Open TypeScript file, verify eglot connects
  - Introduce error, verify flymake highlights
  - Check tree-sitter mode active

#### Rollback Strategy
Remove `(require 'mod-lsp)`, restore to init.el.

#### Risks
- **Risk**: Language server connection issues
  - **Mitigation**: Test with known working language servers

---

### Phase 11: Code Formatting
**Dependencies**: Phase 10
**Status**: pending

#### Objectives
- Extract apheleia and formatting configuration

#### Deliverables
- [ ] mod-format.el created (~100 lines)

#### Implementation Details

**mod-format.el** extracts:
- apheleia
- Format-on-save configurations
- Formatter tool settings

#### Acceptance Criteria
- [ ] Apheleia formats on save
- [ ] Formatters work (prettier, black, rustfmt, etc.)

#### Test Plan
- **Manual Testing**:
  - Edit TypeScript file, save, verify prettier runs
  - Edit Python file, save, verify black runs

#### Rollback Strategy
Remove `(require 'mod-format)`, restore to init.el.

#### Risks
- **Risk**: Formatter not found
  - **Mitigation**: Formatters already installed, just moving config

---

### Phase 12: Debugging
**Dependencies**: Phase 11
**Status**: pending

#### Objectives
- Extract debugger configuration

#### Deliverables
- [ ] mod-debug.el created (~50-100 lines)

#### Implementation Details

**mod-debug.el** extracts:
- Debugger mode configurations
- dap-mode (if present)
- realgud (if present)
- Debugger display-buffer rules

#### Acceptance Criteria
- [ ] Debugger modes work
- [ ] Debugger buffers display correctly

#### Test Plan
- **Manual Testing**:
  - Test debugger if present

#### Rollback Strategy
Remove `(require 'mod-debug)`, restore to init.el.

#### Risks
- **Risk**: Low, debugger config might be minimal

---

### Phase 13: Dired
**Dependencies**: Phase 12
**Status**: pending

#### Objectives
- Extract dired enhancements

#### Deliverables
- [ ] mod-dired.el created (~100-150 lines)

#### Implementation Details

**mod-dired.el** extracts:
- dired enhancements
- dirvish (note spec says "dired" but init has "dirvish")
- dired-sidebar
- dired-subtree
- dired-x

#### Acceptance Criteria
- [ ] Dired opens and works
- [ ] Dired enhancements work (subtree, sidebar)

#### Test Plan
- **Manual Testing**:
  - Open dired
  - Test dired operations
  - Test dirvish if present

#### Rollback Strategy
Remove `(require 'mod-dired)`, restore to init.el.

#### Risks
- **Risk**: Low, dired is stable

---

### Phase 14: Eshell
**Dependencies**: Phase 13
**Status**: pending

#### Objectives
- Extract eshell configuration

#### Deliverables
- [ ] mod-eshell.el created (~50-100 lines)

#### Implementation Details

**mod-eshell.el** extracts:
- eshell
- eshell-prompt
- eshell-aliases
- Custom eshell utilities from +eshell.el

Require +eshell.el from lisp/.

#### Acceptance Criteria
- [ ] Eshell opens and works
- [ ] Custom prompt displays

#### Test Plan
- **Manual Testing**:
  - Open eshell
  - Run commands
  - Verify prompt

#### Rollback Strategy
Remove `(require 'mod-eshell)`, restore to init.el.

#### Risks
- **Risk**: Low, eshell is independent

---

### Phase 15: AI Integration
**Dependencies**: Phase 14
**Status**: pending

#### Objectives
- Extract gptel configuration

#### Deliverables
- [ ] mod-ai.el created (~100-150 lines)

#### Implementation Details

**mod-ai.el** extracts from lines 2910+:
- gptel
- gptel backends
- Custom gptel configuration

#### Acceptance Criteria
- [ ] gptel works
- [ ] LLM integration functional

#### Test Plan
- **Manual Testing**:
  - Open gptel buffer
  - Test sending message

#### Rollback Strategy
Remove `(require 'mod-ai)`, restore to init.el.

#### Risks
- **Risk**: Low, gptel is self-contained

---

### Phase 16-22: Language Modules (7 phases)
**Dependencies**: Phase 15
**Status**: pending

Each language module phase follows same pattern:

#### Phase 16: Lisp Languages
- [ ] mod-lang-lisp.el created

#### Phase 17: JavaScript/TypeScript
- [ ] mod-lang-js.el created

#### Phase 18: Data Formats
- [ ] mod-lang-data.el created

#### Phase 19: Web Languages
- [ ] mod-lang-web.el created

#### Phase 20: Systems Languages
- [ ] mod-lang-systems.el created

#### Phase 21: Functional Languages
- [ ] mod-lang-functional.el created

#### Phase 22: Misc Languages
- [ ] mod-lang-misc.el created

#### Implementation Details (All Language Modules)

Each extracts:
- Major mode packages
- Tree-sitter configuration
- Eglot server setup
- Language-specific packages
- Mode hooks

Require +elisp.el for mod-lang-lisp.el.

#### Acceptance Criteria (All)
- [ ] Module byte-compiles
- [ ] Major mode activates for file type
- [ ] Language server connects (if applicable)
- [ ] Syntax highlighting works

#### Test Plan (All)
- **Manual Testing**:
  - Open file of each language type
  - Verify major mode
  - Verify syntax highlighting
  - Verify LSP if applicable

#### Rollback Strategy (All)
Remove require, restore to init.el.

#### Risks (All)
- **Risk**: Language server configuration issues
  - **Mitigation**: Test with existing files

---

### Phase 23: Org Mode
**Dependencies**: Phase 22
**Status**: pending

#### Objectives
- Extract org-mode and org-node configuration (MUST BE DEFERRED!)

#### Deliverables
- [ ] mod-org.el created (~200-250 lines)

#### Implementation Details

**mod-org.el** extracts from lines 2843-2909:
- org-mode (DEFERRED with :after-call +first-file-hook)
- evil-org
- org-node (DEFERRED)
- org-modern
- org-agenda (DEFERRED)
- org-capture
- org-babel
- Custom org utilities from +roam.el

Require +roam.el from lisp/.

**CRITICAL**: Ensure org, org-node, and org-agenda are deferred!

**Conservative approach**: Keep variable-pitch-mode hook.
**Cleanup approach**: Remove variable-pitch-mode hook (user preference).

#### Acceptance Criteria
- [ ] Org mode DOES NOT load on startup (verify with expensive package check)
- [ ] Org mode loads when opening .org file
- [ ] org-node works
- [ ] org-modern styling applies
- [ ] Org keybindings work (defined in mod-keys.el later)

#### Test Plan
- **Unit Tests**:
  - Verify org not in (featurep 'org) after startup
- **Manual Testing**:
  - Start Emacs, verify org not loaded
  - Open .org file, verify org loads
  - Test org-node-find
  - Verify org-modern styling

#### Rollback Strategy
Remove `(require 'mod-org)`, restore to init.el.

#### Risks
- **Risk**: Accidentally loading org eagerly (HIGH IMPACT!)
  - **Mitigation**: Use expensive package check, test thoroughly

---

### Phase 24: Keybindings (LARGEST MODULE)
**Dependencies**: Phase 23
**Status**: pending

#### Objectives
- Extract ALL keybindings to single module (loads last!)

#### Deliverables
- [ ] mod-keys.el created (~400-500 lines)

#### Implementation Details

**mod-keys.el** extracts from lines 147-517:
- general.el setup (general-auto-unbind-keys)
- Leader key definition
- ALL SPC keybindings (files, buffers, windows, apps, help, git, project, org, code, search, toggles, LLM, errors, narrowing, structure, quit)
- Minibuffer keymaps
- +local-leader-set-key macro
- Universal argument chaining

Require +window.el, +roam.el, +edit-cmds.el from lisp/.

**IMPORTANT**: This module references functions from ALL other modules, so it MUST be loaded last.

#### Acceptance Criteria
- [ ] SPC works as leader key
- [ ] All SPC bindings work (test each category)
- [ ] C-SPC works in insert mode
- [ ] Minibuffer keybindings work
- [ ] Local leader (,) works in org-mode
- [ ] Universal argument chaining works (SPC u u)

#### Test Plan
- **Manual Testing** (comprehensive):
  - SPC f (files): test find, save, recent
  - SPC b (buffers): test switch, list
  - SPC w (windows): test split, delete, move
  - SPC p (project): test find-file, switch-project
  - SPC g (git): test magit-status, blame
  - SPC o (org): test find, agenda
  - SPC c (code): test find-references, find-definition
  - SPC l (LLM): test gptel
  - SPC / (search): test consult-ripgrep
  - Test all toggle bindings (SPC t)
  - Test local leader in org-mode (, n, , p, etc.)

#### Rollback Strategy
Remove `(require 'mod-keys)`, restore keybindings to init.el.

#### Risks
- **Risk**: Keybinding conflicts or missing bindings (HIGH IMPACT!)
  - **Mitigation**: Comprehensive testing of all binding categories

---

### Phase 25: Darwin (macOS)
**Dependencies**: Phase 24
**Status**: pending

#### Objectives
- Extract macOS-specific configuration

#### Deliverables
- [ ] mod-darwin.el created (~50-100 lines)

#### Implementation Details

**mod-darwin.el** extracts:
- exec-path-from-shell
- keychain-environment (from lines 3025-3028)
- Command key bindings (line 534: C-x SPC)
- macOS-specific utilities

**IMPORTANT**: keychain-environment loads here, after all modules, to ensure environment is set up.

#### Acceptance Criteria
- [ ] Only loads on macOS
- [ ] exec-path-from-shell works
- [ ] keychain-environment refreshes env
- [ ] macOS keybindings work

#### Test Plan
- **Manual Testing** (macOS only):
  - Verify PATH is set correctly
  - Test C-x SPC for emoji palette

#### Rollback Strategy
Remove `(require 'mod-darwin)`, restore to init.el.

#### Risks
- **Risk**: Low, macOS config is isolated

---

### Phase 26: Final Cleanup and Init.el Finalization
**Dependencies**: Phase 25
**Status**: pending

#### Objectives
- Finalize init.el to minimal bootstrap
- Remove all legacy config
- Verify complete system

#### Deliverables
- [ ] init.el reduced to < 100 lines
- [ ] config/ and hacks/ directories deleted
- [ ] All modules documented
- [ ] Complete system validation

#### Implementation Details

**Final init.el structure**:
```elisp
;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi
;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:
;; Modular Emacs configuration.
;; Individual features are organized in modules/ directory.

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

;; Bootstrap elpaca package manager
(load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

;; Load modules
(add-to-list 'load-path (file-name-concat user-emacs-directory "modules/"))

;; Core infrastructure (must be first)
(require 'mod-core)
(require 'mod-packages)

;; Evil and completion (foundational)
(require 'mod-evil)
(require 'mod-completion)

;; UI and window management
(require 'mod-ui)
(require 'mod-windows)

;; Editor utilities
(require 'mod-editor)
(require 'mod-nav)

;; Development tools
(require 'mod-project)
(require 'mod-git)
(require 'mod-lsp)
(require 'mod-format)
(require 'mod-debug)

;; Applications
(require 'mod-dired)
(require 'mod-eshell)
(require 'mod-ai)

;; Language support
(require 'mod-lang-lisp)
(require 'mod-lang-js)
(require 'mod-lang-data)
(require 'mod-lang-web)
(require 'mod-lang-systems)
(require 'mod-lang-functional)
(require 'mod-lang-misc)

;; Org mode (deferred)
(require 'mod-org)

;; Keybindings (must be after all packages)
(require 'mod-keys)

;; Platform-specific (conditional)
(when (equal system-type 'darwin)
  (require 'mod-darwin))

;;; Post init setup

;; Print startup time
(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed)))

;; Private configuration override (optional)
(use-package private-config
  :when (file-directory-p "~/private")
  :load-path "~/private")

(provide 'init)
;;; init.el ends here
```

**Cleanup actions**:
1. Delete config/ directory (old rk-*.el files)
2. Delete hacks/ directory (old workarounds)
3. Byte-compile all modules
4. Run full system validation

#### Acceptance Criteria
- [ ] init.el is < 100 lines
- [ ] config/ and hacks/ deleted
- [ ] All 26 modules byte-compile without warnings
- [ ] Emacs starts without errors
- [ ] All keybindings work
- [ ] Startup time within 5% of baseline
- [ ] Expensive package check passes (org, org-node, forge not loaded)
- [ ] Private config override works

#### Test Plan
- **Comprehensive System Test**:
  - Fresh Emacs start, no errors
  - Test representative workflows:
    - Open project, find file, edit, save
    - Git operations (status, commit)
    - LSP operations (find-references, jump-to-definition)
    - Org operations (open org file, org-node-find)
    - LLM operations (gptel)
  - Verify expensive packages not loaded at startup
  - Compare startup time to baseline

#### Rollback Strategy
Full git history available, can rollback to any phase.

#### Risks
- **Risk**: Subtle behavior differences discovered
  - **Mitigation**: Comprehensive testing, user validation

---

## Dependency Map

```
Phase 0 (Prep)
    ↓
Phase 1 (Core + Packages)
    ↓
Phase 2 (Evil)
    ↓
Phase 3 (Completion)
    ↓
Phase 4 (UI)
    ↓
Phase 5 (Windows)
    ↓
Phase 6 (Editor)
    ↓
Phase 7 (Nav)
    ↓
Phase 8 (Project)
    ↓
Phase 9 (Git)
    ↓
Phase 10 (LSP)
    ↓
Phase 11 (Format)
    ↓
Phase 12 (Debug)
    ↓
Phase 13 (Dired)
    ↓
Phase 14 (Eshell)
    ↓
Phase 15 (AI)
    ↓
Phase 16-22 (Languages, parallel possible but serial safer)
    ↓
Phase 23 (Org - CRITICAL: must be deferred!)
    ↓
Phase 24 (Keys - MUST be last before darwin!)
    ↓
Phase 25 (Darwin)
    ↓
Phase 26 (Cleanup)
```

## Resource Requirements

### Development Resources
- **Engineers**: Solo developer (user + Claude Code)
- **Environment**: Local Emacs 30+ installation
- **Tools**: Git, byte-compiler, Elpaca

### Infrastructure
- No external infrastructure needed
- All changes local to ~/.emacs.d/

## Integration Points

### External Systems
None - this is a local configuration refactor.

### Internal Systems
- **lisp/ utilities**: Modules will require files from lisp/ directory
  - +corelib.el
  - +load-incrementally.el
  - +theme.el
  - +window.el
  - +roam.el
  - +edit-cmds.el
  - +eshell.el
  - +pulsar.el
  - +avy.el
  - +evil-collection.el
  - +elisp.el

## Risk Analysis

### Technical Risks
| Risk | Probability | Impact | Mitigation | Owner |
|------|------------|--------|------------|-------|
| Breaking core hooks system | Low | High | Phase 1 tested thoroughly, hooks are critical | User |
| Evil keybindings break | Low | High | Phase 2 tested with common vim operations | User |
| Org loads eagerly | Medium | High | Use expensive package check, test Phase 23 carefully | User |
| Keybindings conflict/missing | Medium | High | Comprehensive testing in Phase 24 | User |
| Subtle behavior changes | Medium | Medium | Incremental approach, test at each phase | User |
| Performance regression | Low | Medium | Measure startup time at Phase 0 and Phase 26 | User |

### Schedule Risks
| Risk | Probability | Impact | Mitigation | Owner |
|------|------------|--------|------------|-------|
| Phase takes longer than expected | Low | Low | Can pause/resume at any phase | User |
| Discovery of unexpected config | Medium | Low | Handle in appropriate phase or add new phase | User |
| Interruption during phase | Low | Low | Each phase is atomic, can resume | User |

## Validation Checkpoints

1. **After Phase 1 (Core)**: Verify hooks system works, expensive package check works
2. **After Phase 2 (Evil)**: Verify vim emulation identical
3. **After Phase 6 (Editor)**: Verify editing experience unchanged
4. **After Phase 10 (LSP)**: Verify language server connections work
5. **After Phase 23 (Org)**: CRITICAL - Verify org NOT loaded at startup
6. **After Phase 24 (Keys)**: Verify ALL keybindings work
7. **After Phase 26 (Final)**: Complete system validation

## Monitoring and Observability

### Metrics to Track
- **Startup time**: Measure at Phase 0 (baseline) and Phase 26 (final)
  - Threshold: < 5% regression
- **Module count**: Track as modules are created (target: 26)
- **init.el line count**: Track reduction (start: 3,042, target: < 100)
- **Byte-compilation warnings**: Should be zero for all modules

### Logging Requirements
- Git commit messages document each phase
- Each commit includes what was extracted and where it went
- Note any deviations from plan in commit messages

### Alerting
- Expensive package check will warn if org/org-node/forge loaded eagerly
- Byte-compilation errors will prevent phase completion

## Documentation Updates Required

- [x] Specification complete (codev/specs/0002-modular-init-refactor.md)
- [ ] This plan (codev/plans/0002-modular-init-refactor.md)
- [ ] Each module header documents its purpose and packages
- [ ] CLAUDE.md updated with new module structure
- [ ] README.md updated if needed

## Post-Implementation Tasks

- [ ] Measure final startup time, compare to baseline
- [ ] Byte-compile all modules
- [ ] Test loading Emacs with emacs -Q then loading init.el
- [ ] Verify private config override works
- [ ] Document any discovered issues in review
- [ ] Update CLAUDE.md with module structure

## Approval

- [ ] User Review - Awaiting user feedback on this plan
- [ ] Proceed to Implementation (Phase 0)

## Change Log

| Date | Change | Reason | Author |
|------|--------|--------|--------|
| 2025-09-30 | Initial plan draft | Created from approved specification | Claude |

## Notes

### Phase Execution Protocol

For each phase:
1. Create git branch: `spider/0002-modular-init-refactor/phase-N-name`
2. Create module file(s)
3. Extract configuration from init.el
4. Update init.el with require statement(s)
5. Test thoroughly (see phase test plan)
6. Byte-compile module(s)
7. Commit with message: `[Spec 0002][Phase N: name] type: description`
8. Verify expensive package check (for phases touching org/forge)
9. Get user approval before proceeding to next phase

### Conservative vs Cleanup Approach

By default, use **conservative approach** (1:1 extraction, no changes).

For cleanup opportunities, ask user before making changes. Cleanup can be done in separate follow-up commits after conservative extraction succeeds.

### Critical Success Factors

1. **Core infrastructure (Phase 1)** must work perfectly - everything depends on it
2. **Org mode (Phase 23)** must be deferred - use expensive package check
3. **Keybindings (Phase 24)** must be comprehensive - test all categories
4. **Incremental validation** - test at every phase, don't batch

### Time Estimates

- Each phase: 15-30 minutes (create module, extract, test, commit)
- Total estimated time: 8-12 hours over multiple sessions
- Can be paused/resumed at any phase boundary