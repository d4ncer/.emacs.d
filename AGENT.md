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
