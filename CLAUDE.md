# CLAUDE.md

## Overview

This is an Emacs configuration split into modules under the `lisp/` directory.

The setup focuses on:

- Writing prose (Markdown, Org)
- Front-end development
  - JavaScript and TypeScript
  - JSX and TSX
  - CSS, Tailwind, and SASS
  - Astro

## Architecture

Bootstrap sequence:

1. `early-init.el` - Platform-specific setup (macOS dark mode, frame settings, disabling `package-enable-at-startup`)
2. `init.el` - Thin loader that adds `lisp/init/` to `load-path` and `require`s each module
3. `lisp/init/init-*.el` - Configuration modules (explore `lisp/init/` to see the current set)

`init-package` and `init-essentials` must load first since other modules depend on their variables. The remaining modules are mostly independent of each other.

## Making changes

Edit the appropriate module in `lisp/init/`. Each file is named `init-<topic>.el` and covers a specific area of the configuration. Use `;;;` and `;;;;` comment headings within modules to organize subsections.

The configuration uses `use-package` for package management with MELPA as the package repository.

## Conventions

**Sentence-case for all headings and prose** — use "Daily notes and journal entries", not "Daily Notes and Journal Entries".

**Custom variables and functions use the `s3thi/` prefix:**

- `s3thi/is-a-mac`, `s3thi/is-a-pc`, `s3thi/is-a-linux` - Platform detection
- `s3thi/prose-line-spacing` - Line spacing for prose modes
- `s3thi/prose-header-scaling` - Header font sizes

## Key directories

- `lisp/init/` - Configuration modules (`init-*.el`)
- `elpa/` - Installed packages (gitignored)
- `snippets/` - Yasnippet templates
- `auto-save/` - Auto-save files
- `undo-tree/` - Undo history
- `custom.el` - Auto-generated custom settings (gitignored)

## Major features

- **Completion:** Vertico + Orderless + Marginalia
- **Prose editing:** visual-fill-column, mixed-pitch, jinx (spell check), centered-cursor-mode
- **Programming:** Flymake, Eglot
- **Git:** Magit

## Version control

This project uses the Jujutsu version control system (`jj`) instead of Git. **Never run any Git commands.** Always use `jj` commands for VCS operations. If you're not sure how to accomplish something using `jj`, stop and ask the user.

Common operations:

- `jj status` — show working copy changes
- `jj log` — show commit history
- `jj describe -m "message"` — set description on the current change
- `jj new` — create a new empty change on top of the current one
- `jj file untrack <path>` — stop tracking a file
- `jj diff` — show changes in the working copy
