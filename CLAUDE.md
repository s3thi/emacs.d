# CLAUDE.md

## Overview

This is a literate Emacs configuration where the main configuration lives in `README.org` and is tangled to `README.el`.

The setup focuses on: 

- Writing prose (Markdown, Org)
- Front-end development
  - JavaScript and TypeScript
  - JSX and TSX
  - CSS, Tailwind, and SASS
  - Astro

## Architecture

Bootstrap sequence:

1. `early-init.el` - Platform-specific setup (macOS library paths for native compilation)
2. `init.el` - Package manager setup, use-package loading, triggers tangle of README.org
3. `README.el` - Generated from README.org, contains the actual configuration

**Key principle:** Edit `README.org`, not `README.el`. The .el file is auto-generated when the .org file is newer.

## Making changes

To modify the configuration:

1. Edit the relevant section in `README.org`
2. Restart Emacs (which auto-tangles) or manually run `org-babel-tangle`

The configuration uses `use-package` for package management with MELPA as the package repository.

## Conventions

**Custom variables and functions use the `s3thi/` prefix:**

- `s3thi/is-a-mac`, `s3thi/is-a-pc`, `s3thi/is-a-penguin` - Platform detection
- `s3thi/prose-line-spacing` - Line spacing for prose modes
- `s3thi/prose-header-scaling` - Header font sizes

## Key directories

- `elpa/` - Installed packages (gitignored)
- `snippets/` - Yasnippet templates
- `auto-save/` - Auto-save files
- `undo-tree/` - Undo history
- `custom.el` - Auto-generated custom settings (gitignored)

## Major features

- **Completion:** Vertico + Orderless + Marginalia
- **Prose editing:** visual-fill-column, mixed-pitch, jinx (spell check), centered-cursor-mode
- **Programming:** Flycheck, Company, LSP-mode
- **Git:** Magit

## Version control

This project uses the Jujutsu version control system (`jj`) instead of Git. Never run any Git commands. Always use `jj` commands for VCS operations. If you're not sure how to accomplish something using `jj`, stop and ask the user.
