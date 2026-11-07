# Dotfiles Repository Agent Guide

## Commands

**Installation & Setup:**
- `./install` - Run Dotbot to create all symlinks (always run after config changes)
- `git submodule update --init --recursive` - Update all git submodules
- `doom sync` - Sync Doom Emacs packages after editing doom.d/

**Verification:**
- `bin/check-dotfiles` - Read-only syntax and symlink audit
- `python3 -m unittest bin.test-check-dotfiles` - Checker regression tests
- `bash -n script.sh` - Syntax check bash scripts
- `fish -n script.fish` - Syntax check fish scripts
- `shellcheck script.sh` - Lint shell scripts (if available)

## Code Style

**Shell (Bash/Zsh):**
- Use `set -euo pipefail` at the top of scripts
- Prefer `"$var"` over `$var` for variable expansion
- Use `[[ ]]` over `[ ]` for conditionals
- Document complex scripts with header comments (purpose, usage)
- Use descriptive variable names (SNAKE_CASE for constants, lowercase for locals)

**Fish:**
- Use `abbr` for command shortcuts (not `alias`)
- Modular configs in `config/fish/conf.d/*.fish` (one concern per file)
- Check command existence: `if command -v foo >/dev/null`
- Set paths: `set -gx PATH "/new/path" $PATH`

**Emacs Lisp:**
- Follow Doom Emacs conventions (lexical binding, `after!` hooks)
- Set user preferences in `doom.d/config.el`
- Declare packages in `doom.d/packages.el`
- Module selection in `doom.d/init.el`

**YAML:**
- Validate syntax before committing (use `yq` or `yamllint`)
- 2-space indentation
- Use quotes for strings with special characters

## Critical Rules

**Never Edit:**
- `emacs.d/.local/*` - Doom Emacs cache
- `nvim/lazy-lock.json` - auto-managed plugin lockfile
- `modules/*` - git submodules (bump, don't edit)

**Always Run Install:**
- Run `./install` after adding/modifying dotfile symlinks
- Update `install.conf.yaml` for new symlinks (never create manually)

**Agent config is out of scope:**
- Claude Code / OpenCode / Codex config lives in `~/agent-config`, not here
