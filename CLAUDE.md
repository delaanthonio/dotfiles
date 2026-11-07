<coding_guidelines>
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a dotfiles repository focused on shell and UI configuration: shells
(zsh, fish), terminal (kitty, ghostty, tmux, Zellij, starship), and editors
(Doom Emacs, Neovim). It uses Dotbot for symlink management and git submodules
for external dependencies.

Agent infrastructure (Claude Code, OpenCode) lives in a separate
`~/agent-config` repository and is intentionally out of scope here.

## Installation & Setup

```bash
# Initial installation
git clone --recursive https://github.com/delaanthonio/dotfiles ~/.dotfiles
cd ~/.dotfiles
./install

# Update submodules after pulling changes
git submodule update --init --recursive

# Re-run installation after config changes
./install
```

## Directory Structure

```
~/.dotfiles/
├── install                     # Dotbot installation script (POSIX sh)
├── install.conf.yaml           # Dotbot symlink configuration
├── modules/                    # Git submodules (dotbot, oh-my-zsh, zsh plugins)
├── bin/                        # Helper scripts (check-dotfiles, upgrade-emacs.sh)
├── config/                     # Application configs
│   ├── fish/                   # Fish shell (conf.d/, completions/, functions/)
│   ├── ghostty/ kitty/ tmux/ zellij/ cmux/  # Terminal configs
│   ├── homebrew/ npm/          # Package installers
│   └── launchd/ emacs-client/  # macOS extras
├── doom.d/                     # Doom Emacs config (init.el, config.el, packages.el)
├── emacs.d/                    # Doom Emacs (git submodule)
├── nvim/                       # Neovim configuration (LazyVim)
├── zshrc, zshenv               # Zsh configuration
├── gitconfig                   # Git configuration
├── starship.toml               # Starship prompt config
├── Brewfile                    # Homebrew packages (brew bundle)
└── Brewfile.labcorp            # Extra packages for work machines
```

## Symlink Management

All dotfile symlinks are managed by Dotbot via `install.conf.yaml`. The install
script updates submodules, backs aside conflicting regular files, creates
symlinks, cleans broken ones, and installs Homebrew/npm packages.

**Never manually symlink files** - always update `install.conf.yaml` and re-run `./install`.

## Shell Configuration

### Fish

Modular via `config/fish/conf.d/*.fish` - one concern per file, auto-sourced on
startup (aliases, brew, editor, starship, etc.).

### Zsh

Loads from `zshenv` (env vars, always) then `zshrc` (interactive). `zshrc`
exits early in coding-agent environments (checks `$CODING_AGENT`, `$CURSOR_AI`,
`$VSCODE_AI`, `$CODESPACE`).

## Editors

### Doom Emacs

- Source: `emacs.d/` (git submodule of hlissner/doom-emacs)
- Config: `doom.d/` (`init.el` modules, `config.el` personal, `packages.el` packages)
- Commands: `doom sync`, `doom upgrade`, `doom doctor`

### Neovim

- Distribution: LazyVim; entry `nvim/init.lua`, modules in `nvim/lua/`

## Homebrew

- `Brewfile` - primary package list; `Brewfile.labcorp` - work extras
  (`DOTFILES_BREWFILE=labcorp`)
- `brew bundle` installs missing packages only; upgrades handled by topgrade
- Use `bi <formula>` / `bci <cask>` (fish and zsh) to install and record in the
  Brewfile in one step

## Git Configuration

Key aliases (from `gitconfig`): `git amend`, `git revise`, `git exclude`,
`git include`, plus Git Town (`git hack`, `git append`, `git compress`,
`git diff-parent`).

## Verification

```bash
bin/check-dotfiles              # syntax + symlink audit (read-only)
bin/check-dotfiles --strict     # also fail on warnings
python3 -m unittest bin.test-check-dotfiles   # checker regression tests
fish -n config/fish/conf.d/x.fish   # syntax check a fish file
bash -n script.sh                   # syntax check a bash script
```

## Critical Files

**Do not edit directly:**
- `emacs.d/.local/*` - Doom Emacs cache
- `nvim/lazy-lock.json` - Neovim plugin lockfile (auto-managed)
- `modules/*` - git submodules (bump, don't edit)

**Always edit these instead:**
- `doom.d/*.el` - Doom config (not emacs.d/)
- `nvim/lua/*` - Neovim config (not lazy-lock.json)
</coding_guidelines>
