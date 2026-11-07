# Dela's dotfiles

Shell, terminal, and editor configuration for macOS (and Linux), managed with
[Dotbot](https://github.com/anishathalye/dotbot). Focused on look-and-feel and
workflow: zsh, fish, kitty, ghostty, tmux, Zellij, Doom Emacs, Neovim, and the
tooling around them.

Agent infrastructure (Claude Code, Codex) lives in a separate
`~/agent-config` repository.

## Installation

```bash
git clone --recursive https://github.com/delaanthonio/dotfiles ~/.dotfiles
cd ~/.dotfiles
./install
```

`./install` (POSIX sh, safe from any login shell):

1. Updates git submodules
2. Moves aside any conflicting local files as `<name>.bak.<unix-ts>` instead of
   overwriting them
3. Symlinks everything via `install.conf.yaml` and cleans broken symlinks
4. Installs Homebrew packages from `Brewfile` (work machines: set
   `DOTFILES_BREWFILE=labcorp` to also apply `Brewfile.labcorp`)
5. Installs the managed global npm CLIs via mise's Node
6. Enables the droid-daemon LaunchAgent if linked but not loaded

Re-run `./install` after any config change. Machine-local secrets and
generated files (fish `secrets.fish`, OrbStack `config.fish`, fish 4.3
"frozen" files, `.claude/settings.local.json`) are gitignored and never
overwritten silently.

## Layout

```
├── install, install.conf.yaml   # Dotbot installer (safe backup-and-link)
├── bin/                         # Helpers: check-dotfiles, commit-submodules,
│                                # update-nvim.sh, upgrade-emacs.sh
├── config/
│   ├── fish/                    # conf.d/, completions/, functions/
│   ├── ghostty/ kitty/ cmux/    # Terminal emulators
│   ├── tmux/ zellij/            # Multiplexers (gruvbox theme)
│   ├── herdr/                   # herdr terminal workspace config
│   ├── ccstatusline/ launchd/   # Statusline, droid-daemon LaunchAgent
│   ├── homebrew/ npm/           # Package installers (Brewfiles at root)
│   └── emacs-client/            # Emacs Client.app for ~/Applications
├── doom.d/                      # Doom Emacs config (emacs.d is a submodule)
├── nvim/                        # Neovim (LazyVim; lua/plugins/*.lua overrides)
├── Brewfile, Brewfile.labcorp   # Homebrew declarations (personal / work)
├── topgrade.toml                # Update runner (brew, nvim plugins, Doom)
├── zshrc, zshenv, gitconfig     # Shell and git configuration
└── starship.toml, direnvrc      # Prompt and direnv
```

## Updating

- **Packages:** topgrade runs Homebrew upgrades, Doom updates, and Neovim
  plugin updates (`bin/update-nvim.sh` commits `nvim/lazy-lock.json` bumps
  automatically; never pushes)
- **Submodules:** topgrade floats them; commit the bumps with
  `bin/commit-submodules`
- **Configs:** edit in the repo, re-run `./install`, push
- Install conflicting-file backups accumulate as `*.bak.<unix-ts>` next to the
  original — delete them once confirmed

## Read-only checks

Run `python3 bin/check-dotfiles` from this repository. Requires Python 3.9+
and PyYAML (installed for mise's Python via
`pip install --break-system-packages PyYAML`). Bash, Zsh, Fish, and ShellCheck
enable their respective validation checks; missing validators produce
warnings. Use `--strict` to make warnings fail as well.

The checker validates JSON/YAML syntax, shell syntax and lint, Dotbot sources,
and installed managed symlinks. It does not source configs, run the installer,
install packages, access the network, or repair anything. Conditional Dotbot
links are reported as skipped rather than executing their conditions.
Dependencies, local agent state, and symlinked files and directories are
excluded from the file scan. Diagnostics omit file contents. This is static
validation, not a full application or security audit.

Run checker regression tests with `python3 -B bin/test-check-dotfiles.py`.

## Node.js with mise

Mise manages Node versions in both Fish and Zsh. For a new machine:

```bash
brew install mise
mise use --global node@24
```

Open a fresh terminal after installing the dotfiles. Interactive shells
activate mise; non-interactive shells use its shims. The global CLI installer
(`bash config/npm/install.sh`, also run by `./install`) uses the installed
Node selected by mise. Missing mise/Node prerequisites are reported and
skipped; runtimes are not downloaded automatically by this script.

Use `mise use node@24` inside a project to record its version in `mise.toml`.
For direnv, replace `use nvm ...` with `use mise` in `.envrc` and run
`direnv allow`.
