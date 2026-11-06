# Dela's dotfiles

My dotfiles

## Installation

Requires Git and Python 3.9+. Preview first; no packages or agent configuration
are installed by default:

```bash
git clone https://github.com/delaanthonio/dotfiles ~/.dotfiles
cd ~/.dotfiles
./install --dry-run
./install
```

### Profiles

| Profile | Configuration |
| --- | --- |
| `minimal` (default) | Fish, its functions/completions, Starship, Neovim, global Git ignore patterns. No Git identity or login-shell changes. |
| `server` | Minimal plus tmux, Zellij and direnv. Linux-safe; Fish links exclude `brew.fish`, `claude.fish` and `emacs*.fish`. |
| `workstation` | macOS only: desktop terminals, Emacs/Doom, Zsh and its plugins, Git configuration, and all other original non-agent links. |
| `agents` (opt-in layer) | Existing Claude, OpenCode and ccstatusline configuration. Codex and Droid local configuration is deliberately untouched. |

```bash
./install --profile server --dry-run
./install --profile server
./install --profile minimal,agents
./install --profile agents                  # only the agent layer
```

Choose one base profile, optionally followed by `,agents`. Unknown names fail
with the supported list. There is no automatic hostname mapping: an omitted
profile always means `minimal`.

The single link table is `install.conf.yaml`. It uses JSON syntax (valid YAML)
so previews need only Python's standard library. Profile/platform metadata is
removed when composing a temporary Dotbot configuration. Recursive globs keep
their relative paths. Minimal/server link individual Fish startup files;
workstation preserves the original whole-directory link.

### Safety and explicit opt-ins

- `--dry-run` prints every selected link, its status, required submodule command,
  backup and package step. It does not write files, fetch dependencies or run
  package managers, even with `--force` or `--packages`.
- Existing regular files, directories and conflicting symlinks are **skipped
  and reported**. Correct links are unchanged. Missing sources are reported and
  skipped, not invented. Nothing is renamed by default.
- `--force` moves conflicts to unique sibling backups named
  `<target>.bak.<unix-seconds>[.<counter>]` before linking. Backups are never
  overwritten. `--force-agents` enables the agents layer and applies this
  backup behavior only to its targets, not shell/editor targets.
- A symlink or non-directory in a target's parent path is always skipped,
  including with `--force`, to avoid writing outside HOME or into the checkout.
  Resolve such parent conflicts yourself. Changing profiles does not remove
  links left by a previous profile.
- `--packages` is required for every package installer. On macOS,
  minimal/server use `brew install`; workstation uses the root Brewfile via
  `brew bundle --no-upgrade`. On Debian/Ubuntu, minimal installs `fish neovim`
  with apt; server adds `direnv tmux`. Install Starship and Zellij separately
  when not available from your distribution. Other Linux distributions should
  manage packages separately.
- Agent npm packages require both the agents layer and `--packages`. The
  currently absent `config/npm/install.sh` is reported and skipped.
- Only Dotbot (and its own dependencies) is initialized for minimal, server and
  agents. Doom Emacs, oh-my-zsh and the Zsh plugins are initialized only for
  workstation, never by a blanket repository submodule update.

To reproduce the old full Mac link selection and opt into its package steps:

```bash
./install --profile workstation,agents --packages --force --force-agents --dry-run
./install --profile workstation,agents --packages --force --force-agents
```

Unlike the old installer, this command preserves replaced targets in backups.
Pre-existing missing sources (including `agents/AGENTS.md` and the Emacs Client
app) and the absent npm script remain reported gaps. Leave off `--force` and
`--force-agents` to keep local conflicts untouched.

Test without touching your home:

```bash
test_home=$(mktemp -d)
HOME="$test_home" ./install --profile server --dry-run
HOME="$test_home" ./install --profile server
HOME="$test_home" ./install --profile server   # reports No changes
```

Each real run records its selected profile in
`~/.config/dotfiles/profile.json`, without replacing an unmanaged state file.
This describes the most recent selection, not an inventory of older profiles.

## Read-only checks

Run `python3 bin/check-dotfiles` from this repository (or use its absolute path
from another directory). Requires Python 3.9+ and PyYAML. Bash, Zsh, Fish, and
ShellCheck enable their respective validation checks; missing validators produce
warnings. Use `--strict` to make warnings fail as well.

The checker validates JSON/YAML syntax, shell syntax and lint, Dotbot sources,
installed managed symlinks, and generated agent content. It does not source
configs, run the installer/builder, install packages, access the network, or
repair anything. Managed links are checked for the last selected profile
(minimal if no profile record exists). Legacy conditional Dotbot links are
reported as skipped rather than executing their conditions. Dependencies,
local agent state, and symlinked files
and directories are excluded from the file scan. Diagnostics omit file contents.
This is static validation, not a full application or security audit.

Run checker regression tests with `python3 -B bin/test-check-dotfiles.py`.
Run profile regression tests with `python3 -B bin/test-install-dotfiles.py`;
real-install cases use temporary homes and require initialized Dotbot.
`./check` runs both suites, shell/data syntax checks and the agent drift check.

## Node.js with mise

Mise manages Node versions in both Fish and Zsh. For a new machine:

```bash
brew install mise
mise use --global node@24
```

Open a fresh terminal after installing the dotfiles. Interactive shells activate
mise; non-interactive shells use its shims. Zsh supports custom `MISE_DATA_DIR`
and `XDG_DATA_HOME` locations. Existing per-machine mise configuration is not
overwritten by this repository.

Use `mise use node@24` inside a project to record its version in `mise.toml`.
For direnv, replace `use nvm ...` with `use mise` in `.envrc`, configure Node in
`mise.toml`, and run `direnv allow`. Add `layout node` if the project relied on the
old helper adding `node_modules/.bin` to PATH. Projects that only have `.nvmrc`
should migrate their version selection to `mise.toml`.

When present, the global CLI installer (`bash config/npm/install.sh`, run by
`./install --profile agents --packages`)
uses the installed Node selected by mise in the current directory, regardless of
an inherited NVM PATH. Run it again after switching Node versions to install the
managed CLIs for that version. Missing mise/Node prerequisites are reported and
skipped; runtimes are not downloaded automatically by this script.

NVM installations and their global packages are not removed or copied. Start a
fresh terminal to discard inherited NVM state. Other runtime managers (such as
rbenv and pyenv) are unchanged by this Node migration.
