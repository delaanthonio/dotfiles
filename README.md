# Dela's dotfiles

My dotfiles

## Installation

Install starship:
``` bash
brew install starship
```
or:
``` bash
curl -sS https://starship.rs/install.sh | sh
```

Install dotfiles:
```bash
git clone --recursive https://github.com/delaanthonio/dotfiles ~/.dotfiles
cd ~/.dotfiles
./install
```

## Read-only checks

Run `python3 bin/check-dotfiles` from this repository (or use its absolute path
from another directory). Requires Python 3.9+ and PyYAML. Bash, Zsh, Fish, and
ShellCheck enable their respective validation checks; missing validators produce
warnings. Use `--strict` to make warnings fail as well.

The checker validates JSON/YAML syntax, shell syntax and lint, Dotbot sources,
installed managed symlinks, and generated agent content. It does not source
configs, run the installer/builder, install packages, access the network, or
repair anything. Conditional Dotbot links are reported as skipped rather than
executing their conditions. Dependencies, local agent state, and symlinked files
and directories are excluded from the file scan. Diagnostics omit file contents.
This is static validation, not a full application or security audit.

Run checker regression tests with `python3 -B bin/test-check-dotfiles.py`.

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

The global CLI installer (`bash config/npm/install.sh`, also run by `./install`)
uses the installed Node selected by mise in the current directory, regardless of
an inherited NVM PATH. Run it again after switching Node versions to install the
managed CLIs for that version. Missing mise/Node prerequisites are reported and
skipped; runtimes are not downloaded automatically by this script.

NVM installations and their global packages are not removed or copied. Start a
fresh terminal to discard inherited NVM state. Other runtime managers (such as
rbenv and pyenv) are unchanged by this Node migration.
