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
