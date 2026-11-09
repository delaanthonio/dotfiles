# Bash configuration, managed by dotfiles (see install.conf.yaml).
# Machine-local additions go in ~/.bashrc.local (not managed).
# shellcheck shell=bash disable=SC1091

# Rancher Desktop
[[ -d "$HOME/.rd/bin" ]] && export PATH="$HOME/.rd/bin:$PATH"

# Rust
[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# Maestro
[[ -d "$HOME/.maestro/bin" ]] && export PATH=$PATH:$HOME/.maestro/bin

# Local overrides
[[ -f "$HOME/.bashrc.local" ]] && . "$HOME/.bashrc.local"

# Smarter cd with frecency (zoxide): use `z` like cd.
if [[ $- == *i* ]] && command -v zoxide >/dev/null; then
  eval "$(zoxide init bash)"
fi

# Starship prompt (interactive shells only)
if [[ $- == *i* ]] && command -v starship >/dev/null; then
  eval "$(starship init bash)"
fi
