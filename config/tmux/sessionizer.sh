#!/usr/bin/env bash
set -euo pipefail

roots=(
  "$HOME/.dotfiles"
  "$HOME/Code"
  "$HOME/Developer"
  "$HOME/Projects"
  "$HOME/src"
  "$HOME/work"
)

paths=()
for root in "${roots[@]}"; do
  [ -d "$root" ] || continue
  paths+=("$root")
  while IFS= read -r dir; do
    paths+=("$dir")
  done < <(find "$root" -mindepth 1 -maxdepth 2 -type d \( -name .git -o -name node_modules -o -name .cache \) -prune -o -type d -print 2>/dev/null)
done

if [ "${#paths[@]}" -eq 0 ]; then
  printf 'No project directories found.\n'
  exit 1
fi

if command -v fzf >/dev/null 2>&1; then
  selected=$(printf '%s\n' "${paths[@]}" | awk '!seen[$0]++' | fzf --prompt='tmux session> ' --height=80% --reverse)
else
  printf '%s\n' "${paths[@]}" | awk '!seen[$0]++'
  printf '\nInstall fzf for interactive selection, or pass a path as the first argument.\n'
  selected="${1:-}"
fi

[ -n "${selected:-}" ] || exit 0
[ -d "$selected" ] || { printf 'Not a directory: %s\n' "$selected" >&2; exit 1; }

name=$(basename "$selected" | tr '.: ' '___')
if ! tmux has-session -t "$name" 2>/dev/null; then
  tmux new-session -ds "$name" -c "$selected"
fi

tmux switch-client -t "$name"
