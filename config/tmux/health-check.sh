#!/usr/bin/env bash
set -u

failures=0
warn() { printf 'WARN: %s\n' "$*"; }
fail() { printf 'FAIL: %s\n' "$*"; failures=$((failures + 1)); }
ok() { printf 'OK: %s\n' "$*"; }

command -v tmux >/dev/null 2>&1 && ok "tmux found: $(tmux -V)" || fail "tmux is not installed"

for script in clipboard.sh host-status.sh cheatsheet.sh sessionizer.sh; do
  path="$HOME/.config/tmux/$script"
  [ -f "$path" ] || path="$(dirname "$0")/$script"
  [ -f "$path" ] && ok "$script exists" || { fail "$script missing"; continue; }
  [ -x "$path" ] && ok "$script is executable" || fail "$script is not executable"
  bash -n "$path" && ok "$script syntax" || fail "$script syntax failed"
done

if [ -n "${COLORTERM:-}" ] && [[ "$COLORTERM" == *truecolor* || "$COLORTERM" == *24bit* ]]; then
  ok "truecolor advertised by COLORTERM=$COLORTERM"
else
  warn "truecolor not advertised by COLORTERM"
fi

if command -v fzf >/dev/null 2>&1; then
  ok "fzf found for sessionizer"
else
  warn "fzf not found, sessionizer will print candidates only"
fi

if command -v pbcopy >/dev/null 2>&1 || command -v wl-copy >/dev/null 2>&1 || command -v xclip >/dev/null 2>&1 || command -v xsel >/dev/null 2>&1; then
  ok "clipboard command found"
else
  warn "no clipboard command found"
fi

exit "$failures"
