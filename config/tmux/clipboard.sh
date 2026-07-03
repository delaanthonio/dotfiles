#!/usr/bin/env bash
set -euo pipefail

if command -v pbcopy >/dev/null 2>&1; then
  exec pbcopy
elif command -v wl-copy >/dev/null 2>&1; then
  exec wl-copy
elif command -v xclip >/dev/null 2>&1; then
  exec xclip -selection clipboard -in
elif command -v xsel >/dev/null 2>&1; then
  exec xsel --clipboard --input
fi

cat >/dev/null
printf 'tmux: no clipboard command found (pbcopy, wl-copy, xclip, xsel)\n' >&2
exit 1
