#!/usr/bin/env bash
# Print a styled hostname pill only when this tmux server is running on a
# remote machine reached via SSH. Prints nothing locally.

if [ -n "${SSH_CONNECTION:-}" ] || [ -n "${SSH_TTY:-}" ]; then
  host=$(hostname -s 2>/dev/null || hostname)
  printf '#[fg=#282828,bg=#b16286,bold] %s #[default]' "$host"
fi
