#!/usr/bin/env bash
# Install Homebrew packages from a Brewfile via `brew bundle`.
#
# Usage (invoked by ./install as a dotbot shell step, or by hand):
#   ./config/homebrew/install.sh                      # personal Brewfile
#   DOTFILES_BREWFILE=labcorp ./config/homebrew/install.sh  # work machine
#
# Behavior:
# - Locates brew in PATH, /opt/homebrew/bin, or /usr/local/bin.
# - Installs missing packages only; upgrades are left to topgrade.
# - Skips with a loud warning when brew is missing so a fresh machine can
#   still bootstrap the rest of the dotfiles.

set -euo pipefail

dir="$(cd "$(dirname "$0")" && pwd)"
name="${DOTFILES_BREWFILE:-}"
if [ -n "$name" ]; then
  brewfile="$dir/Brewfile.$name"
else
  brewfile="$dir/Brewfile"
fi

if ! command -v brew >/dev/null 2>&1; then
  for candidate in /opt/homebrew/bin /usr/local/bin; do
    if [ -x "$candidate/brew" ]; then
      PATH="$candidate:$PATH"
      export PATH
      break
    fi
  done
fi
if ! command -v brew >/dev/null 2>&1; then
  echo "WARNING: brew not found; skipping Brewfile packages."
  echo "WARNING: Install Homebrew from https://brew.sh then re-run ./install."
  exit 0
fi

if [ ! -f "$brewfile" ]; then
  echo "WARNING: $brewfile not found; skipping brew bundle."
  exit 0
fi

echo "==> brew bundle --file=$brewfile"
brew bundle --file="$brewfile"
