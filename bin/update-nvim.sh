#!/bin/sh
# Update Neovim plugins via lazy.nvim and commit the lockfile bump.
#
# Invoked by topgrade ("03 Neovim" in topgrade.toml). Commits only
# nvim/lazy-lock.json so plugin bumps stay separate from feature work;
# never pushes - review and push manually.

set -eu

DOTFILES="${DOTFILES:-$HOME/.dotfiles}"
LOCKFILE="nvim/lazy-lock.json"

command -v nvim >/dev/null || { echo "nvim unavailable; skipping"; exit 0; }

# Start from the latest main so the bump applies to current dotfiles; being
# offline or diverged should not fail the whole topgrade run.
git -C "$DOTFILES" pull --ff-only -q || echo "topgrade-nvim: skipping pull (offline or diverged)"

nvim --headless '+Lazy! update' +qa

if git -C "$DOTFILES" diff --quiet -- "$LOCKFILE"; then
    echo "topgrade-nvim: no plugin changes"
    exit 0
fi

git -C "$DOTFILES" add "$LOCKFILE"
git -C "$DOTFILES" commit -m "chore(nvim): bump lazy-lock.json (topgrade)"
git -C "$DOTFILES" log --oneline -1
