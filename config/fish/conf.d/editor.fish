#!/usr/bin/env fish

# Set EDITOR with emacsclient -t as first choice, falling back to nvim/vim
if command -q emacs
    set -gx EDITOR "emacsclient -t"
else if command -q nvim
    set -gx EDITOR nvim
else if command -q vim
    set -gx EDITOR vim
else
    set -gx EDITOR nano
end
