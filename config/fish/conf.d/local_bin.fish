#!/usr/bin/env fish

# Add ~/.local/bin to PATH if it exists and isn't already in PATH
if test -d "$HOME/.local/bin"; and not contains "$HOME/.local/bin" $PATH
    set -gx PATH "$HOME/.local/bin" $PATH
end