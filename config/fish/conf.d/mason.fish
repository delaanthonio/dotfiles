#!/usr/bin/env fish

# Add Mason bin to PATH for Neovim LSP tools (pyright, etc.)
if test -d "$HOME/.local/share/nvim/mason/bin"; and not contains "$HOME/.local/share/nvim/mason/bin" $PATH
    set -gx PATH "$HOME/.local/share/nvim/mason/bin" $PATH
end
