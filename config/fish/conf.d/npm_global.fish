# Add npm global bin to PATH if it exists and isn't already in PATH
# (managed prefix, installs via config/npm/install.sh)
if test -d "$HOME/.npm-global/bin"; and not contains "$HOME/.npm-global/bin" $PATH
    set -gx PATH "$HOME/.npm-global/bin" $PATH
end
