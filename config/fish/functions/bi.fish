# Install formulae and record them in the Brewfile in one step,
# so ad-hoc installs never drift from the manifest.
# Usage: bi ripgrep fd
function bi --description "brew install formulae and record them in the Brewfile"
    brew install $argv
    and _brew_record brew $argv
end
