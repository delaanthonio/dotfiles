# Install casks and record them in the Brewfile in one step.
# Usage: bci firefox slack
function bci --description "brew install --cask and record casks in the Brewfile"
    brew install --cask $argv
    and _brew_record cask $argv
end
