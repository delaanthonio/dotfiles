# Shared helper for bi/bci: append entries to the Brewfile.
# Skips flags (anything starting with "-") and packages already declared.
function _brew_record --description "Record brew/cask packages in ~/.dotfiles/Brewfile"
    set -l type $argv[1]
    set -e argv[1]
    set -l brewfile "$HOME/.dotfiles/Brewfile"
    for pkg in $argv
        if string match -q -- '-*' $pkg
            continue
        end
        if not grep -qE "^$type \"$pkg\"" "$brewfile" 2>/dev/null
            echo "$type \"$pkg\"" >> "$brewfile"
        end
    end
end
