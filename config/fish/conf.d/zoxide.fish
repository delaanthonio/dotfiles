# Smarter cd with frecency (zoxide): use `z` like cd.
if status is-interactive && command -v zoxide >/dev/null
    zoxide init fish | source
end
