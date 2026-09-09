# Use runtime-switching hooks interactively and shims in scripts.
if command -v mise >/dev/null
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end
