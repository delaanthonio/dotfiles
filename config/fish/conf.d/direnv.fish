# Direnv integration for Fish shell
# Automatically loads .envrc files when entering directories

if command -v direnv >/dev/null
    direnv hook fish | source
end