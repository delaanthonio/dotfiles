#!/usr/bin/env fish

# List of IDE commands to check for
set -l ide_cmds "cursor" "code" "windsurf"
# Corresponding TERM_PROGRAM patterns
set -l term_program_env_names "vscode" "vscode" "vscode"

for i in (seq (count $ide_cmds))
    set -l cmd $ide_cmds[$i]
    if command -v $cmd >/dev/null
        set -l term_name (string split ' ' $term_program_env_names[$i])
        string match -q -- "$TERM_PROGRAM" $term_name
        and . ($cmd --locate-shell-integration-path fish)
        and break
    end
end
