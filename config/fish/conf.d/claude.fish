#!/usr/bin/env fish

if test -d "$HOME/.claude/local"; and not contains "$HOME/.claude/local" $PATH
    set -gx PATH "$HOME/.claude/local" $PATH
end
