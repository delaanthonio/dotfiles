# Auto-manage SSH agent and keys on macOS and Linux
# 1) Prefer launchd-provided agent on macOS (GUI sessions)
# 2) Otherwise start a session-local agent and publish its socket

function __ssh_export_to_session --argument var val
    set -gx $var $val
    if test (uname) = Darwin
        command launchctl setenv $var $val >/dev/null 2>&1
    end
end

function __ssh_parse_agent_output --argument line
    if string match -rq '^SSH_AUTH_SOCK=' $line
        set -l sock (string replace -r '^SSH_AUTH_SOCK=([^;]+);.*' '$1' $line)
        __ssh_export_to_session SSH_AUTH_SOCK $sock
    else if string match -rq '^SSH_AGENT_PID=' $line
        set -l pid (string replace -r '^SSH_AGENT_PID=([^;]+);.*' '$1' $line)
        __ssh_export_to_session SSH_AGENT_PID $pid
    end
end

# Use launchd-provided socket if available (macOS only)
if test -z "$SSH_AUTH_SOCK" -a (uname) = Darwin
    set -l launchd_sock (launchctl getenv SSH_AUTH_SOCK)
    if test -n "$launchd_sock"
        set -gx SSH_AUTH_SOCK $launchd_sock
    end
end

# Fallback: share one agent at a fixed socket instead of starting a new agent
# for every shell. Each `ssh host <cmd>` starts a fresh shell, so a per-shell
# agent leaks one process per command. Start the shared agent only when nothing
# answers on its socket; the lock stops two shells from starting it at once.
if test -z "$SSH_AUTH_SOCK"
    set -l agent_sock $HOME/.ssh/agent.sock
    set -l start_agent 'SSH_AUTH_SOCK="$1" ssh-add -l >/dev/null 2>&1
if [ $? -eq 2 ]; then
    rm -f "$1"
    ssh-agent -a "$1" >/dev/null
fi'
    command mkdir -p $HOME/.ssh
    if type -q flock
        command flock -o $HOME/.ssh/agent.lock sh -c $start_agent sh $agent_sock
    else
        command sh -c $start_agent sh $agent_sock
    end
    if test -S $agent_sock
        __ssh_export_to_session SSH_AUTH_SOCK $agent_sock
    end
end

# Last resort: scan /tmp for an agent socket
if test -z "$SSH_AUTH_SOCK"
    set -l candidates (command find /tmp -maxdepth 1 -name 'ssh-*' -type d 2>/dev/null)
    for dir in $candidates
        set -l socket (command find $dir -maxdepth 1 -name 'agent.*' -print -quit 2>/dev/null)
        if test -n "$socket"
            __ssh_export_to_session SSH_AUTH_SOCK $socket
            break
        end
    end
end

# Load keys stored in macOS Keychain (macOS only)
if test (uname) = Darwin && type -q ssh-add
    ssh-add --apple-load-keychain >/dev/null 2>&1
end

functions -e __ssh_export_to_session __ssh_parse_agent_output >/dev/null 2>&1
