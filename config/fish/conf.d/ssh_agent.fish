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

# Fallback: start an agent if still no socket
if test -z "$SSH_AUTH_SOCK"
    set -l agent_output (ssh-agent -s)
    for line in $agent_output
        __ssh_parse_agent_output $line
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
