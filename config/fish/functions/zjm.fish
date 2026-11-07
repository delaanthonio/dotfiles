#!/usr/bin/env fish

# zjm.fish: Enhanced Zellij session management with layout selection and cleanup
# Usage: zjm <subcommand> [session_name]
# Subcommands: attach, list, kill, cleanup, clean-all

# Helper function to select layout based on hostname
function __zjm_select_layout
    set -l short_hostname (hostname -s 2>/dev/null; or hostname)
    set -l layout "default"

    # Use triple-pane layout on mac-mini
    if string match -q "*mac-mini*" $short_hostname
        set layout "triple-pane"
    end

    echo $layout
end

# Helper function to check if session exists
function __zjm_session_exists
    zellij list-sessions 2>/dev/null | tail -n +2 | sed 's/\x1b\[[0-9;]*m//g' | awk '{print $1}' | grep -q "^$argv[1]\$"
end

function zjm --description 'Manage Zellij sessions with layout selection and cleanup'
    if test (count $argv) -lt 1
        echo "Usage: zjm [attach|list|kill|cleanup|clean-all] [session_name]"
        echo ""
        echo "Subcommands:"
        echo "  attach <name>   - Attach or create session (alias: a)"
        echo "  list            - List all sessions (alias: ls)"
        echo "  kill <name>     - Kill a specific session (alias: k)"
        echo "  cleanup <name>  - Force cleanup hung session (alias: c)"
        echo "  clean-all       - Kill all Zellij processes and clean sockets (alias: ca)"
        return 1
    end

    set -l subcommand $argv[1]
    set -e argv[1]  # Remove subcommand from args

    switch $subcommand
        case attach a
            if test (count $argv) -lt 1
                echo "Usage: zjm attach <session_name>"
                return 1
            end
            set -l session_name $argv[1]
            set -l layout (__zjm_select_layout)

            # Check if session exists
            if __zjm_session_exists $session_name
                # Session exists: attach without layout
                zellij attach $session_name
            else
                # Session doesn't exist: create with layout using -n flag
                zellij -n $layout --session $session_name
            end

        case list ls
            zellij list-sessions

        case kill k
            if test (count $argv) -lt 1
                echo "Usage: zjm kill <session_name>"
                return 1
            end
            set -l session_name $argv[1]
            zellij kill-session $session_name
            echo "Session '$session_name' killed"

        case cleanup c
            if test (count $argv) -lt 1
                echo "Usage: zjm cleanup <session_name>"
                return 1
            end
            set -l session_name $argv[1]

            echo "Cleaning up hung session: $session_name"

            # Find and kill the specific zellij server process
            set -l pid (pgrep -f "zellij --server.*/$session_name\$")
            if test -n "$pid"
                echo "Found server process: $pid"
                kill -9 $pid
                sleep 1
                echo "Killed server process"
            else
                echo "No server process found for session: $session_name"
            end

            # Remove socket files
            set -l socket_files (find /var/folders -name $session_name -type s 2>/dev/null)
            if test -n "$socket_files"
                for socket in $socket_files
                    echo "Removing socket: $socket"
                    rm -f $socket
                end
            else
                echo "No socket files found"
            end

            echo "Cleanup complete for session: $session_name"

        case clean-all ca
            echo "WARNING: This will kill ALL Zellij processes and sessions!"
            echo -n "Are you sure? [y/N] "
            read -l confirm

            if test "$confirm" = "y" -o "$confirm" = "Y"
                echo "Killing all Zellij processes..."
                pkill -9 zellij
                sleep 1

                echo "Cleaning up all socket files..."
                find /var/folders -path "*/zellij-*/*" -type s -delete 2>/dev/null

                echo "All Zellij sessions cleaned up"
            else
                echo "Cancelled"
                return 1
            end

        case '*'
            echo "Invalid subcommand: $subcommand"
            echo "Usage: zjm [attach|list|kill|cleanup|clean-all] [session_name]"
            return 1
    end
end

# Completions for zjm command
complete -c zjm -n '__fish_use_subcommand' -xa 'attach\t"Attach or create session" list\t"List all sessions" kill\t"Kill a session" cleanup\t"Force cleanup hung session" clean-all\t"Kill all Zellij processes"'
complete -c zjm -n '__fish_use_subcommand' -xa 'a\t"Attach or create session (short)" ls\t"List all sessions (short)" k\t"Kill a session (short)" c\t"Force cleanup hung session (short)" ca\t"Kill all Zellij processes (short)"'

# Complete session names for commands that need them
complete -c zjm -n '__fish_seen_subcommand_from attach a kill k cleanup c' -xa '(zellij list-sessions 2>/dev/null | tail -n +2 | awk \'{print $1}\')'
