#!/usr/bin/env fish

# tunnel.fish: Manage autossh tunnels for SSH port forwarding
# Usage: tunnel <subcommand> <port> <ssh_host>
# Subcommands: start, stop, status, restart, list

# Helper function to extract SSH hosts from config for completion
function __tunnel_complete_ssh_hosts
    awk '/^Host/ && $2 !~ /[*?]/ {print $2}' ~/.ssh/config 2>/dev/null
end

function tunnel --description 'Manage autossh tunnels over SSH/Tailscale'
    if test (count $argv) -lt 1
        echo "Usage: tunnel [start|stop|status|restart|list] <port> <ssh_host>"
        return 1
    end

    set -l subcommand $argv[1]
    set -e argv[1]  # Remove subcommand from args

    switch $subcommand
        case start
            if test (count $argv) -lt 2
                echo "Usage: tunnel start <port> <ssh_host>"
                return 1
            end
            set -l port $argv[1]
            set -l host $argv[2]
            # Check if tunnel already exists
            if pgrep -af "autossh.*-L $port:localhost:$port.*$host" >/dev/null
                echo "Tunnel already running for port $port to host $host"
                return 1
            end
            autossh -M 0 -f -N -L $port:localhost:$port $host
            echo "Tunnel started for port $port to host $host"

        case stop
            if test (count $argv) -lt 2
                echo "Usage: tunnel stop <port> <ssh_host>"
                return 1
            end
            set -l port $argv[1]
            set -l host $argv[2]
            pkill -f "autossh.*-L $port:localhost:$port.*$host"
            echo "Tunnel stopped for port $port to host $host"

        case status
            if test (count $argv) -lt 2
                echo "Usage: tunnel status <port> <ssh_host>"
                return 1
            end
            set -l port $argv[1]
            set -l host $argv[2]
            if pgrep -af "autossh.*-L $port:localhost:$port.*$host" >/dev/null
                echo "Tunnel running for port $port to host $host"
            else
                echo "No running tunnel for port $port to host $host"
            end

        case restart
            if test (count $argv) -lt 2
                echo "Usage: tunnel restart <port> <ssh_host>"
                return 1
            end
            tunnel stop $argv
            tunnel start $argv
            echo "Tunnel restarted for port $argv[1] to host $argv[2]"

        case list
            # List all active autossh tunnels
            pgrep -af autossh | grep -E 'autossh.*-L [0-9]+:localhost:[0-9]+' || echo "No active tunnels"

        case '*'
            echo "Invalid subcommand. Usage: tunnel [start|stop|status|restart|list] <port> <ssh_host>"
            return 1
    end
end

# Completions for tunnel command
complete -c tunnel -n '__fish_use_subcommand' -xa 'start\t"Start a new tunnel" stop\t"Stop an existing tunnel" status\t"Check tunnel status" restart\t"Restart a tunnel" list\t"List all active tunnels"'
complete -c tunnel -n '__fish_seen_subcommand_from start stop status restart; and __fish_is_nth_token 2' -xa '(seq 1024 65535)' -d 'Port number'
complete -c tunnel -n '__fish_seen_subcommand_from start stop status restart; and __fish_is_nth_token 3' -xa '(__tunnel_complete_ssh_hosts)' -d 'SSH host'
# No args needed for 'list', so no further completions there

