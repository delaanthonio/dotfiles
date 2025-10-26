# =============================================================================
# ZSHENV CONFIGURATION
# =============================================================================
# Last updated: $(date +%Y-%m-%d)
# Description: Environment variables and PATH setup for zsh
# Note: This file is sourced for all zsh invocations (interactive and non-interactive)

# =============================================================================
# CORE ENVIRONMENT VARIABLES
# =============================================================================

# Editor configuration
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"

# SSH configuration
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Manual page path
export MANPATH="/usr/local/man:$MANPATH"

# =============================================================================
# PATH CONFIGURATION
# =============================================================================

# System paths
export PATH="$PATH:/usr/local/bin:/usr/bin"

# Homebrew paths (macOS)
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"

# Local user binaries
export PATH="$HOME/.local/bin:$PATH"

# =============================================================================
# DEVELOPMENT TOOLS SETUP
# =============================================================================

# Go development
[ -d "$HOME/go/bin" ] && export PATH="$HOME/go/bin:$PATH"

# Rust development
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Node.js package managers
if [[ -d "$HOME/Library/pnpm" ]]; then
    export PNPM_HOME="$HOME/Library/pnpm"
    export PATH="$PNPM_HOME:$PATH"
fi

# =============================================================================
# APPLICATION-SPECIFIC SETUP
# =============================================================================

# Doom Emacs
if [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

# GNU Utils for macOS (via Homebrew)
if [[ -d "/opt/homebrew/opt/make/libexec/gnubin" ]]; then
    export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
fi

