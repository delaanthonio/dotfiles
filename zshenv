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
export EDITOR="emacs -nw"
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

if [[ -d "/opt/homebrew/opt/nvm" ]]; then
    export NVM_HOMEBREW=/opt/homebrew/opt/nvm
fi

# Emacs native-comp linker fix (Homebrew libgccjit/gcc on macOS)
_emacs_libgccjit_root="/opt/homebrew/opt/libgccjit/lib/gcc/current"
_emacs_gcc_root="/opt/homebrew/opt/gcc/lib/gcc/current/gcc"
_emacs_native_comp_paths=()
if [[ -d "$_emacs_libgccjit_root" ]]; then
    _emacs_native_comp_paths+=("$_emacs_libgccjit_root")
fi
if [[ -d "$_emacs_gcc_root" ]]; then
    _emacs_native_comp_paths+=("${_emacs_gcc_root}"/aarch64-apple-darwin*/<->(N/) "$_emacs_gcc_root")
fi
for _emacs_native_comp_path in "${_emacs_native_comp_paths[@]}"; do
    case ":${LIBRARY_PATH:-}:" in
        *":${_emacs_native_comp_path}:"*) ;;
        *) export LIBRARY_PATH="${_emacs_native_comp_path}${LIBRARY_PATH:+:$LIBRARY_PATH}" ;;
    esac
done
unset _emacs_libgccjit_root _emacs_gcc_root _emacs_native_comp_paths _emacs_native_comp_path
