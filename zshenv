# zshenv -*- mode: sh; -*-
# User configuration

export PATH="$PATH:/usr/local/bin/:/usr/bin"
export MANPATH="/usr/local/man:$MANPATH"

export TERM="xterm-256color"
export EDITOR="emacsclient --create-frame --alternate-editor=''"
export ALTERNATE_EDITOR="vim"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Local bin
export PATH="$HOME/.local/bin:$PATH"

# Go
[ -d "$HOME/go/bin" ] && export PATH="$HOME/go/bin:$PATH"

#brew
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"

# Doom emacs bin
if [ -d $HOME/.emacs.d/bin ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

# Rustup
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Node
[ -d "$HOME/.nvm" ] && export NVM_DIR="$HOME/.nvm"
[ -d "$HOME/.nvm" ] && export NODE_VERSIONS="$HOME/.nvm/versions/node/"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && source "/opt/homebrew/opt/nvm/nvm.sh"

if [[ -d "$HOME/Library/pnpm" ]]; then
    export PNPM_HOME="$HOME/Library/pnpm"
    export PATH="$PNPM_HOME:$PATH"
fi
