# User configuration

export PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export MANPATH="/usr/local/man:$MANPATH"

export TERM="xterm-256color"
export EDITOR="micro"
export ALTERNATE_EDITOR="emacsclient"

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

export PATH="$HOME/.pyenv/bin:$PATH" # Add RVM to PATH for scripting
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Local bin
export PATH="$HOME/.local/bin:$PATH"
