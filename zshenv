# User configuration

export PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG="en_US.UTF-8"

export TERM="xterm-256color"
export EDITOR="emacsclient"

export VIRTUALENVWRAPPER_PYTHON='~/.pyenv/versions/3.5.1/bin/virtualenvwrapper.sh'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

export PATH="$HOME/.pyenv/bin:$PATH" # Add RVM to PATH for scripting
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH="/opt/bin:$PATH"

# Local bin
export PATH="$HOME/.local/bin:$PATH"

export PATH="/opt/android/sdk/tools:$PATH"

# Load nvm
export NVM_DIR="/home/dell/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
