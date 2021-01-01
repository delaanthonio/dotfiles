# User configuration

export PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export MANPATH="/usr/local/man:$MANPATH"

export TERM="xterm-256color"
export EDITOR="emacsclient --create-frame --alternate-editor=''"
export ALTERNATE_EDITOR="vim"

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Local bin
export PATH="$HOME/.local/bin:$PATH"

# Doom emacs bin
if [ -d $HOME/.emacs.d/bin ]; then
   export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -f ~/.zshenv_local ]; then
    source ~/.zshenv_local
fi
