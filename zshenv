# User configuration

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

export PATH="$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG="en_US.UTF-8"

export TERM="xterm-256color"
export EDITOR="emacsclient"

export VIRTUALENVWRAPPER_PYTHON='~/.pyenv/versions/3.5.1/bin/virtualenvwrapper.sh'
export PATH="/opt/emacs-24.5/bin:$PATH"

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# ghc w/ stack
export PATH="$HOME/.stack/programs/x86_64-linux/ghc-7.10.2/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

export PATH="$HOME/.pyenv/bin:$PATH" # Add RVM to PATH for scripting
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH="/opt/bin:$PATH"

# Local bin
export PATH="$HOME/.local/bin:$PATH"

# QT 5
export PATH="$HOME/.qt/5.5/gcc_64/bin:$PATH"

# Dev Tools
export PATH="$HOME/Dev/clang+llvm-3.9.1/bin:$PATH"
export PATH="$HOME/Dev/neo4j-community-3.1.0/bin:$PATH"

export PATH="/opt/android/sdk/tools:$PATH"
export JAVA_HOME="/usr/lib/jvm/java-7-openjdk-amd64"
export ANDROID_STUDIO="/opt/android/studio"
export ANDROID_HOME="~/android/sdk"

export SUBLIME_N9_EXPORT_DIR="~/Android/nexus_9/sublime_n9/"

export GITHUB_USER='Beta1440'

# Load nvm
export NVM_DIR="/home/dell/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
