# zshrc -*- mode: sh; -*-
# Set name of the theme to load.
ZSH_THEME=""

# You may need to manually set your language environment
LANG="en_US.UTF-8"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="mm/dd/yyyy"

MODULES_DIR=$HOME/.dotfiles/modules

# Path to your oh-my-zsh installation.
export ZSH=$MODULES_DIR/oh-my-zsh

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    brew
    common-aliases
    direnv
    extract
    gh
    git
    fzf
    kubectl
    magic-enter
    pip
    pj
    terraform
)

MAGIC_ENTER_GIT_COMMAND='git status -u .'

if (( $+commands[eza] )); then
    MAGIC_ENTER_OTHER_COMMAND='eza -lh .'
else
    MAGIC_ENTER_OTHER_COMMAND='ls -lh .'
fi

# Project Jump
PROJECT_PATHS=($HOME/Developer $HOME/Git)

if (( $+commands[apt] )); then
    plugins+=(ubuntu)
fi

if (( $+commands[docker] )); then
    plugins+=(docker)
    alias d="docker"
    alias db="docker build"
    alias dex="docker exec -it"

    dccmd="docker compose"

    alias dco="$dccmd"
    alias dcb="$dccmd build"
    alias dce="$dccmd exec"
    alias dcps="$dccmd ps"
    alias dcrestart="$dccmd restart"
    alias dcrm="$dccmd rm"
    alias dcr="$dccmd run"
    alias dcstop="$dccmd stop"
    alias dcup="$dccmd up"
    alias dcupb="$dccmd up --build"
    alias dcupd="$dccmd up -d"
    alias dcdn="$dccmd down"
    alias dcl="$dccmd logs"
    alias dclf="$dccmd logs -f"
    alias dcpull="$dccmd pull"
    alias dcstart="$dccmd start"
    alias dck="$dccmd kill"

    unset dccmd
fi

if (( $+commands[yarn] )); then
    plugins+=(yarn)
fi

if [[ -d "$HOME/.nvm" ]]; then
    export NVM_DIR="$HOME/.nvm"
    export NODE_VERSIONS="$HOME/.nvm/versions/node/"
    plugins+=(npm)
    plugins+=(nvm)
    zstyle ':omz:plugins:nvm' lazy yes
fi

if [[ -d "$HOME/.rbenv" ]]; then
    eval "$(rbenv init - zsh)"
fi

if (( $+commands[systemd] )); then
    plugins+=(systemd)
fi

source "$MODULES_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"

source $ZSH/oh-my-zsh.sh

# Starship Prompt
eval "$(starship init zsh)"

# Homebrew completion
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

  autoload -Uz compinit
  compinit
fi

# Always load syntax highlighting last
source "$MODULES_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# Aliases
alias ..2='cd ../../'
alias ..3='cd ../../../'

alias e="$EDITOR"
alias et='emacsclient -t'
alias ec='emacsclient -c'

alias hib='systemctl hibernate'
alias sus='systemctl suspend'
alias reb='systemctl reboot'

alias tg='topgrade'

unalias pip

# Functions
function reload() {
    exec zsh
}

# pnpm
# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
