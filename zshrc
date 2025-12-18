# =============================================================================
# ZSHRC CONFIGURATION
# =============================================================================
# Last updated: $(date +%Y-%m-%d)
# Description: Personal zsh configuration with oh-my-zsh, starship, and customizations

# =============================================================================
# ENVIRONMENT SETUP
# =============================================================================

# Check if running in a coding agent environment
# Common indicators: specific environment variables, process names, or parent processes
is_coding_agent() {
    [[ -n "$CODING_AGENT" || -n "$CURSOR_AI" || -n "$VSCODE_AI" || -n "$CODESPACE" ]]
}

if is_coding_agent; then
   exit 0
fi

# Set name of the theme to load.
ZSH_THEME=""

# Language environment
LANG="en_US.UTF-8"

# Oh-my-zsh configuration
MODULES_DIR=$HOME/.dotfiles/modules
export ZSH=$MODULES_DIR/oh-my-zsh

# Oh-my-zsh settings
DISABLE_AUTO_UPDATE="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"

# =============================================================================
# PLUGIN CONFIGURATION
# =============================================================================

# Core plugins
plugins=(
    common-aliases
    extract
    git
    pip
    pj
)

# Project Jump configuration
PROJECT_PATHS=($HOME/Developer $HOME/Git)

# Conditional plugin loading based on available commands
if (( $+commands[apt] )); then
    plugins+=(ubuntu)
fi

if (( $+commands[brew] )); then
    plugins+=(brew)
fi

if (( $+commands[direnv] )); then
    plugins+=(direnv)
fi

if (( $+commands[gh] )); then
    plugins+=(gh)
fi

if (( $+commands[fzf] )); then
    plugins+=(fzf)
fi

if (( $+commands[kubectl] )); then
    plugins+=(kubectl)
fi

if (( $+commands[terraform] )); then
    plugins+=(terraform)
fi

if (( $+commands[yarn] )); then
    plugins+=(yarn)
fi

if (( $+commands[systemd] )); then
    plugins+=(systemd)
fi

# =============================================================================
# DEVELOPMENT ENVIRONMENT SETUP
# =============================================================================

# Node.js (NVM)
if [[ -d "$HOME/.nvm" ]]; then
    export NVM_DIR="$HOME/.nvm"
    export NODE_VERSIONS="$HOME/.nvm/versions/node/"
    plugins+=(npm)
    plugins+=(nvm)

    zstyle ':omz:plugins:nvm' lazy yes
fi

# Ruby (rbenv)
if [[ -d "$HOME/.rbenv" ]] && command -v rbenv >/dev/null 2>&1; then
    eval "$(rbenv init - zsh 2>/dev/null)"
fi

source "$MODULES_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"

source $ZSH/oh-my-zsh.sh

eval "$(starship init zsh)"

# Homebrew completion
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

    autoload -Uz compinit
    compinit
fi

if (( $+commands[git-town] )); then
    source <(git-town completions zsh)
fi

if (( $+commands[tailscale] )); then
    tailscale completion zsh > "${fpath[1]}/_tailscale"
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

# Functions
function reload() {
    exec zsh
}

# pnpm
# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
