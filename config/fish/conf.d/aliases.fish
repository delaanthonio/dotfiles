# Navigation
alias .. 'cd ..'
alias ..2 'cd ../..'
alias ..3 'cd ../../..'
abbr -a ll 'ls -lah'
abbr -a la 'ls -A'
abbr -a l 'ls -CF'

# Editors
alias e 'emacsclient -nc -a "emacs"'
alias et 'emacsclient -t'
alias ec 'emacsclient -c'
alias v 'nvim'
alias vi 'nvim'

# Git (common operations)
abbr -a g git
abbr -a gs 'git status'
abbr -a ga 'git add'
abbr -a gc 'git commit'
abbr -a gco 'git checkout'
abbr -a gb 'git branch'
abbr -a gp 'git push'
abbr -a gl 'git pull'
abbr -a gd 'git diff'
abbr -a glog 'git log --oneline --graph --decorate'

# Git Town
abbr -a t 'git town'
abbr -a th 'git town hack'
abbr -a ts 'git town sync'
abbr -a tp 'git town propose'
abbr -a tsh 'git town ship'
abbr -a tk 'git town kill'
abbr -a tr 'git town rename-branch'
abbr -a tc 'git town continue'
abbr -a tu 'git town undo'
abbr -a tpr 'git town prepend'
abbr -a ta 'git town append'

# Docker
abbr -a d docker
abbr -a dc 'docker compose'
abbr -a dps 'docker ps'
abbr -a dex 'docker exec -it'
abbr -a dlog 'docker logs -f'
abbr -a dco 'docker compose'
abbr -a dcb 'docker compose build'
abbr -a dce 'docker compose exec'
abbr -a dcps 'docker compose ps'
abbr -a dcup 'docker compose up -d'
abbr -a dcdn 'docker compose down'
abbr -a dcl 'docker compose logs -f'

# Kubernetes
abbr -a k kubectl
abbr -a kgp 'kubectl get pods'
abbr -a kgs 'kubectl get svc'
abbr -a kgd 'kubectl get deployment'
abbr -a kdp 'kubectl describe pod'
abbr -a klog 'kubectl logs -f'

# Package managers
abbr -a y yarn
abbr -a p pnpm
abbr -a n npm

# System
abbr -a tg topgrade
abbr -a reload 'source ~/.config/fish/config.fish'

# Zellij
abbr -a zj zellij
abbr -a zja 'zellij attach'
abbr -a zjl 'zellij list-sessions'

# Terraform
abbr -a tf terraform
abbr -a tfi 'terraform init'
abbr -a tfp 'terraform plan'
abbr -a tfa 'terraform apply'

# Directory jumps (if you have common project dirs)
# alias proj 'cd ~/projects'
# alias dot 'cd ~/.dotfiles'
