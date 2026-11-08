# Bash login-shell configuration, managed by dotfiles (see install.conf.yaml).
# macOS runs login shells by default, so chain to .bashrc here.
# shellcheck shell=bash disable=SC1091

[[ -f "$HOME/.bashrc" ]] && . "$HOME/.bashrc"
