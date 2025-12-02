#!/usr/bin/env bash
# Claude Code status line script
# Displays: user@host in dir on branch | py:venv node:version aws:profile
# Context elements only appear when relevant

set -euo pipefail

# Read JSON input from Claude Code
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model // empty')

# Base info
current_user=$(whoami)
logged_in_user=$(stat -f '%Su' /dev/console 2>/dev/null || echo "$current_user")
host=$(hostname -s)
dir=$(basename "$cwd")

# ANSI color codes
GREEN=$'\033[32m'
YELLOW=$'\033[33m'
MAGENTA=$'\033[35m'
CYAN=$'\033[36m'
RESET=$'\033[0m'

# Git info
git_info=""
if git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null || \
           git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  # Truncate branch name if longer than 20 chars
  if [[ ${#branch} -gt 20 ]]; then
    branch="${branch:0:20}…"
  fi
  if [[ -n "$branch" ]]; then
    if ! git -C "$cwd" --no-optional-locks diff --quiet 2>/dev/null || \
       ! git -C "$cwd" --no-optional-locks diff --cached --quiet 2>/dev/null; then
      git_info=" on ${YELLOW}${branch}*${RESET}"
    else
      git_info=" on ${YELLOW}${branch}${RESET}"
    fi
  fi
fi

# Contextual info (only shown when relevant)
ctx=""

# Python virtualenv
if [[ -n "${VIRTUAL_ENV:-}" ]]; then
  venv_name=$(basename "$VIRTUAL_ENV")
  ctx="$ctx ${MAGENTA}py:${venv_name}${RESET}"
fi

# Node version (only if .nvmrc or package.json exists)
if [[ -f "$cwd/.nvmrc" ]] || [[ -f "$cwd/package.json" ]]; then
  if command -v node > /dev/null 2>&1; then
    node_ver=$(node -v 2>/dev/null | sed 's/v//')
    ctx="$ctx ${GREEN}node:${node_ver}${RESET}"
  fi
fi

# AWS Profile (only in terraform directories)
if [[ -n "${AWS_PROFILE:-}" ]]; then
  if [[ -f "$cwd/main.tf" ]] || [[ -f "$cwd/terraform.tf" ]] || [[ -d "$cwd/.terraform" ]]; then
    ctx="$ctx ${YELLOW}aws:${AWS_PROFILE}${RESET}"
  fi
fi

# Claude model
if [[ -n "$model" ]]; then
  case "$model" in
    claude-sonnet-4-*) short_model="sonnet" ;;
    claude-opus-4-*)   short_model="opus" ;;
    *sonnet*)          short_model="sonnet" ;;
    *opus*)            short_model="opus" ;;
    *haiku*)           short_model="haiku" ;;
    *)                 short_model="$model" ;;
  esac
  ctx="$ctx ${MAGENTA}🤖 ${short_model}${RESET}"
fi

# Add separator if we have context
if [[ -n "$ctx" ]]; then
  ctx=" |$ctx"
fi

# Output the status line
# Only show user@ if running as different user than logged in
if [[ "$current_user" != "$logged_in_user" ]]; then
  printf "%s%s@%s%s in %s%s%s%s%s" "$GREEN" "$current_user" "$host" "$RESET" "$CYAN" "$dir" "$RESET" "$git_info" "$ctx"
else
  printf "%s%s%s in %s%s%s%s%s" "$GREEN" "$host" "$RESET" "$CYAN" "$dir" "$RESET" "$git_info" "$ctx"
fi
