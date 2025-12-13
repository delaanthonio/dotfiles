#!/usr/bin/env bash
set -euo pipefail

# Agent Build Script
# Combines Claude agent bodies with OpenCode frontmatter templates
# Usage: ./build-agents.sh [agent_name] [--dry-run] [--verbose]

# Configuration
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CLAUDE_AGENTS_DIR="${DOTFILES_DIR}/claude/agents"
TEMPLATES_DIR="${DOTFILES_DIR}/agents/opencode-templates"
GENERATED_DIR="${DOTFILES_DIR}/config/opencode/agents"

# Model mapping
declare -A MODEL_MAP=(
  [opus]="anthropic/claude-opus-4-20250514"
  [sonnet]="anthropic/claude-sonnet-4-20250514"
  [haiku]="anthropic/claude-haiku-4-20250514"
)

# Internal agent name mapping (filename -> internal_name)
declare -A AGENT_NAMES=(
  [architect]="architect"
  [astro]="astro"
  [builder]="builder"
  [clarity]="clarity"
  [code-architect]="code-architect"
  [content]="content"
  [devops-expert]="devops-expert"
  [django]="django"
  [docs]="docs"
  [dotfiles-expert]="dotfiles-expert"
  [email]="email"
  [intel]="intel"
  [monitor]="monitor"
  [next]="next"
  [observability]="observe"
  [ops]="ops"
  [pystyle]="pystyle"
  [regress]="regress"
  [reliability]="reliable"
  [reviewer]="review"
  [roadmap]="roadmap"
  [security]="security"
  [seo]="seo"
  [social-media-manager]="social"
  [stories]="stories"
  [tester]="tester"
  [ux]="ux"
)

# Flags
DRY_RUN=false
VERBOSE=false
CHECK_MODE=false
SPECIFIC_AGENT=""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
  echo -e "${BLUE}ℹ${NC}  $*"
}

log_success() {
  echo -e "${GREEN}✅${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}⚠️${NC}  $*"
}

log_error() {
  echo -e "${RED}❌${NC} $*"
}

# Parse arguments
parse_args() {
  while [[ $# -gt 0 ]]; do
    case $1 in
      --dry-run)
        DRY_RUN=true
        shift
        ;;
      --verbose)
        VERBOSE=true
        shift
        ;;
      --check)
        CHECK_MODE=true
        shift
        ;;
      --help)
        print_usage
        exit 0
        ;;
      -*)
        log_error "Unknown option: $1"
        print_usage
        exit 1
        ;;
      *)
        SPECIFIC_AGENT="$1"
        shift
        ;;
    esac
  done
}

print_usage() {
  cat << 'EOF'
Agent Build Script

Usage:
  build-agents.sh [options] [agent_name]

Options:
  --dry-run    Show what would change without building
  --verbose    Show detailed output
  --check      Check if rebuilds needed (exit code 2 if out of sync)
  --help       Show this help message

Examples:
  build-agents.sh              # Build all agents
  build-agents.sh reviewer     # Build specific agent
  build-agents.sh --dry-run    # Show what would change
  build-agents.sh --verbose    # Detailed output
EOF
}

# Convert shorthand model name to full name
convert_model_name() {
  local model="$1"
  # Remove 'anthropic/' prefix if present
  model="${model#anthropic/}"
  # Check if it's a shorthand
  if [[ -v MODEL_MAP[$model] ]]; then
    echo "${MODEL_MAP[$model]}"
  else
    # Return as-is if not shorthand
    echo "$model"
  fi
}

# Extract agent body (everything after second ---)
extract_body() {
  local file="$1"
  
  # Find the line numbers of --- separators
  local separator_lines=()
  local line_num=0
  
  while IFS= read -r line; do
    ((line_num++))
    if [[ "$line" == "---" ]]; then
      separator_lines+=("$line_num")
    fi
  done < "$file"
  
  # We need at least 2 separators (start and end of frontmatter)
  if [[ ${#separator_lines[@]} -lt 2 ]]; then
    return 1
  fi
  
  # Get the line after the second separator
  local start_line=$((${separator_lines[1]} + 1))
  
  # Extract everything after the second ---
  if [[ $start_line -gt 0 ]]; then
    tail -n +$((start_line)) "$file"
  fi
}

# Validate YAML syntax
validate_yaml() {
  local file="$1"
  
  if ! command -v yq &> /dev/null; then
    # If yq not available, do basic validation
    if grep -q '^[a-z_]*:' "$file"; then
      return 0
    else
      return 1
    fi
  fi
  
  yq eval . "$file" > /dev/null 2>&1
}

# Build a single agent
build_agent() {
  local agent_file="$1"
  local agent_name="${AGENT_NAMES[$agent_file]}"
  local claude_file="${CLAUDE_AGENTS_DIR}/${agent_file}.md"
  local template_file="${TEMPLATES_DIR}/${agent_file}.yaml"
  local output_file="${GENERATED_DIR}/${agent_name}.md"
  local temp_file="${output_file}.tmp.$$"
  
  [[ $VERBOSE == true ]] && log_info "Building agent: $agent_name (from $agent_file)"
  
  # Check if Claude agent exists
  if [[ ! -f "$claude_file" ]]; then
    log_error "Claude agent not found: $claude_file"
    return 1
  fi
  
  # Check if template exists
  if [[ ! -f "$template_file" ]]; then
    log_warn "No OpenCode template for '$agent_file', skipping"
    return 0
  fi
  
  # Validate template YAML
  if ! validate_yaml "$template_file"; then
    log_error "Invalid YAML in template: $template_file"
    return 1
  fi
  
  # Extract body from Claude agent
  local body
  body=$(extract_body "$claude_file")
  
  if [[ -z "$body" ]]; then
    log_error "Failed to extract body from $claude_file"
    return 1
  fi
  
  # Read template and convert model names
  local template_content
  template_content=$(cat "$template_file")
  
  # Convert shorthand model names in template
  local model_line
  model_line=$(echo "$template_content" | grep '^model:')
  if [[ -n "$model_line" ]]; then
    local shorthand_model
    shorthand_model=$(echo "$model_line" | sed 's/^model: *//')
    local full_model
    full_model=$(convert_model_name "$shorthand_model")
    
    if [[ "$shorthand_model" != "$full_model" ]]; then
      [[ $VERBOSE == true ]] && log_info "Converting model: $shorthand_model → $full_model"
      template_content=$(echo "$template_content" | sed "s|^model: .*|model: $full_model|")
    fi
  fi
  
  # Generate frontmatter
  local frontmatter="---\n${template_content}\n---\n"
  
  # Combine frontmatter and body
  local full_content="${frontmatter}${body}"
  
  # Write to temp file
  echo -e "$full_content" > "$temp_file"
  
  # Atomic rename (temp -> output)
  mv "$temp_file" "$output_file"
  
  log_success "Built $agent_name"
  return 0
}

# Build all agents or specific agent
build_agents() {
  local agents_to_build=()
  local failed_builds=()
  local skipped_builds=()
  
  if [[ -n "$SPECIFIC_AGENT" ]]; then
    agents_to_build=("$SPECIFIC_AGENT")
  else
    # Get all agent filenames
    for file in "$CLAUDE_AGENTS_DIR"/*.md; do
      agents_to_build+=("$(basename "$file" .md)")
    done
  fi
  
  if [[ $DRY_RUN == true ]]; then
    log_info "DRY RUN - showing what would be built:"
    echo ""
  fi
  
  for agent_file in "${agents_to_build[@]}"; do
    if [[ -v AGENT_NAMES[$agent_file] ]]; then
      if [[ $DRY_RUN == true ]]; then
        local agent_name="${AGENT_NAMES[$agent_file]}"
        echo "  • $agent_file → $agent_name"
      else
        if build_agent "$agent_file"; then
          true  # Already logged by build_agent
        else
          failed_builds+=("$agent_file")
        fi
      fi
    else
      log_warn "Unknown agent: $agent_file"
    fi
  done
  
  echo ""
  
  if [[ $DRY_RUN == true ]]; then
    log_info "To execute, run: ./build-agents.sh"
    return 0
  fi
  
  # Summary
  local total=${#agents_to_build[@]}
  local success=$((total - ${#failed_builds[@]}))
  
  if [[ ${#failed_builds[@]} -eq 0 ]]; then
    log_success "Successfully built all $total agents"
    return 0
  else
    log_error "Build failed for: ${failed_builds[*]}"
    log_error "Built $success/$total agents"
    return 1
  fi
}

# Check if agents are out of sync
check_sync() {
  local out_of_sync=()
  
  for agent_file in "$CLAUDE_AGENTS_DIR"/*.md; do
    local filename=$(basename "$agent_file" .md)
    if [[ ! -v AGENT_NAMES[$filename] ]]; then
      continue
    fi
    
    local agent_name="${AGENT_NAMES[$filename]}"
    local output_file="${GENERATED_DIR}/${agent_name}.md"
    
    if [[ ! -f "$output_file" ]]; then
      out_of_sync+=("$filename")
    fi
  done
  
  if [[ ${#out_of_sync[@]} -gt 0 ]]; then
    log_warn "Out of sync: ${out_of_sync[*]}"
    return 2
  else
    log_success "All agents in sync"
    return 0
  fi
}

# Main
main() {
  parse_args "$@"
  
  # Create output directory if it doesn't exist
  mkdir -p "$GENERATED_DIR"
  
  if [[ $CHECK_MODE == true ]]; then
    check_sync
    return $?
  fi
  
  build_agents
}

main "$@"
