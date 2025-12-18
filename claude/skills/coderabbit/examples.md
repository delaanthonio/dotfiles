# CodeRabbit Skill Examples

## Example 1: Quick Development Review

**User says:** "Review my changes"

**Claude does:**
```bash
# Check git status
git status

# Check for config files
ls .coderabbit.yaml CLAUDE.md 2>/dev/null

# Run review with detected configs
coderabbit review --type uncommitted --plain
```

## Example 2: PR Readiness Check

**User says:** "Is my branch ready for PR?"

**Claude does:**
```bash
# Check current branch and its upstream
git branch -vv

# Compare against main with full review
coderabbit review --base main --plain -c CLAUDE.md
```

## Example 3: Review and Fix Issues

**User says:** "Review my code and fix any issues"

**Claude does:**
```bash
# Get review prompts
coderabbit review --prompt-only > /tmp/coderabbit-review.txt

# Read and analyze the review
cat /tmp/coderabbit-review.txt

# Then fix issues based on the feedback
```

## Example 4: Review Specific Commit Range

**User says:** "Review what I've done since yesterday's commit"

**Claude does:**
```bash
# Find yesterday's commit
git log --since="yesterday" --oneline

# Review from that commit
coderabbit review --base-commit <commit-hash> --plain
```

## Example 5: Incremental Review

**User says:** "Just review my staged changes"

**Claude does:**
```bash
# Show what's staged
git diff --staged --stat

# Review only uncommitted (includes staged)
coderabbit review --type uncommitted --plain
```

## Example 6: Full Repository Review

**User says:** "Do a comprehensive code review"

**Claude does:**
```bash
# Full review with all available context
coderabbit review \
  --type all \
  --plain \
  -c .coderabbit.yaml CLAUDE.md 2>/dev/null || coderabbit review --type all --plain
```

## Common Patterns

### Detecting Available Configs
```bash
configs=""
[ -f .coderabbit.yaml ] && configs="$configs .coderabbit.yaml"
[ -f CLAUDE.md ] && configs="$configs CLAUDE.md"
[ -n "$configs" ] && echo "Using configs:$configs"
```

### Choosing Review Type Based on Git State
```bash
# If there are uncommitted changes, review those
if [ -n "$(git status --porcelain)" ]; then
  coderabbit review --type uncommitted --plain
# If on a feature branch, compare to main
elif [ "$(git branch --show-current)" != "main" ]; then
  coderabbit review --base main --plain
# Otherwise, review recent commits
else
  coderabbit review --type committed --plain
fi
```
