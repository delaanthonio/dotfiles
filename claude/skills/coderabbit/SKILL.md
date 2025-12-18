---
name: coderabbit
description: "AI-powered code review using CodeRabbit CLI. Use when reviewing code changes, checking PR readiness, getting feedback on staged/committed changes, or when the user mentions code review, CodeRabbit, or wants their changes reviewed."
allowed-tools: Read, Bash, Glob, Grep
---

# CodeRabbit Code Review

Use the CodeRabbit CLI to provide AI-driven code reviews for local changes.

## When to Use This Skill

Activate this skill when the user:
- Asks for a code review of their changes
- Wants to check if their code is PR-ready
- Mentions "CodeRabbit" or "coderabbit"
- Wants feedback on staged, committed, or all changes
- Needs to compare their branch against a base branch

## Prerequisites

Before running CodeRabbit:
1. Ensure you're in a git repository
2. Check for config files to pass to CodeRabbit

## Auto-Detect Configuration

Check for and use these config files if present:
- `.coderabbit.yaml` - CodeRabbit-specific configuration
- `CLAUDE.md` - Project instructions that provide context

Build the config flag:
```bash
# Check what configs exist
ls -la .coderabbit.yaml CLAUDE.md 2>/dev/null

# If configs exist, include them with -c flag
coderabbit review -c .coderabbit.yaml CLAUDE.md
```

## Review Types

### Review All Changes (Default)
```bash
coderabbit review --type all
```

### Review Only Committed Changes
Use when reviewing what's already been committed but not pushed:
```bash
coderabbit review --type committed
```

### Review Only Uncommitted Changes
Use during active development to get feedback on work-in-progress:
```bash
coderabbit review --type uncommitted
```

## PR-Style Reviews

### Compare Against Base Branch
Use when preparing a PR or reviewing branch changes:
```bash
coderabbit review --base main
coderabbit review --base origin/main
```

### Compare Against Specific Commit
Use for incremental reviews:
```bash
coderabbit review --base-commit abc1234
```

## Output Modes

### Interactive Mode (Default)
Best for exploration and discussion:
```bash
coderabbit review
```

### Plain Text Mode
Best for capturing output or non-interactive environments:
```bash
coderabbit review --plain
```

### Prompt-Only Mode
Get the AI review prompts to analyze or fix issues yourself:
```bash
coderabbit review --prompt-only
```

## Recommended Workflows

### Pre-PR Review
```bash
# Get comprehensive review comparing to main branch
coderabbit review --base main --plain
```

### Development Feedback Loop
```bash
# Quick review of uncommitted changes
coderabbit review --type uncommitted --plain
```

### Review and Fix
```bash
# Get prompts, then fix issues
coderabbit review --prompt-only > /tmp/review.txt
# Read the review and address issues
```

## Building Commands

Always check context first:
1. Run `git status` to understand the repository state
2. Check for `.coderabbit.yaml` and `CLAUDE.md`
3. Select appropriate review type based on user intent
4. Choose output mode based on whether fixes are needed

Example command construction:
```bash
# Full command with auto-detected config and base branch comparison
coderabbit review \
  --base main \
  --type all \
  --plain \
  -c .coderabbit.yaml CLAUDE.md
```
