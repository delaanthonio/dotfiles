# Dual-Format Agent System

This system manages agents for both **Claude Code** and **OpenCode** platforms, sharing agent content while supporting platform-specific configurations.

## Architecture

### Source of Truth
- **`claude/agents/*.md`** - Claude agent definitions (source of truth)
  - Contains: name, description, tools, model, agent body content
  - Example: `claude/agents/reviewer.md`

### Platform-Specific Configuration
- **`agents/opencode-templates/*.yaml`** - OpenCode frontmatter templates
  - One template per agent with OpenCode-specific settings
  - Contains: description, mode, model, temperature, tools
  - Example: `agents/opencode-templates/reviewer.yaml`

### Generated Artifacts
- **`agents/generated/opencode/*.md`** - Generated OpenCode agents
  - Auto-created by build script: frontmatter + body
  - Tracked in git for consistency across machines
  - Symlinked to `~/.config/opencode/agents/`

## Directory Structure

```
.dotfiles/
├── claude/agents/              # Source of truth (27 agents)
│   ├── reviewer.md
│   ├── security.md
│   └── ...
│
├── agents/
│   ├── opencode-templates/     # OpenCode configs (27 templates)
│   │   ├── reviewer.yaml
│   │   ├── security.yaml
│   │   └── ...
│   │
│   ├── generated/
│   │   └── opencode/           # Generated agents (27 files, auto-created)
│   │       ├── review.md       # (internal name: review)
│   │       ├── security.md
│   │       └── ...
│   │
│   └── scripts/
│       └── build-agents.sh     # Build script
│
└── .git/hooks/
    └── pre-commit              # Auto-rebuild on commit
```

## Agent Name Mapping

The system handles internal name differences between Claude and OpenCode:

| File | Claude Name | OpenCode Name |
|------|------------|---------------|
| reviewer.md | review | review |
| reliability.md | reliable | reliable |
| observability.md | observe | observe |
| social-media-manager.md | social | social |
| tester.md | tester | tester |

All other agents use the filename (without .md) as their internal name.

## Build Script

### Usage

```bash
# Build all agents
./agents/scripts/build-agents.sh

# Build specific agent
./agents/scripts/build-agents.sh reviewer

# Dry run (show what would change)
./agents/scripts/build-agents.sh --dry-run

# Verbose output
./agents/scripts/build-agents.sh --verbose

# Check if rebuild needed
./agents/scripts/build-agents.sh --check
```

### Features
- ✅ Extracts body from Claude agents
- ✅ Reads OpenCode templates
- ✅ Converts shorthand model names (opus → anthropic/claude-opus-4-20250514)
- ✅ Generates OpenCode agents (frontmatter + body)
- ✅ Atomic writes (no partial files)
- ✅ Skips missing templates with warning (non-blocking)
- ✅ Colored output for status visibility
- ✅ Exit codes for scripting

### Model Name Conversion

Shorthand model names in templates are automatically converted:

```yaml
# In template
model: sonnet
# Becomes in generated agent
model: anthropic/claude-sonnet-4-20250514
```

Supported mappings:
- `opus` → `anthropic/claude-opus-4-20250514`
- `sonnet` → `anthropic/claude-sonnet-4-20250514`
- `haiku` → `anthropic/claude-haiku-4-20250514`

## Git Hook

### Automatic Rebuild on Commit

The `.git/hooks/pre-commit` hook automatically:

1. Detects if you've edited Claude agents or OpenCode templates
2. Rebuilds affected agents
3. Auto-stages generated files
4. Continues with commit if successful
5. Aborts commit if build fails

### Workflow Example

```bash
# Edit Claude agent (source of truth)
vim claude/agents/security.md

# Stage and commit
git add claude/agents/security.md
git commit -m "Update security checklist"

# → Pre-commit hook automatically:
#   - Rebuilds agents/generated/opencode/security.md
#   - Stages the generated file
#   - Both files included in commit
```

### Benefits
- ✅ Never out-of-sync
- ✅ Automatic regeneration
- ✅ Clean git history
- ✅ No manual rebuild needed

## OpenCode Templates

### Template Format

Minimal YAML configuration per agent:

```yaml
description: "Agent description..."
mode: subagent  # or main
model: sonnet   # shorthand or full name
temperature: 0.1  # one decimal place
tools:
  write: true
  edit: true
  bash: true
```

### Agent Modes

- **`subagent`** - Specialized review/analysis agents
  - Examples: reviewer, security, reliability, qa, regress
  - Deterministic, read-only analysis
  - Temperature: 0.0-0.2

- **`main`** - Implementation, planning, and content agents
  - Examples: builder, django, next, content, roadmap
  - Can write code or create content
  - Temperature: 0.2-0.5

### Temperature Guidelines

- **0.0** - Deterministic (security scanning, style enforcement)
- **0.1** - Low creativity (code review, testing)
- **0.2** - Implementation (code generation)
- **0.3** - Planning (architecture, strategy)
- **0.4** - Creative (content, marketing)
- **0.5** - High creativity (social media)

### Tool Restrictions

Configure which tools each agent can access:

```yaml
tools:
  write: false  # Create new files
  edit: false   # Edit existing files
  bash: true    # Execute bash commands
```

Common patterns:
- **Review agents**: `write: false, edit: false` - Read-only
- **Implementation agents**: `write: true, edit: true` - Full access
- **Analysis agents**: `bash: true` - Need command execution

## Workflow Examples

### Editing a Claude Agent

```bash
# 1. Edit source
vim claude/agents/clarity.md

# 2. Stage and commit
git add claude/agents/clarity.md
git commit -m "Improve code review checklist"

# → Hook auto-rebuilds and stages OpenCode agent
# → Both files committed together
```

### Changing OpenCode Configuration

```bash
# 1. Edit template
vim agents/opencode-templates/reliability.yaml

# 2. Adjust temperature, mode, or tools as needed
# 3. Stage and commit
git add agents/opencode-templates/reliability.yaml
git commit -m "Lower reliability agent temperature for consistency"

# → Hook auto-rebuilds affected agent
# → Generated file automatically staged
```

### Adding a New Agent

```bash
# 1. Create Claude agent
vim claude/agents/my-agent.md
# Add frontmatter with name, description, model, tools

# 2. Create OpenCode template
vim agents/opencode-templates/my-agent.yaml
# Add description, mode, model, temperature, tools

# 3. Initial build
./agents/scripts/build-agents.sh my-agent

# 4. Update symlinks (if needed)
./install

# 5. Commit all files
git add claude/agents/my-agent.md \
        agents/opencode-templates/my-agent.yaml \
        agents/generated/opencode/my-agent.md
git commit -m "feat: add my-agent"
```

### Manual Rebuild (if needed)

```bash
# Rebuild all agents
./agents/scripts/build-agents.sh

# See what would change
./agents/scripts/build-agents.sh --dry-run

# Debug specific agent
./agents/scripts/build-agents.sh --verbose reviewer
```

## Verification

### Check Generated Files

```bash
# List all generated OpenCode agents
ls -la agents/generated/opencode/

# View agent count
ls agents/generated/opencode/ | wc -l

# Check specific agent
head -15 agents/generated/opencode/review.md
```

### Verify Symlinks

```bash
# Check OpenCode agents directory
ls -la ~/.config/opencode/agents/

# Verify symlink target
ls -L ~/.config/opencode/agents/review.md
```

### Test Build Script

```bash
# Dry run to see what would build
./agents/scripts/build-agents.sh --dry-run

# Check sync status
./agents/scripts/build-agents.sh --check

# Rebuild with verbose output
./agents/scripts/build-agents.sh --verbose
```

## Troubleshooting

### Build Errors

**Issue:** `Error: Invalid YAML in template`
- **Solution:** Check YAML syntax in `agents/opencode-templates/{agent}.yaml`

**Issue:** `Warning: No OpenCode template for 'new-agent'`
- **Solution:** Create `agents/opencode-templates/new-agent.yaml`

**Issue:** `Failed to extract body from agent`
- **Solution:** Ensure Claude agent has proper frontmatter (two `---` separators)

### Git Hook Issues

**Issue:** `Commit aborted` after pre-commit hook
- **Check:** Run `./agents/scripts/build-agents.sh --check` manually
- **Fix:** Address any build errors, then retry commit

**Issue:** Generated files not staged
- **Solution:** Manually run `git add agents/generated/opencode/*.md`

### OpenCode Agent Issues

**Issue:** OpenCode doesn't recognize agent
- **Check:** Verify symlink exists: `ls -la ~/.config/opencode/agents/`
- **Check:** Verify file has correct frontmatter with `description:` and `mode:`

**Issue:** Agent has wrong settings
- **Fix:** Edit `agents/opencode-templates/{agent}.yaml`
- **Rebuild:** Run `./agents/scripts/build-agents.sh {agent}`
- **Stage:** `git add agents/generated/opencode/{agent}.md`

## Complete Agent Configuration

### Review/Analysis Agents (Subagents)
- clarity, monitor, observability, pystyle, qa, regress, reliability, reviewer, security, ux

### Implementation Agents (Main)
- astro, builder, code-architect, devops-expert, django, dotfiles-expert, next

### Planning/Strategy Agents (Main)
- architect, ops, roadmap, stories

### Content/Marketing Agents (Main)
- content, docs, email, seo, social-media-manager

### Research Agents (Main)
- intel

## References

- **Build Script:** `agents/scripts/build-agents.sh`
- **Git Hook:** `.git/hooks/pre-commit`
- **Dotbot Config:** `install.conf.yaml` (OpenCode symlink section)
- **Claude Agents:** `claude/agents/` directory
- **OpenCode Templates:** `agents/opencode-templates/` directory
- **Generated Agents:** `agents/generated/opencode/` directory

## Summary

This system provides:

✅ **Single source of truth** - Claude agents are authoritative  
✅ **Automatic synchronization** - Git hook rebuilds on changes  
✅ **Platform-specific configs** - Each platform gets optimal settings  
✅ **Shared content** - Agent bodies reused perfectly  
✅ **Clean git history** - Generated files tracked, no conflicts  
✅ **Easy maintenance** - Simple scripts, clear structure  
✅ **Scalable** - Add new agents or platforms easily  

## Quick Commands

```bash
# Rebuild all agents
./agents/scripts/build-agents.sh

# Check what changed
./agents/scripts/build-agents.sh --dry-run

# Rebuild specific agent
./agents/scripts/build-agents.sh security

# Manual commit (hook auto-rebuilds)
git add claude/agents/reviewer.md
git commit -m "Update reviewer agent"
```
