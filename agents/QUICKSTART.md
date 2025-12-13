# Quick Start: Dual-Format Agent System

## What Was Built

A system that maintains agents for both **Claude Code** and **OpenCode** platforms:

- **Single source of truth**: `claude/agents/*.md` (27 agents)
- **Platform-specific config**: `agents/opencode-templates/*.yaml` (27 templates)
- **Auto-generated agents**: `config/opencode/agents/*.md` (27 files)
- **Automatic syncing**: Git hook rebuilds agents on commit
- **Symlinked to OpenCode**: `~/.config/opencode/agents/` (27 files)

## Your Daily Workflow

### Editing an Agent

```bash
# 1. Edit Claude agent (source of truth)
vim claude/agents/reviewer.md

# 2. Stage and commit
git add claude/agents/reviewer.md
git commit -m "Update reviewer agent checklist"

# → Git hook AUTOMATICALLY:
#   • Rebuilds config/opencode/agents/review.md
#   • Stages the generated file
#   • Includes both in commit

# → OpenCode picks up changes immediately via symlink
```

### Adjusting OpenCode Settings

```bash
# 1. Edit OpenCode template (adjust mode, temperature, tools)
vim agents/opencode-templates/reliability.yaml

# 2. Stage and commit
git add agents/opencode-templates/reliability.yaml
git commit -m "Lower reliability agent temperature"

# → Git hook auto-rebuilds and stages generated agent
```

### Manual Rebuild (rarely needed)

```bash
# Rebuild all agents
./agents/scripts/build-agents.sh

# Rebuild specific agent
./agents/scripts/build-agents.sh security

# See what would change
./agents/scripts/build-agents.sh --dry-run
```

## Key Features

✅ **Never out of sync** - Git hook ensures automatic regeneration  
✅ **Single source of truth** - Edit Claude agents only  
✅ **Platform-specific configs** - Different settings per platform  
✅ **Shared content** - Agent bodies reused perfectly  
✅ **Clean git history** - Generated files tracked, no conflicts  

## Files You Created

### Build Automation
- `agents/scripts/build-agents.sh` - Build script with all logic
- `.git/hooks/pre-commit` - Git hook for auto-rebuild
- `install.conf.yaml` - Updated with OpenCode symlink config

### Templates (27 files)
- `agents/opencode-templates/*.yaml` - One per agent
- Format: description, mode, model, temperature, tools

### Generated Agents (27 files, auto-created)
- `config/opencode/agents/*.md` - Combined frontmatter + body
- Tracked in git for consistency across machines

## Directory Layout

```
~/.dotfiles/
├── claude/agents/                    # SOURCE - Edit these
│   ├── reviewer.md
│   ├── security.md
│   └── ... (27 total)
│
├── agents/
│   ├── opencode-templates/           # CONFIG - Customize these
│   │   ├── reviewer.yaml
│   │   ├── security.yaml
│   │   └── ... (27 total)
│   │
│   ├── generated/opencode/           # OUTPUT - Auto-generated
│   │   ├── review.md
│   │   ├── security.md
│   │   └── ... (27 total)
│   │
│   └── scripts/
│       └── build-agents.sh           # Build logic
│
└── .git/hooks/
    └── pre-commit                    # Auto-rebuild trigger

~/.config/opencode/agents/            # SYMLINKED - Used by OpenCode
├── review.md -> ~/.dotfiles/config/opencode/agents/review.md
├── security.md -> ~/.dotfiles/config/opencode/agents/security.md
└── ... (27 symlinks)
```

## Agent Name Mapping

Most agents use their filename as the internal name. A few have custom mappings:

| File | Internal Name |
|------|---------------|
| reviewer.md | review |
| tester.md | tester |
| observability.md | observe |
| reliability.md | reliable |
| social-media-manager.md | social |

## Common Tasks

### Update Agent Description
```bash
vim claude/agents/security.md
# Edit description in frontmatter
git add claude/agents/security.md
git commit -m "Update security agent description"
```

### Change Agent Temperature
```bash
vim agents/opencode-templates/reliability.yaml
# Change temperature value
git add agents/opencode-templates/reliability.yaml
git commit -m "Adjust reliability agent temperature"
```

### Add New Agent
```bash
# Create Claude agent
vim claude/agents/new-agent.md
# Add: name, description, model, tools, body

# Create OpenCode template
vim agents/opencode-templates/new-agent.yaml
# Add: description, mode, model, temperature, tools

# Build it
./agents/scripts/build-agents.sh new-agent

# Commit all files
git add claude/agents/new-agent.md \
        agents/opencode-templates/new-agent.yaml \
        config/opencode/agents/new-agent.md
git commit -m "feat: add new-agent"
```

## Verification Commands

```bash
# Check build script is executable
ls -lh agents/scripts/build-agents.sh

# Check git hook is executable
ls -lh .git/hooks/pre-commit

# See all generated OpenCode agents
ls -la config/opencode/agents/

# Verify agent count
ls config/opencode/agents/ | wc -l

# Check OpenCode symlinks
ls ~/.config/opencode/agents/ | wc -l

# View a sample agent
head -15 ~/.config/opencode/agents/review.md
```

## Git Hook Behavior

When you commit changes to agents, the pre-commit hook:

1. **Detects** if you've edited Claude agents or OpenCode templates
2. **Rebuilds** affected agents automatically
3. **Stages** generated files automatically
4. **Continues** commit if successful
5. **Aborts** commit if build fails

### Example

```bash
$ git add claude/agents/reviewer.md
$ git commit -m "Fix: update reviewer checklist"

# Hook runs automatically:
🔄 Rebuilding OpenCode agents for: reviewer
✅ Built review
✅ OpenCode agents rebuilt and staged
[main abc1234] Fix: update reviewer checklist
 2 files changed, 50 insertions(+)
 create mode 100644 config/opencode/agents/review.md
```

## Troubleshooting

### Build fails on commit
```bash
# Check what's wrong
./agents/scripts/build-agents.sh --verbose

# Fix the issue (usually YAML syntax)
vim agents/opencode-templates/{agent}.yaml

# Retry commit
git commit -m "Your message"
```

### Agent not updating in OpenCode
```bash
# Rebuild manually
./agents/scripts/build-agents.sh

# Verify symlink exists
ls -la ~/.config/opencode/agents/review.md

# Restart OpenCode if needed
```

### Missing OpenCode template warning
```bash
# Create the template
vim agents/opencode-templates/my-agent.yaml

# Add required fields:
# description: "..."
# mode: main or subagent
# model: sonnet, opus, or haiku
# temperature: 0.1
# tools: {write: true/false, edit: true/false, bash: true/false}

# Rebuild
./agents/scripts/build-agents.sh
```

## Next Steps

1. ✅ **System is ready to use**
2. ✅ **Edit Claude agents to see auto-generation**
3. ✅ **Adjust OpenCode configs as needed**
4. ✅ **Commit changes normally** (hook handles the rest)

## Documentation

For detailed information, see:
- `agents/README.md` - Complete system documentation
- `agents/scripts/build-agents.sh` - Build script with full logic
- `agents/opencode-templates/` - See template examples

## Support

Common operations:

```bash
# Rebuild everything
./agents/scripts/build-agents.sh

# Check specific agent
./agents/scripts/build-agents.sh reviewer

# See what would change
./agents/scripts/build-agents.sh --dry-run

# Verbose output for debugging
./agents/scripts/build-agents.sh --verbose
```

That's it! The system is fully operational and ready for daily use.
