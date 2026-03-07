---
name: review-coderabbit
description: "Fast CodeRabbit-driven review with context detection and safe auto-fix"
---

# CodeRabbit Review Command

Fast, targeted code review with automatic safe fixes and multi-layered safety validation.

## Usage

```
/review-coderabbit [options]
```

## Options

- `--uncommitted` - Review uncommitted changes (default: auto-detect)
- `--branch` - Review branch vs main (default: auto-detect)
- `--base <branch>` - Custom base branch for comparison (default: main, fallback: master)
- `--pr <number>` - Review PR by number (requires gh CLI and authentication)
- `--no-auto-fix` - Report findings only, don't apply auto-fixes
- `--tests <cmd>` - Custom test command (e.g., `npm run test:unit`)
- `--skip-tests` - Skip test validation entirely
- `--max-fixes <n>` - Stop after applying N fixes (default: unlimited)
- `--debug` - Show internal JSON categorization and debug output

## Examples

**Auto-detect scope (recommended)**
```
/review-coderabbit
```

**Review uncommitted changes**
```
/review-coderabbit --uncommitted
```

**Review branch with custom base**
```
/review-coderabbit --branch --base develop
```

**Report-only mode (no fixes)**
```
/review-coderabbit --no-auto-fix
```

**Custom test command**
```
/review-coderabbit --tests "npm run test:unit"
```

**Review with limits**
```
/review-coderabbit --max-fixes 10 --skip-tests
```

## How It Works

### Phase 0: Context Detection
Auto-detects scope (uncommitted, branch, PR) based on git state, or uses explicit flags.

### Phase 1: CodeRabbit Execution
Runs CodeRabbit CLI with appropriate scope flags and error handling.

### Phase 2: Finding Categorization
Uses LLM to categorize findings into:
- **Auto-fixable**: formatting, null guards, types, explicit returns
- **Requires Approval**: logic changes, schema, auth, breaking changes
- **Informational**: optimizations, suggestions

### Phase 3: Safety Validation & Snapshot
Creates reversible baseline (stash -u), detects test/lint commands, checks repo safety.

### Phase 4: Apply Auto-fixes
Applies auto-fixable findings one-by-one with:
- Syntax validation (best effort)
- Batch validation (lint, typecheck, tests)
- Atomic rollback if validation fails

### Phase 5: Present Approval Items
Shows findings requiring human approval, grouped by risk level.

### Phase 6: Summary Report
Markdown report with:
- Auto-fixed items
- Approval-required items with risk assessment
- Validation status (tests, hooks)
- Next steps

## Safety Guarantees

1. **Reversible baseline**: All uncommitted changes stashed before any fixes applied
2. **No semantics changes without approval**: Auto-fixes limited to formatting, types, null guards, logging
3. **Atomic rollback**: If any validation fails, all fixes rolled back and findings presented as approval-required

## Integration with `/code-review`

- **This command** (`/review-coderabbit`): Fast auto-fix focused, immediate feedback, minimal scoring
- **`/code-review` command**: Comprehensive analysis, detailed scoring, manual review, specialized agents

Use this for rapid WIP feedback; use `/code-review` for production readiness reviews.

---

## Execution

Delegate to coderabbit agent with parsed arguments:

Parse arguments from `$ARGUMENTS`:
- Extract scope flags: `--uncommitted`, `--branch`, `--pr <num>`, `--base <branch>`
- Extract behavior flags: `--no-auto-fix`, `--skip-tests`, `--max-fixes <n>`, `--debug`
- Extract override flags: `--tests <cmd>`

Invoke coderabbit agent:

```
Task(
  subagent_type="coderabbit",
  prompt="CodeRabbit review with: $PARSED_ARGS"
)
```

The agent will:
1. Parse and validate arguments
2. Execute 6-phase workflow
3. Report findings and applied fixes
4. Provide next steps for approval-required items
