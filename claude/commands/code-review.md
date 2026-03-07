---
name: code-review
description: "Comprehensive code review analyzing UX, reliability, clarity, testing, and production readiness with structured scoring"
---

# Code Review

Perform a systematic 3-phase code review with optional auto-fixes.

**For detailed checklists, scoring rules, and agent dispatch guidance, see the `code-review` skill.**

---

## Scope Interpretation

Parse arguments: $ARGUMENTS

- No arguments → Review uncommitted changes (`git diff`)
- `--branch` → Review branch changes (`git diff main` or `git diff default`)
- `--pr <number>` → Review pull request by number
- `<file/path>` → Review specific files or paths
- `--no-auto-fix` → Skip automatic fixes in Phase 3
- `--depth <fast|standard|deep>` → Override auto-detected depth
- URL → Extract PR number from GitHub URL

---

## Execution Flow

### Step 0: Context Detection
Run context detection from skill:
1. Determine review target (uncommitted, branch, PR, pasted)
2. Determine size (file count, LOC changed)
3. Run sensitivity detection
4. Auto-select depth (Fast/Standard/Deep) or use `--depth` flag

### Step 1: Create Todo List
Track progress with 3 phases:
1. Phase 1: Analysis (8 domain todos: Silent Failures, Breaking Changes, UX, Testing, Type Safety, Clarity, Docs, Accessibility)
2. Phase 2: Scoring & Verdict
3. Phase 3: Action (Categorize, Auto-fix, Present for approval, Implement approved)

---

## Phase 1: Code Analysis

Analyze changes using checklists from the `code-review` skill based on detected depth:
- **Fast**: Critical only (21 items) - Silent Failures, Breaking Changes, API Compatibility
- **Standard**: Critical + High (40 items) - Add UX, Testing, Type Safety
- **Deep**: All categories (55+ items) - Add Clarity, Docs, Accessibility

Mark each todo as `in_progress` → `completed` as you analyze each domain.

Document all findings with `file:line` references.

---

## Phase 2: Scoring & Verdict

Score each domain using scoring rules from the `code-review` skill:
- **Silent Failure Prevention**: X/10
- **Breaking Changes**: ✅/❌
- **UX & Accessibility**: X/5 (Standard+)
- **Test Quality**: X/5 (Standard+)
- **Code Clarity**: X/5 (Deep only)

Apply verdict rules from skill:
- **BLOCKED**: Any breaking change unmitigated OR Silent Failure ≤5 OR Security issues OR Test Quality = 0
- **NEEDS WORK**: Silent Failure 6-7 OR any domain <3
- **READY**: Silent Failure ≥8 AND Breaking Changes clear AND all domains ≥3

---

## Phase 3: Action

Skip if `--no-auto-fix` flag present.

### Categorize Findings

**Auto-Fixable** (apply without approval):
- Formatting, typos
- Adding null guards
- Adding logging/surfacing
- Adding explicit error returns
- Adding type annotations
- Small refactors (rename only)

**Requires Approval**:
- Retries, fallbacks, control flow changes
- Schema changes
- Auth/permission changes
- Anything affecting money/security
- Breaking API changes

### Apply Auto-Fixes

1. Verify tests exist and pass
2. Apply auto-fixes systematically (use Edit tool)
3. Re-run tests after each fix
4. Rollback if tests fail
5. Show summary of applied fixes

### Present Complex Items

For items requiring approval, present by category:
```
Category: Reliability & Silent Failures (3 items)
⚠️ May require architectural changes

1. Add error handling to payment API - payments/api.ts:45
   → May need: Retry logic, circuit breaker, or error boundary

Would you like me to attempt these, or handle them separately?
```

Allow approval per category or per item.

### Implement Approved Changes

1. Apply fixes systematically
2. Run tests after each category
3. Ask user if tests fail (rollback, debug, or skip)
4. Show completion summary

---

## Output Format

Use the standard output template from the `code-review` skill:

```markdown
## Code Review: [scope]

**Date**: YYYY-MM-DD
**Scope**: [uncommitted | branch | PR #X | files]
**Depth**: [Fast | Standard | Deep]
**Files**: X files, Y insertions, Z deletions

**Reviewed**: [what was checked]
**Not reviewed**: [explicit gaps]

---

### Summary
- [Top finding 1]
- [Top finding 2]
- [Top finding 3 or positive note]

### Verdict: [READY | NEEDS WORK | BLOCKED]
[1-2 sentence justification]

---

### Scores

| Domain | Score | Notes |
|--------|-------|-------|
| Silent Failure Prevention | X/10 | [note] |
| Breaking Changes | ✅/❌ | [note] |
| UX & Accessibility | X/5 | [note] |
| Test Quality | X/5 | [note] |
| Code Clarity | X/5 | [note] |

---

### Issues

**Blocking**:
1. [Issue] - file:line - [category]

**High Priority**:
1. [Issue] - file:line - [category]

**Medium**:
1. [Issue] - file:line - [category]

---

### Suggested Fixes
1. [Fix] - file:line

### Test Plan
- [ ] [Test to run or write]

### Follow-ups
- [Future improvement]
```

---

## Agent Dispatch

If depth is **Deep** or sensitivity detection triggered, consider spawning specialized agents:

Use dispatch rules from `code-review` skill:
- **Max agents**: 2 per review
- **Priority**: security > reliability > regress > ux > tester > clarity

Spawn using Task tool:
```
subagent_type: "security"  # or "reliable", "ux", "tester", "regress", "clarity"
prompt: "Review these changes for [focus]: <diff summary>"
```

---

## Usage Examples

```bash
/code-review                          # Uncommitted changes, auto-fix
/code-review --branch                 # Branch diff vs main
/code-review --pr 123                 # PR #123
/code-review src/api/users.ts         # Specific files
/code-review --no-auto-fix            # Analysis only
/code-review --depth deep             # Force deep review
/code-review --branch --no-auto-fix   # Branch review, no fixes
```

---

## Reference

For detailed checklists, scoring criteria, and agent dispatch rules, see:
- Skill: `code-review` (`claude/skills/code-review/SKILL.md`)
- Agents: `security`, `reliable`, `ux`, `tester`, `regress`, `clarity`
