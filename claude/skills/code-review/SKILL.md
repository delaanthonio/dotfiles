---
name: code-review
description: "Comprehensive code review using CodeRabbit CLI, structured checklists, and specialized agents. Use when reviewing code changes, checking PR readiness, or when the user mentions code review."
allowed-tools: Read, Bash, Glob, Grep, Task
---

# Code Review Skill

Comprehensive code review orchestrating CodeRabbit CLI, structured checklists, and specialized agents.

## Skill vs Command

- **This skill** (`code-review`): Activated automatically when user asks for code review
- **The command** (`/code-review`): Explicit invocation with arguments

Both use the same underlying methodology; the skill provides guidance, the command provides direct execution.

---

## Context Detection (Step 0)

Before choosing a review approach, determine what you're reviewing:

### Determine Review Target (in order of precedence)

1. **PR link/number provided** → Fetch PR diff (see PR Diff Strategy)
2. **Git repo with dirty working tree** → `--type uncommitted`
3. **Git repo with commits ahead of base** → `--type committed` or `--base main`
4. **Pasted code/diff** → Bypass CodeRabbit CLI, do checklist-only review
5. **Specific files provided** → Review those files only

### PR Diff Strategy

```bash
# Check if gh is available and authenticated
if command -v gh &>/dev/null && gh auth status &>/dev/null; then
  gh pr diff <number> --patch > /tmp/pr-diff.patch
  # OR for file list only:
  gh pr view <number> --json files -q '.files[].path'
else
  # Fallback: instruct user
  echo "Run: git fetch origin pull/<number>/head:pr-<number> && git diff main...pr-<number>"
  # OR ask user to paste diff
fi
```

### Determine Size

```bash
# File count
git diff --name-only | wc -l

# Summary (insertions/deletions)
git diff --shortstat
# Output: "X files changed, Y insertions(+), Z deletions(-)"
```

- **Small**: <5 files, <100 LOC
- **Medium**: 5-20 files, 100-500 LOC
- **Large**: >20 files or >500 LOC

### Sensitivity Detection

```bash
# Check if diff touches sensitive paths/keywords
git diff --name-only | grep -iE '(auth|billing|payment|stripe|webhook|permission|admin|crypto|secret|token|credential|\.env)'

# OR grep diff content for common secrets:
git diff | grep -iE '(password|api.?key|secret|token|bearer|credential)'

# High-signal secret patterns (less false positives):
git diff | grep -E '(AKIA[0-9A-Z]{16}|-----BEGIN (RSA|EC|OPENSSH) PRIVATE KEY-----|xox[baprs]-|ghp_|github_pat_)'
```

**If sensitive patterns found** → force **Deep** depth.

### Failure Mode

If any detection command fails, fall back to **checklist-only review** and ask user to paste diff.

---

## Depth Selection

Auto-select based on context, allow user override:

| Depth | When to Use | What's Included |
|-------|-------------|-----------------|
| **Fast** | WIP, quick feedback, <5 files | Critical checklists only + top 3 issues |
| **Standard** | Pre-PR, branch review | Critical + High priority + scoring + verdict |
| **Deep** | Production release, sensitive code | All categories + agents + test plan audit |

### Default Selection

- Uncommitted changes → **Fast**
- Branch comparison → **Standard**
- >20 files → **Deep**
- Sensitivity detection triggered → **Deep** (override)

---

## Quick Reviews with CodeRabbit

### Prerequisites

```bash
# Verify git repo
git rev-parse --is-inside-work-tree

# Check for configs (CodeRabbit auto-detects these if present)
ls -la .coderabbit.yaml CLAUDE.md 2>/dev/null
```

### Review Commands

```bash
# Quick WIP feedback
coderabbit review --type uncommitted --plain

# Pre-PR against main
coderabbit review --base main --plain

# With explicit config (if auto-detect fails)
coderabbit review --base main --plain --config .coderabbit.yaml
```

**Note**: CodeRabbit auto-detects `.coderabbit.yaml` and `CLAUDE.md` in repo root. Only use `--config` flag if auto-detection fails.

---

## Comprehensive Review (3-Phase)

### Phase 1: Analysis Checklists

Checklist depth by mode:
- **Fast**: Critical only (21 items)
- **Standard**: Critical + High (40 items)
- **Deep**: All categories (55+ items)

#### Critical - Silent Failure Prevention (always check)

- [ ] DB writes verify affected rows / returned object / error state
- [ ] API calls verify status code or success indicator
- [ ] No empty catch blocks
- [ ] All async operations have error handlers
- [ ] File/cache writes verify success before proceeding
- [ ] No swallowed errors (catch blocks log or re-throw)
- [ ] No fire-and-forget async calls (promises/tasks awaited or errors handled)
- [ ] Third-party calls (payment, email, auth) confirm success
- [ ] State mutations verify success
- [ ] Return values from critical operations checked

#### Critical - Breaking Changes (always check)

- [ ] No function signature changes without migration
- [ ] No removed/renamed exports from public APIs
- [ ] No database column renames/deletions without migration
- [ ] No URL/route changes
- [ ] No config/env variable renames without docs
- [ ] Request/response format changes have migration path

#### Critical - API/Schema Compatibility (always check)

- [ ] Public exports unchanged or deprecated properly
- [ ] API response shape unchanged or versioned
- [ ] Migrations have rollback plan
- [ ] Feature flags for risky changes
- [ ] Deprecation notes in changelog

#### High Priority - UX (Standard+)

- [ ] Keyboard navigation works
- [ ] Error messages user-friendly
- [ ] Loading states shown
- [ ] Form validation clear
- [ ] No destructive actions without confirmation
- [ ] Empty states provide guidance
- [ ] Focus management in modals
- [ ] Mobile touch targets ≥44px

#### High Priority - Test Coverage (Standard+)

- [ ] Edge cases tested (null, empty, boundary, 0, -1, MAX_INT)
- [ ] Error paths have tests
- [ ] No flaky patterns (setTimeout, hardcoded delays)
- [ ] Complex logic uses parameterized tests
- [ ] Async operations tested for timeouts
- [ ] Invalid input types tested

#### High Priority - Type Safety (Standard+)

- [ ] No type escapes without justification
- [ ] Null/undefined handled explicitly
- [ ] Function params and return types explicit
- [ ] Union types narrowed before use
- [ ] No implicit type coercion

#### Medium - Code Clarity (Deep only)

- [ ] Function names verb-based
- [ ] No magic numbers
- [ ] Variables descriptive
- [ ] Booleans use is/has/can/should prefixes
- [ ] Functions <50 lines
- [ ] Parameter count ≤4 or use object

#### Medium - Documentation (Deep only)

- [ ] README updated for new dependencies
- [ ] Docstrings for new public functions
- [ ] API docs reflect changes
- [ ] Config docs updated

#### Medium - Accessibility (Deep only)

- [ ] Images have alt text
- [ ] ARIA labels present
- [ ] Color contrast meets WCAG AA
- [ ] Semantic HTML used
- [ ] Focus indicators visible

---

### Phase 2: Scoring & Verdict

#### Scoring Rules

**Silent Failure Prevention (X/10)**

| Score | Criteria |
|-------|----------|
| 10 | All operations verify success, comprehensive error handling, logging, metrics |
| 7-9 | Minor gaps but failures visible, most operations checked |
| 4-6 | Meaningful silent failure risk, some unchecked operations |
| 1-3 | Failures likely invisible, multiple unchecked paths |
| 0 | Data loss possible, critical operations unchecked |

**UX & Accessibility (X/5)**

| Score | Criteria |
|-------|----------|
| 5 | Full keyboard nav, error states, loading states, WCAG AA |
| 3-4 | Most items covered, 1-2 minor gaps |
| 1-2 | Missing critical UX patterns |
| 0 | No UX consideration |

**Test Quality (X/5)**

| Score | Criteria |
|-------|----------|
| 5 | Edge cases, error paths, no flaky patterns, parameterized |
| 3-4 | Good coverage, minor gaps |
| 1-2 | Only happy path |
| 0 | No tests or widespread flaky patterns |

**Code Clarity (X/5)**

| Score | Criteria |
|-------|----------|
| 5 | Clear naming, documented, type-safe |
| 3-4 | Mostly clear, minor issues |
| 1-2 | Confusing names, poor types |
| 0 | Unreadable |

#### Verdict Rules

**BLOCKED** if any:
- Any breaking change unmitigated
- Silent Failure Prevention ≤5
- Security issues found (if security agent ran)
- Test Quality = 0 on non-trivial changes

**NEEDS WORK** if any:
- Silent Failure Prevention 6-7
- Any domain score <3

**READY** if all:
- Silent Failure Prevention ≥8
- Breaking Changes mitigated (✅)
- All scored domains ≥3

---

### Phase 3: Action

#### Auto-fix Guardrails

| Allowed (no approval) | Not Allowed (requires approval) |
|-----------------------|--------------------------------|
| Formatting, typos | Behavior changes |
| Adding null guards | Retries, fallbacks, different control flow |
| Adding logging/surfacing | Schema changes |
| Adding explicit error returns | Auth/permission changes |
| Adding type annotations | Anything affecting money/security |
| Small refactors (rename only) | Breaking API changes |

**Key rule**: Do not introduce retries, fallbacks, or different control flow without approval. That's where "helpful" fixes turn into subtle bugs.

#### Before Any Auto-fix

```bash
# Run tests relevant to changes
npm test -- --findRelatedTests <changed-files>
# OR
pytest <changed-files>
```

---

## Agent Dispatch

### Agent IDs (canonical)

- `security` - Vulnerabilities, secrets, OWASP
- `reliable` - Silent failures, resilience patterns
- `ux` - Accessibility, user flows
- `tester` - Edge cases, flaky tests
- `regress` - Breaking changes, contracts
- `clarity` - Naming, documentation

### Dispatch Conditions

| Agent | Dispatch When |
|-------|---------------|
| `security` | Sensitivity detection triggered OR auth/payment/secrets in diff |
| `reliable` | DB/repository/client files changed AND depth=Standard+ |
| `ux` | UI files changed AND (depth=Deep OR user asked for UX review) |
| `tester` | Test files changed OR coverage gaps identified |
| `regress` | types/, schemas/, openapi, exports, public API touched |
| `clarity` | >200 LOC changed in single file AND depth=Deep |

### Dispatch Rules

- **Max agents**: 2 per review (avoid noise)
- **Priority order**: security > reliable > regress > ux > tester > clarity
- **Override**: Sensitivity detection → always spawn `security` first
- **Large PRs** (>30 files): Default to `reliable` + `regress`

### Spawning Syntax

```
Task tool:
  subagent_type: "security"  # or "reliable", "ux", "tester", "regress", "clarity"
  prompt: "Review these changes for [focus area]: <diff summary or file list>"
```

---

## Standard Output Template

Every review produces this structure:

```markdown
## Code Review: [scope]

**Date**: YYYY-MM-DD
**Scope**: [uncommitted | branch | PR #X | files]
**Depth**: [Fast | Standard | Deep]
**Files**: X files, Y insertions, Z deletions

**Reviewed**: [what was actually checked - e.g., "uncommitted diff against HEAD, 8 files"]
**Not reviewed**: [explicit gaps - e.g., "runtime behavior, staging deploy, performance benchmarks"]

---

### Summary
- [Bullet 1: Most important finding]
- [Bullet 2: Second finding]
- [Bullet 3: Third finding or positive note]

### Verdict: [READY | NEEDS WORK | BLOCKED]
[1-2 sentence justification]

---

### Scores

| Domain | Score | Notes |
|--------|-------|-------|
| Silent Failure Prevention | X/10 | [brief note] |
| Breaking Changes | ✅/❌ | [brief note] |
| UX & Accessibility | X/5 | [brief note] |
| Test Quality | X/5 | [brief note] |
| Code Clarity | X/5 | [brief note] |

---

### Issues

**Blocking** (must fix before merge):
1. [Issue] - file:line - [category]

**High Priority** (should fix):
1. [Issue] - file:line - [category]

**Medium** (nice to have):
1. [Issue] - file:line - [category]

---

### Suggested Fixes
1. [Fix description] - file:line
2. [Fix description] - file:line

### Test Plan
- [ ] [Test to run or write]
- [ ] [Test to run or write]

### Follow-ups (optional)
- [Future improvement not blocking this PR]
```

---

## Recommended Workflows

### Quick Feedback Loop
```bash
coderabbit review --type uncommitted --plain
```
For rapid WIP feedback during development.

### Pre-PR Review
```bash
# CodeRabbit + Phase 1-2 checklist
coderabbit review --base main --plain
# Then run through Critical and High Priority checklists
```

### Production Readiness
```bash
# Full 3-phase + specialized agents
# 1. CodeRabbit scan
coderabbit review --base main --plain

# 2. Comprehensive checklist (all categories)
# 3. Spawn agents based on dispatch rules
# 4. Aggregate findings and produce standard output
```

### Security Audit
```bash
# Security agent + CodeRabbit
# 1. Run sensitivity detection
# 2. Spawn security agent
# 3. Run CodeRabbit with security focus
# 4. Combine findings
```
