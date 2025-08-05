---
name: stack-submit-check
description: "Quick pre-submission verification that stack is ready for gt submit based on shift-left quality gates"
---

Perform a lightweight verification that your PR stack is ready for submission.

## Purpose

Since quality is built-in through our shift-left approach, this command simply verifies that all quality gates have been properly executed and the stack is submission-ready.

## What it checks

**✅ Quality Gates Verification**
- All PRs have passing tests
- Linting and formatting completed
- Security checks passed during implementation
- Performance considerations addressed

**✅ Stack Structure Validation**
- Logical PR boundaries maintained
- Dependencies between PRs are correct
- Commit messages are clear and consistent
- No unintended files included

**✅ Submission Readiness**
- Stack is properly rebased on main
- No merge conflicts exist
- All branches are pushed to remote
- Ready for `gt submit --no-interactive`

## Usage

```bash
/stack-submit-check
```

Run this before `gt submit` to get confidence that your stack will submit cleanly.

## What it does

1. **Runs `gt status`** to verify stack health
2. **Checks recent quality gate results** from implementation process
3. **Validates stack structure** using `gt log`
4. **Confirms submission readiness** (no conflicts, branches pushed)
5. **Provides go/no-go decision** with specific issues if any

## Expected Output

```markdown
## 🚀 Stack Submission Check

### Quality Gates Status
✅ All tests passing (verified during implementation)
✅ Code quality gates passed
✅ Security and performance reviewed
✅ Test coverage adequate

### Stack Structure
✅ 4 PRs in logical sequence
✅ Clear commit messages
✅ No merge conflicts
✅ All branches pushed to remote

### Verdict: READY FOR SUBMISSION
Run: `gt submit --no-interactive`
```

## Philosophy

This is **verification, not inspection**. Quality was built-in during the implementation process through:
- Mandatory quality gates in stack-implementer
- Iterative parallel reviews during development
- Comprehensive planning by implementation-planner
- Continuous testing throughout stack creation

The shift-left approach means this check should almost always pass - if it doesn't, something in the upstream process needs attention.

## Integration

Use as the final step before submission:
1. `/execute-full-workflow` → Implementation with built-in quality
2. `/stack-revise` → Any ad-hoc improvements (optional)  
3. `/stack-submit-check` → Quick verification
4. `gt submit --no-interactive` → Submit stack

Lightweight by design - the heavy lifting was done upstream.