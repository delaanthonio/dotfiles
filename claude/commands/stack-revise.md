---
name: stack-revise
description: "Apply ad-hoc revisions to existing PR stacks based on custom feedback and instructions"
---

Apply targeted revisions to an existing PR stack based on your feedback, review comments, or custom instructions.

## Usage

Use this command when you need to:
- Incorporate code review feedback across the stack
- Apply custom styling or architectural changes
- Fix issues discovered after initial implementation
- Refactor based on new insights or requirements
- Make targeted improvements to specific PRs

## What it does

1. **Analyzes current stack state** using `gt log` and `gt status`
2. **Parses your revision instructions** to understand scope and requirements
3. **Plans revision strategy** (absorb, split, move, new PR, etc.)
4. **Executes targeted changes** using appropriate Graphite commands
5. **Maintains stack integrity** and test coverage
6. **Provides progress updates** on changes made

## Example Usage

```
/stack-revise Fix variable naming to use camelCase across all PRs in the authentication stack
```

```
/stack-revise Move the database migration to an earlier PR and update the API endpoints to use the new schema
```

```
/stack-revise Apply these code review comments: [paste review feedback]
```

## Input Required

Provide specific instructions about what revisions you want:
- **Scope**: Which PRs or files need changes
- **Requirements**: What specifically needs to be fixed/changed
- **Context**: Any constraints or preferences for the revision approach

## Advanced Usage

The stack-reviser can handle complex scenarios:
- **Cross-stack refactoring**: Changes affecting multiple PRs
- **Stack restructuring**: Reordering, splitting, or merging PRs
- **Architectural adjustments**: Major changes discovered during review
- **Style/convention fixes**: Applying project standards retroactively

## Integration

Works with your existing stack - no need to restart implementation. The reviser will:
- Preserve your commit history where possible
- Maintain PR relationships and dependencies  
- Keep tests passing throughout the revision process
- Coordinate with review agents if needed

This command gives you fine-grained control over stack improvements without disrupting the overall structure.