Check all PRs in the current stack for automated agent comments and respond appropriately:

1. List all branches in the current stack using git town status
2. For each PR in the stack, check for comments from automated agents (bots, CodeRabbit, SonarQube, etc.)
3. Identify any unaddressed agent feedback
4. Prioritize critical issues (security, bugs) over style issues
5. For each agent comment requiring action:
   - Implement the suggested fix
   - Verify it doesn't break stack dependencies
   - Test the specific layer (uv run pytest for Python, pnpm test for TypeScript)
   - Update dependent PRs if needed

Provide a summary of actions taken and any agent feedback that requires manual review.