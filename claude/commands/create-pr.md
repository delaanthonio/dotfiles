Create a pull request for the current branch with proper context and formatting:

1. Check the current branch and ensure it's not main/master
2. Get the branch name and determine the base branch (usually main)
3. Check if there are uncommitted changes and prompt to commit them
4. For single PRs: use gh pr create
5. For stacked PRs: use gt submit --no-interactive to submit the entire stack
6. Generate an appropriate PR title based on:
   - Branch name patterns (feature/, fix/, chore/, etc.)
   - Recent commit messages
   - Changed files analysis
7. Create a comprehensive PR description including:
   - Summary of changes
   - Type of change (feature, bugfix, refactor, etc.)
   - Testing instructions
   - Related issues (if any)
   - Checklist for reviewers
   - Stack visualization (if using Graphite)
8. Set appropriate labels based on file changes and branch naming
9. Assign reviewers if configured
10. Provide the PR URL for easy access

Consider the dotfiles repository context and common development patterns.