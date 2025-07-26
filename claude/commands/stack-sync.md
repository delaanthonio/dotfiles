Synchronize and update the current stacked PR workflow.

Steps:
1. Run `git town sync` to update all branches
2. Check for merge conflicts and resolve them
3. Update PR descriptions with current status
4. Run tests on each branch in the stack
5. Update commit messages to follow conventional commits
6. Push all changes and update PRs
7. Watch for new GitHub Action workflows via gh to confirm the build passes
