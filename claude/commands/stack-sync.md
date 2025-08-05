Synchronize and update the current stacked PR workflow.

Steps:
1. Run `gt sync` to update all branches and pull latest trunk
2. Check for merge conflicts and resolve them interactively
3. Use `gt restack` if needed to rebase entire stack
4. Run tests on each branch in the stack
5. Update commit messages using `gt modify` if needed
6. Push all changes with `gt submit --no-interactive`
7. Watch for new GitHub Action workflows via gh to confirm the build passes
