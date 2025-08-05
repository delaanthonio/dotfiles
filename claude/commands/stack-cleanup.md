Clean up completed or abandoned stacked PRs:

1. Identify merged or closed PRs in the stack
2. Use gt sync to automatically:
   - Pull latest trunk changes
   - Rebase open PRs on latest changes
   - Prompt to delete stale/merged branches
3. For manual cleanup if needed:
   - Use git branch -d for safe deletion
   - Use gt restack to fix stack relationships
4. Remove any orphaned branches after confirmation
5. Use gt state to verify stack status
6. Provide summary of cleanup actions taken

Be conservative and ask for confirmation before deleting branches.