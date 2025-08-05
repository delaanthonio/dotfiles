Review the current stacked PR setup for consistency and best practices:

1. Check that each branch in the stack has focused, single-responsibility changes
2. Verify proper base branch relationships using gt state
3. Analyze commit messages for clarity and convention adherence
4. Check for potential conflicts between stack layers
5. Ensure proper test coverage exists for each layer
6. Validate that our uv/python and pnpm/typescript dependencies are handled correctly
7. Suggest improvements for the stack structure

Provide specific Graphite commands to fix any issues found:
- gt modify to amend current branch
- gt restack to rebase the entire stack
- gt checkout to switch between branches