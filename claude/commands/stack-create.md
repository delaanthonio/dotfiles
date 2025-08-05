Create a new feature stack for: $ARGUMENTS

Follow these steps:
1. Analyze the feature requirements
2. Present the proposed stack structure and get user confirmation
3. For each branch in the stack:
   - Write the code changes first (before creating branch)
   - Stage changes with git add
   - Create branch with gt create --message "descriptive commit message"
4. Generate appropriate branch names following our naming convention
5. Ensure each branch has a clear, single responsibility
6. After all branches created, submit stack with gt submit --no-interactive

The Graphite workflow: write code → stage → create branch → repeat for each PR → submit all.

