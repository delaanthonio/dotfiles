Use the Task tool to delegate to agents for planning and implementing a PR stack.

Workflow:
1. If implementation approach is unclear, first invoke implementation-planner agent to:
   - Analyze feature and propose 3 implementation approaches
   - Provide recommendation with trade-offs
   - Get user confirmation on approach

2. Then invoke implementation-planner agent to:
   - Take the chosen approach for: $ARGUMENTS
   - Break it down into a logical stack of 1-5 focused PRs
   - Create detailed implementation plan
   - Present the plan for your confirmation

3. Upon approval, invoke stack-implementer agent to:
   - Execute the implementation plan
   - Create branches with Graphite
   - Submit the stack and provide GitHub PR links

This leverages specialized sub-agents with clear separation of concerns.
