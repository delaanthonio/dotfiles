Execute a complete end-to-end implementation workflow with iterative cycles, built-in error recovery, and production-grade quality gates.

**Prerequisites:** Architecture should be defined using `/plan-architecture` command and Linear tickets created before using this workflow.

Use the TodoWrite tool to track the overall workflow, then delegate to specialized agents:

**Phase 1: Implementation Planning**

**If `--simple` flag is present:**
Use the Task tool to delegate to the **implementation-planner** agent with `--quick-mode`:
- Analyze the implementation request: $ARGUMENTS
- Use streamlined planning approach (single approach, <500 words)
- Focus on PR breakdown, dependencies, and implementation strategy
- Output: Quick Implementation Plan
- **Skip checkpoint approval** - proceed directly to Phase 2

**If `--complex` flag is present or default:**
Use the Task tool to delegate to the **implementation-planner** agent:
- Analyze the implementation request: $ARGUMENTS
- Validate requirements completeness, edge cases, and dependencies
- Review existing codebase patterns and conventions
- Generate 3 distinct implementation approaches for the defined architecture
- Recommend optimal implementation strategy with technical justification
- Break down recommended approach into logical, reviewable PR chunks
- Plan PR stack structure with clear scope, dependencies, and sequence
- Define implementation success criteria and validation strategy
- Output: Complete Implementation Plan with approach analysis and PR stack

**🛑 CHECKPOINT: Implementation Plan Approval** (Complex mode only)
Present the Complete Implementation Plan to the user:
- "Please review the proposed implementation approach and PR structure" 
- "Do you have questions about the technical strategy or PR breakdown?"
- "Would you like to discuss implementation trade-offs or adjust the plan?"

**Wait for explicit user approval before proceeding to implementation.**

**Phase 2: Iterative Implementation & Review Cycles**
For each PR in the PR Stack Plan, execute this tight feedback loop:

**Error Recovery:** At any point, if implementation gets blocked:
- **stack-implementer** has built-in unblocking procedures (self-diagnosis → peer consultation → strategic escalation → user escalation)
- Can dynamically split complex PRs using built-in splitting criteria
- Can rollback individual PRs without affecting the stack

**2a. Single PR Implementation**
Use the Task tool to delegate to the **stack-implementer** agent:
- Include the current PR specification from the Implementation Plan
- Implement code, tests, and documentation for THIS PR ONLY
- Follow established conventions and the recommended approach
- Handle git operations (stage, commit, create branch)
- Keep changes focused and reviewable

**2b. Immediate PR Review**
Use the Task tool to delegate to the **parallel-reviewer** agent:
- Review ONLY the current PR (not the entire stack)
- **Intelligent review dispatch**: Analyzes changed files to run only relevant specialist reviewers
- **Always dispatched**: stack-reviewer (code quality/pragmatism) and test-specialist (test coverage)
- **Conditionally dispatched** based on file changes:
  - **security-auditor**: Dependencies, config files, auth code, database queries
  - **performance-analyzer**: Database queries, loops, I/O operations, data processing
  - **observability-expert**: API endpoints, error handling, external service calls
- Run linting, type checking, and tests for this PR
- Provide immediate, focused feedback with dispatch efficiency metrics

**2c. PR Refinement**
If issues found in review:
- Use **stack-implementer** again to address specific feedback
- Use `gt modify` to update the current PR
- Re-run **parallel-reviewer** if significant changes made
- Continue until PR passes all quality gates
- **Escalation**: If refinement cycles exceed 3 iterations, escalate to user for guidance

**2d. PR Completion**
- Mark current PR as complete and ready for merge
- Move to next PR in the Implementation Plan
- Repeat cycle 2a-2d until all PRs completed

**Phase 3: Stack Submission**
- Run `/stack-submit-check` to verify submission readiness
- Submit the entire stack using `gt submit --no-interactive`
- **Post-submission validation**: 
  - Verify successful submission and provide all PR URLs
  - Run any post-deployment smoke tests if applicable
  - Validate final integration and functionality
- **Documentation**: Provide comprehensive summary of delivered changes with:
  - Feature overview and architecture decisions
  - Testing approach and coverage achieved
  - Known limitations or follow-up work needed
  - Migration or deployment considerations

**Key Benefits:**
- **Flexible Complexity**: `--simple` for quick tasks, `--complex` for comprehensive planning
- **Shift-Left Quality**: Issues caught early in small, focused PRs (both modes)
- **Fast Feedback**: Immediate review after each PR implementation  
- **Reviewability**: Small, logical changes easier to review effectively
- **Velocity**: Rapid iteration cycles with targeted improvements
- **Risk Reduction**: Problems isolated to individual PRs, not entire features
- **Error Recovery**: Built-in unblocking and rollback mechanisms
- **Quality Assurance**: Comprehensive testing and validation at every layer (both modes)
- **Production Ready**: Post-submission validation and documentation
- **Resource Efficiency**: Intelligent review dispatch optimizes agent utilization

**Workflow Customization:**
Add flags after $ARGUMENTS to customize behavior:

**Complexity Modes:**
- `--simple`: Streamlined workflow for simple, well-defined tasks
  - Quick planning (single approach, <500 words)
  - Skip checkpoint approval
  - 1-3 PRs maximum
  - Same quality gates maintained
- `--complex` (default): Full comprehensive workflow with extensive planning

**Other Options:**
- `--skip-checkpoint`: Skip implementation plan approval (use carefully)
- `--test-strategy=pyramid|trophy`: Override default testing approach
- `--max-refinement-cycles=N`: Change escalation threshold (default: 3)

**Usage Examples:**
```bash
# Simple bug fix or small feature
/execute-full-workflow --simple Fix user registration validation error

# Complex feature requiring planning
/execute-full-workflow --complex Implement multi-tenant authentication system

# Default (complex mode)
/execute-full-workflow Add real-time notifications system
```