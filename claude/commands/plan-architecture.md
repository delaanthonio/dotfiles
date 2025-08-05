Define system architecture and break down work into Linear tickets for team execution.

Analyze the request: $ARGUMENTS

**Phase 1: Architectural Analysis**
- Understand the business requirements and constraints
- Research existing system architecture and patterns
- Define high-level system design and component interactions
- Identify data models, API contracts, and integration points
- Consider scalability, security, and performance requirements
- Document architectural decisions and rationale

**Phase 2: Work Breakdown**
- Break down the architecture into logical, deliverable components
- Identify dependencies between components
- Estimate complexity and effort for each component
- Define clear boundaries and interfaces between components
- Consider team expertise and capacity for assignments

**Phase 3: Linear Ticket Creation** 
Use the mcp__linear__create_issue tool to create tickets:

**For Complex Features:** Create parent ticket with sub-issues:
- **Parent Ticket**: High-level feature with overall architecture context
- **Sub-Issues**: Individual components (Backend API, Frontend UI, Database, Testing, Documentation)
- Each sub-issue includes:
  - Specific component focus and acceptance criteria
  - Architectural context and integration requirements
  - Dependencies on other components
  - Technical specifications and constraints

**For Simple Features:** Create single comprehensive ticket with:
- Clear architectural context
- Detailed acceptance criteria
- Technical specifications
- Integration requirements

**Output Deliverables:**
1. **Architecture Document**: System design, components, and decision rationale
2. **Linear Project**: Organized tickets with clear dependencies and assignments
3. **Implementation Readiness**: Architecture defined enough for implementation teams to proceed

This command focuses on the "what" and "why" of the system design, providing a foundation for implementation teams to determine "how" to build it.