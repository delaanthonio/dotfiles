Create a Linear ticket with comprehensive motivation and acceptance criteria for the engineering team.

Analyze the request: $ARGUMENTS

**Determine Ticket Structure:**
- If the request is complex or has multiple distinct components, create a parent ticket with sub-issues
- If the request is focused on a single feature/fix, create one comprehensive ticket
- Look for keywords like "epic", "project", "system", "multiple", or naturally separable work items

Structure the Linear ticket with:

**Title**: Clear, descriptive title summarizing the work

**Description** should include:

**## Motivation**
- Why is this change needed?
- What problem does it solve?
- What is the business or technical impact?
- Who are the stakeholders affected?

**## Background Context**
- Current state of the system/feature
- Any relevant technical debt or constraints
- Related tickets or previous work
- Dependencies or integration points

**## Acceptance Criteria**
- [ ] Specific, testable requirements (use checkboxes)
- [ ] Functional requirements (what should work)
- [ ] Non-functional requirements (performance, security, etc.)
- [ ] Edge cases and error handling
- [ ] Documentation updates needed
- [ ] Testing requirements

**## Technical Notes**
- Suggested implementation approach (if applicable)
- Potential risks or considerations
- Impact on other systems or teams

**## Definition of Done**
- [ ] Code implemented and reviewed
- [ ] Tests written and passing
- [ ] Documentation updated
- [ ] Deployed to staging/production
- [ ] Stakeholders notified

**For Single Tickets:**
Use the mcp__linear__create_issue tool to create one comprehensive ticket with:
- Appropriate team assignment
- Priority level based on urgency/impact
- Labels for categorization
- Due date if time-sensitive

**For Parent Ticket with Sub-Issues:**
1. Create the parent ticket using mcp__linear__create_issue with:
   - High-level overview and motivation
   - Overall acceptance criteria
   - Project-level labels and timeline

2. Create sub-issues using mcp__linear__create_issue with:
   - parentId set to the parent ticket ID
   - Specific component focus (e.g., "Backend API", "Frontend UI", "Database Migration")
   - Detailed acceptance criteria for that component
   - Individual estimates and assignments

**Example Sub-Issue Breakdown:**
- **Backend**: API endpoints and business logic
- **Frontend**: User interface and interactions  
- **Database**: Schema changes and migrations
- **Testing**: Integration and end-to-end tests
- **Documentation**: User guides and technical docs

After creation, provide all Linear ticket URLs for team reference, showing the parent-child relationship.