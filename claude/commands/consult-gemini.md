Craft a context-aware prompt for Gemini AI based on the current conversation and user's request.

Analyze the conversation context, including:
- Current task or problem being discussed
- Technical details and constraints mentioned
- User's specific question: $ARGUMENTS

Then formulate a comprehensive prompt that includes:
1. Relevant background context from the conversation
2. Specific technical details or code snippets if applicable
3. The user's question or request for advice
4. Request for external perspective on trade-offs, alternatives, or best practices
5. Instruction to "Please search Google if you need current information or documentation"

Use the Bash tool to run from fish shell: fish -c 'gemini -p "your_crafted_prompt_here"'

This provides contextual external insights from Gemini AI with Google search capability for up-to-date information on architectural decisions, code review, or technical questions.
