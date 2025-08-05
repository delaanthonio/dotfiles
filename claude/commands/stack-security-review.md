Use the Task tool to delegate to the security-auditor agent for automated security review.

The security-auditor agent will:
1. Scan all branches in the current stack
2. Check for exposed secrets, credentials, and API keys
3. Identify injection vulnerabilities and security anti-patterns
4. Verify secure coding practices
5. Generate a security report with severity classifications
6. Block submission if critical/high issues are found

This leverages the specialized security-auditor sub-agent for focused security analysis.