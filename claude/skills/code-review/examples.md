# Code Review Examples & References

## Pattern Index
Quick links: [Silent Failures](#silent-failures) | [Transactions](#transactions-partial-failures) | [Breaking Changes](#breaking-changes) | [Security & PII](#security--pii) | [UX](#ux-patterns) | [Type Safety](#type-safety) | [Usage](#coderabbit-usage-examples)

---

## Code Patterns

### Silent Failures

**Empty catch (bad)**
```typescript
try {
  await saveUser(user);
} catch (e) {
  /* silent */
}
```

**Visible failure (good)** - show options:
```typescript
// Option A: Return explicit result
const result = await saveUser(user);
if (!result.ok) {
  logger.error('Save failed', { userId: user.id, error: result.error });
  return { success: false, error: result.error };
}

// Option B: Throw with logging
try {
  await saveUser(user);
} catch (e) {
  logger.error('Save failed', { userId: user.id, error: e });
  throw e; // Re-throw after logging
}
```

**Fire-and-forget (bad)**
```typescript
sendEmail(user.email, template); // no await, no catch
```

**Awaited with visibility (good)**
```typescript
const sent = await sendEmail(user.email, template).catch(e => {
  logger.error('Email failed', { to: user.email, error: e });
  return false;
});
// Caller decides what to do with `sent === false`
```

---

### Transactions & Partial Failures

**Partial batch update (bad)**
```typescript
for (const item of items) {
  await updateItem(item); // Some succeed, some fail - inconsistent state
}
```

**Transaction boundary (good)**
```typescript
await db.transaction(async (tx) => {
  for (const item of items) {
    await tx.updateItem(item); // All or nothing
  }
});
// Use your DB/ORM's transaction primitive; goal is all-or-nothing on batch updates.
```

**Idempotency for webhooks (good)**
```typescript
const processed = await checkIdempotencyKey(event.id);
if (processed) return { status: 'already_processed' };

// Process event...
await markProcessed(event.id);
// Implementation should be atomic (unique constraint on event.id to prevent races)
```

---

### Breaking Changes

**Removed export (bad)**
```typescript
// Before: export function calculateTotal(items)
// After: (deleted) - breaks all callers
```

**Deprecation path (good)**
```typescript
/** @deprecated Use calculateOrderTotal. Removed in v3.0. */
export function calculateTotal(items: Item[]): number {
  return calculateOrderTotal({ items });
}
```

---

### Security & PII

**Logging secrets (bad)**
```typescript
logger.info('Auth attempt', { email, password, token }); // PII + secrets!
```

**Safe logging (good)**
```typescript
logger.info('Auth attempt', {
  email: maskEmail(email),  // user***@example.com
  hasToken: !!token         // boolean only
});
```

**SQL injection (bad)**
```typescript
const query = `SELECT * FROM users WHERE email = '${email}'`;
```

**Parameterized query (good)**
```typescript
const query = 'SELECT * FROM users WHERE email = ?';
db.execute(query, [email]);
```

---

### UX Patterns

**Destructive without confirm (bad)**
```tsx
<button onClick={deleteAccount}>Delete Account</button>
```

**With confirmation (good)**
```tsx
<button onClick={() => setShowConfirm(true)}>Delete Account</button>
{showConfirm && (
  <ConfirmDialog
    title="Delete account?"
    message="This cannot be undone."
    onConfirm={deleteAccount}
    onCancel={() => setShowConfirm(false)}
  />
)}
```

**No loading state (bad)**
```tsx
<button onClick={handleSubmit}>Submit</button>
```

**With loading state (good)**
```tsx
<button onClick={handleSubmit} disabled={isLoading}>
  {isLoading ? 'Submitting...' : 'Submit'}
</button>
```

---

### Type Safety

**Unnarrowed union (bad)**
```typescript
function process(input: string | null) {
  return input.toUpperCase(); // null crash
}
```

**Type guard (good)**
```typescript
function process(input: string | null): string {
  if (input === null) return '';
  return input.toUpperCase();
}
```

**Type escape (bad)**
```typescript
const data = JSON.parse(response) as MyType; // unsafe cast
```

**Runtime validation (good)**
```typescript
const data = JSON.parse(response);
if (!isMyType(data)) {
  throw new Error('Invalid response shape');
}
return data;
```

---

## Review Comment Templates

Copy-paste for common feedback:

**Silent failure**:
> This call can fail silently. Please either: (1) return an explicit error result, (2) throw after logging, or (3) add metrics/alerting for failure visibility.

**Breaking change**:
> This changes a public contract. Please add deprecation notice with migration path, or version the endpoint.

**Missing test**:
> This needs a test for the error path. Example: what happens when [X] fails?

**PII exposure**:
> This logs potentially sensitive data. Please mask or remove: [field].

**No loading state**:
> Add loading state to prevent double-submissions and provide user feedback during async operations.

**Type escape**:
> This type assertion bypasses compile-time checks. Please add runtime validation or narrow the type properly.

---

## Reference Links

### Security
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [OWASP Cheat Sheets](https://cheatsheetseries.owasp.org/)
- [CWE Database](https://cwe.mitre.org/)

### Accessibility
- [WCAG Quick Reference](https://www.w3.org/WAI/WCAG21/quickref/)
- [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/)
- [A11y Project](https://www.a11yproject.com/)

### Official Docs
- [MDN Web Docs](https://developer.mozilla.org/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/)

---

## Tool Lookup Guide

### Context7 (if available)
Check if Context7 MCP tools are connected before using.

```
# Lookup framework docs
mcp__context7__resolve-library-id("react error boundaries")
mcp__context7__get-library-docs("/facebook/react", topic="error boundaries")
```

Common lookups:
- React: `/facebook/react`
- Next.js: `/vercel/next.js`
- Express: `/expressjs/express`
- pytest: `/pytest-dev/pytest`
- Django: `/django/django`

### WebSearch (if available)
Use for version-specific issues, security vulns, latest guidelines.

Example queries:
- "[framework] [version] [issue] site:github.com"
- "CVE [library] [year]"
- "OWASP [topic] latest"

---

## CodeRabbit Usage Examples

### Example 1: Quick WIP Review
```bash
# Check what's changed
git status

# Run quick review
coderabbit review --type uncommitted --plain
```

### Example 2: Pre-PR Branch Review
```bash
# Check current branch
git branch -vv

# Compare against main
coderabbit review --base main --plain
```

### Example 3: Review with Configs
```bash
# Check for config files
ls .coderabbit.yaml CLAUDE.md 2>/dev/null

# Run with explicit configs (if auto-detect fails)
coderabbit review --base main --plain --config .coderabbit.yaml
```

### Example 4: Choosing Review Type Based on Git State
```bash
# If there are uncommitted changes, review those
if [ -n "$(git status --porcelain)" ]; then
  coderabbit review --type uncommitted --plain
# If on a feature branch, compare to main
elif [ "$(git branch --show-current)" != "main" ]; then
  coderabbit review --base main --plain
# Otherwise, review recent commits
else
  coderabbit review --type committed --plain
fi
```

---

## Maintenance Rule

**Hard cap**: This file must stay under 300 lines.

**When adding examples**: Remove or consolidate an existing one. Prefer high-signal patterns that appear frequently in reviews.
