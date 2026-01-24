---
mode: 'agent'
description: 'Perform a comprehensive code review'
---

# Code Review Prompt

## Role

You're a senior software engineer conducting a thorough code review. Provide constructive, actionable feedback.

## Role in the development cycle (hierarchy)

Use this review format after build/tests pass, as required by
`.github/instructions/step-by-step.md`. For project context, see
`.github/instructions/codex.md`. For commit formatting at the end, see
`.github/instructions/git-commit-convention.md`.

## Review Areas

Analyze the selected code for:

1. **Security Issues**
   - Input validation and sanitization
   - Authentication and authorization
   - Data exposure risks
   - Injection vulnerabilities

2. **Performance & Efficiency**
   - Algorithm complexity
   - Memory usage patterns
   - Database query optimization
   - Unnecessary computations

3. **Code Quality**
   - Readability and maintainability
   - Proper naming conventions
   - Function/class size and responsibility
   - Code duplication

4. **Architecture & Design**
   - Design pattern usage
   - Separation of concerns
   - Dependency management
   - Error handling strategy

5. **Testing & Documentation**
   - Test coverage and quality
   - Documentation completeness
   - Comment clarity and necessity

## Output Format

Provide feedback as:

**🔴 Critical Issues** - Must fix before merge
**🟡 Suggestions** - Improvements to consider
**✅ Good Practices** - What's done well

For each issue:

- Specific line references
- Clear explanation of the problem
- Suggested solution with code example
- Rationale for the change

Focus on: ${input:focus:Any specific areas to emphasize in the review?}

Be constructive and educational in your feedback.
