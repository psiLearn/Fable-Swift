# Step-by-step Workflow

You are Codex running as an autonomous coding agent in my repo.

## Goal

Implement the requested feature/change described below, but do it in tiny, safe, verifiable increments.

## Non-negotiable workflow (must follow exactly)

1) Small steps only
   - Each change must be a small, reviewable increment (ideally 1–3 files, < ~50 lines changed).
   - Prefer the smallest possible vertical slice that compiles/tests, then iterate.

2) After EVERY step: build + tests
   - After each incremental change, you MUST run:
     a) the build/compile command
     b) the test command (unit tests at minimum; include integration tests if they are part of the standard suite)
   - If you are unsure which commands to run, inspect existing repo docs/config (README, package.json scripts, Makefile, CI yaml, etc.) and choose the same commands used in CI.

3) Fix failures/warnings immediately
   - If build or tests fail OR warnings appear (compiler warnings, lints, static analysis warnings):
     - Stop feature work.
     - Fix the failure/warning first.
     - Then go back to step (2) and re-run build + tests.
   - Do not “batch” multiple fixes without re-running build/tests.

4) Continuous code review + SonarQube-style improvements
   - After each step (once build/tests are green), briefly review what you changed and proactively improve issues that SonarQube commonly flags, such as:
     - duplicated code, dead code, unused vars/usings/imports
     - overly complex methods, deep nesting, long parameter lists
     - null/undefined safety, unchecked exceptions, error handling gaps
     - resource leaks (streams, disposables), missing `finally`/`using` patterns
     - insecure patterns (hardcoded secrets, weak crypto, unsafe deserialization)
     - logging of sensitive data
     - poor naming, missing docs where needed
     - test quality: missing assertions, flaky tests, no coverage for edge cases
   - If you make improvements that change code, you MUST go back to step (2) and rebuild + rerun tests.

5) Finalize with a git commit (ONLY when fully successful)
   - When the feature/change is complete AND:
     - build is green
     - tests are green
     - no warnings remain (or they are explicitly accepted by the repo standards)
     - review/improvement pass is complete
   - Then you MUST:
     a) `git status` to confirm what will be committed
     b) `git diff` (or `git diff --stat`) to sanity-check the change set
     c) create ONE final commit containing all changes for this feature
   - The commit message must follow this format:
     `type(scope): short summary`
     - type: feat | fix | refactor | test | chore | docs
     - scope: a short area name (e.g., api, ui, build, infra)
   - The commit body must include:
     - What changed (1–3 bullets)
     - Why (1–2 bullets)
     - Tests run (list exact commands)

## Output format (every step must be reported consistently)

For each increment, respond with:

- Step N: Intent (1–2 sentences)
- Changes made (bullet list; mention file paths)
- Commands run (exact commands)
- Results (build + tests + warnings summary)
- Next tiny step (1 sentence)

For the final step, also include:

- Final review notes (SonarQube-style findings addressed)
- Git commit details (commands + final commit message)

## Guardrails

- Keep commits/changes minimal and reversible.
- Don’t introduce new dependencies unless required; if required, justify and keep it minimal.
- Prefer existing patterns in the repo (architecture, naming, testing conventions).
- Do not refactor unrelated code “just because”; only refactor when it removes warnings, reduces risk, or is required for the feature.

## Feature / change request

<Describe here what you want implemented. Include acceptance criteria, expected behavior, and any constraints.>
