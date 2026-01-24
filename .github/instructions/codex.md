# Codex Agent Instructions

You are working inside the Fable compiler repository.

## Role in the development cycle (hierarchy)

This file provides project context and mission guidance. It is subordinate to the workflow rules in
`.github/instructions/step-by-step.md` and should be used alongside:

- `.github/instructions/beast-mode.md` for execution mode.
- `.github/instructions/code-review-prompt.md` for review formatting.
- `.github/instructions/git-commit-convention.md` for commit message structure.

## Project context

- Language: F# (.NET)
- Purpose: Compile F# to multiple target languages
- Existing backends: JS, Python, Rust, Dart
- Architecture: FCS → Fable AST → target emitters

## Your mission

- Explore feasibility of adding a Swift backend
- Compare against Fable2Rust and Fable2Dart
- Prefer minimal, domain-programming subset

## Rules

- Do not break existing targets
- Do not change public APIs without explanation
- All changes must be incremental and documented
- Keep a TODO list at all times

## Output expectations

- Written analysis
- File-level pointers
- Step-by-step implementation plan
