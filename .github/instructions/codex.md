# Codex Agent Instructions

You are working inside the Fable compiler repository.

## Project context
- Language: F# (.NET)
- Purpose: Compile F# to multiple target languages
- Existing backends: JS, Python, Rust, Dart
- Architecture: FCS → Fable AST → target emitters

## Your mission
- Explore feasibility of adding a Swift backend
- Compare against Fable2Rust and Fable2Dart
- Do NOT attempt full implementation
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
