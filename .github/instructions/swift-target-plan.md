# Swift Target Completion Plan

## Scope and constraints

- Goal: ship a usable Swift backend for a minimal, domain-programming subset.
- v0 scope: primitives, tuples, records, unions, options/results, lists/arrays, functions, simple modules, if/else, match (basic), loops, basic interop.
- Out of scope for v0: computation expressions, SRTP, reflection, units of measure, dynamic operator overloading, advanced object expressions, broad BCL coverage, async/concurrency.
- Keep Swift toolchain optional; emit-only by default.

## Current status (already in repo)

- Language wiring and CLI dispatch exist. See `src/Fable.AST/Plugins.fs`, `src/Fable.Cli/Entry.fs`, `src/Fable.Cli/Pipeline.fs`.
- Minimal Swift AST, printer, and transform are present. See `src/Fable.Transforms/Swift/Swift.fs`, `src/Fable.Transforms/Swift/SwiftPrinter.fs`, `src/Fable.Transforms/Swift/Fable2Swift.fs`.
- Basic control flow (if/else), subscripts, and helper emission are covered with integration tests. See `tests/Integration/Integration/CliTests.fs`.
- High-level TODOs tracked in `docs/swift-backend-todo.md` and feasibility notes in `docs/swift-backend-feasibility.md`.

## Missing work (summary)

- Swift replacements and intrinsic mappings in `src/Fable.Transforms/Replacements.fs` and a new `src/Fable.Transforms/Swift/Replacements.fs`.
- Broaden Swift AST and printer coverage (switch/loops, assignments, literals, types).
- Expand Fable2Swift transform for more Fable expressions, statements, and patterns.
- Add a minimal Swift runtime library and Fable.Core Swift interop surface.
- Build/test integration for Swift library and tests.
- Documentation, examples, and explicit supported/unsupported feature lists.
- Optional toolchain integration (SwiftPM / swiftc) and output layout decisions.

## Step-by-step plan (small increments)

All steps below should follow the repo workflow:

- Keep changes small (1-3 files, under ~50 lines) and run build + tests after each step.
- Fix warnings immediately and re-run build/tests.
- Use commit format `type(scope): summary` with body bullets and exact test commands.
- Recommended baseline commands (from CI): `dotnet build -c Release Fable.sln` and `./build.sh test integration` (or `.\build.bat test integration` on Windows). Add target-specific tests as they appear.

### Step 1: Swift replacements scaffolding

- Intent: create the Swift replacements file and wire it into the existing replacements flow.
- Files: `src/Fable.Transforms/Swift/Replacements.fs`, `src/Fable.Transforms/Replacements.fs`, `src/Fable.Transforms/Fable.Transforms.fsproj`.
- Tests: build + integration tests.
- Exit: Swift replacements module compiles and is invoked, even if it only handles a small subset.

### Step 2: Minimal replacements coverage (strings, console, options)

- Intent: map a handful of core BCL calls used in tests and common samples.
- Files: `src/Fable.Transforms/Swift/Replacements.fs`, `src/Fable.Transforms/Replacements.fs`.
- Tests: build + integration tests.
- Exit: string helpers, console output, and option helpers route through Swift intrinsics.

### Step 3: Swift AST expansion for statements and assignments

- Intent: add AST nodes for assignment, variable declaration, while/for loops, switch, break/continue.
- Files: `src/Fable.Transforms/Swift/Swift.fs`, `src/Fable.Transforms/Swift/SwiftPrinter.fs`, `tests/Integration/Integration/CliTests.fs`.
- Tests: build + integration tests.
- Exit: printer and tests cover new statement nodes.

### Step 4: Transform support for assignments and loops

- Intent: lower Fable Set/while/for to Swift statements.
- Files: `src/Fable.Transforms/Swift/Fable2Swift.fs`, `tests/Integration/Integration/CliTests.fs`.
- Tests: build + integration tests.
- Exit: basic mutable bindings and loop constructs compile to Swift.

### Step 5: Collection literals and tuple/record/union construction

- Intent: support Swift array/dictionary literals and core data constructors.
- Files: `src/Fable.Transforms/Swift/Swift.fs`, `src/Fable.Transforms/Swift/SwiftPrinter.fs`, `src/Fable.Transforms/Swift/Fable2Swift.fs`.
- Tests: build + integration tests.
- Exit: tuples, records, unions, arrays, and lists have concrete Swift output.

### Step 6: Pattern matching and decision tree lowering

- Intent: translate Fable decision trees into Swift switch or nested if where safe.
- Files: `src/Fable.Transforms/Swift/Fable2Swift.fs`, `tests/Integration/Integration/CliTests.fs`.
- Tests: build + integration tests.
- Exit: common match expressions compile with correct guard evaluation.

### Step 7: Type rendering and generic function signatures

- Intent: add Swift type syntax support and map Fable types to Swift types.
- Files: `src/Fable.Transforms/Swift/Swift.fs`, `src/Fable.Transforms/Swift/SwiftPrinter.fs`, `src/Fable.Transforms/Swift/Fable2Swift.fs`.
- Tests: build + integration tests.
- Exit: generated Swift includes consistent type annotations where required.

### Step 8: Swift runtime library skeleton

- Intent: introduce `fable-library-swift` with Option/Result/List/Array/Map/Set/String helpers.
- Files: `src/fable-library-swift/*`, `src/Fable.Build/FableLibrary/Swift.fs`.
- Tests: build + integration tests; add library build smoke test.
- Exit: library builds and is referenced by Swift output.

### Step 9: Fable.Core Swift interop

- Intent: add Swift-specific attributes and helpers.
- Files: `src/Fable.Core/Fable.Core.Swift.fs` (and optionally `Fable.Core.SwiftInterop.fs`).
- Tests: build + integration tests.
- Exit: Swift interop attributes are available for user code.

### Step 10: Build and test integration

- Intent: wire Swift into build/test scripts and add `tests/Swift`.
- Files: `src/Fable.Build/Quicktest/Swift.fs`, `src/Fable.Build/Test/Swift.fs`, `tests/Swift`.
- Tests: build + integration + new Swift test suite.
- Exit: Swift tests run in CI-style scripts.

### Step 11: CLI output layout and optional toolchain

- Intent: decide output structure and optional SwiftPM/swiftc flow.
- Files: `src/Fable.Cli/Pipeline.fs`, `src/Fable.Transforms/Transforms.Util.fs`, build scripts if needed.
- Tests: build + integration tests; optional Swift toolchain check.
- Exit: stable output layout with documented constraints.

### Step 12: Documentation and examples

- Intent: document supported subset, known gaps, and usage examples.
- Files: `docs/swift-backend-todo.md`, `docs/swift-backend-feasibility.md`, new Swift README/example project.
- Tests: build + integration tests (documentation-only changes can skip target-specific tests if justified).
- Exit: docs reflect current capability and usage.

## Open decisions to resolve early

- Swift version baseline and toolchain support policy.
- Runtime library data structure mappings (List/Map/Set semantics).
- Import path and module naming scheme for Swift output.
- Async/concurrency support approach (if any, post-v0).
