# Swift Backend Feasibility Report (Fable)

## Scope and intent
This document assesses feasibility of adding a Swift backend to Fable and lays out a minimal, incremental plan.
It intentionally avoids a full implementation and targets a small, domain-programming subset first.

Note: File references in this report point to the upstream Fable repository (github.com/fable-compiler/Fable, main branch).

## Executive summary
Adding a Swift backend is feasible but non-trivial. The main work is:
- A new target-specific transform + printer (similar to Dart/Rust backends).
- A Swift runtime library to cover core Fable types and helpers.
- CLI/build/test wiring to make the backend usable and testable.

The fastest path is to mirror the Dart backend structure (custom AST + printer) rather than Rust's heavier AST project,
then grow capability iteratively behind a "minimal subset" contract.

## Architecture notes that impact Swift
- Language selection lives in `src/Fable.AST/Plugins.fs` (the `Language` union).
- Compiler options and defaults (including file extensions) live in `src/Fable.Transforms/Global/Compiler.fs`.
- CLI dispatch by language happens in `src/Fable.Cli/Pipeline.fs`, with output paths in `src/Fable.Cli/Main.fs`.
- The F# AST -> Fable AST pipeline is in `src/Fable.Transforms/FSharp2Fable.fs` and `src/Fable.Transforms/FableTransforms.fs`.
- Replacements and core library mappings are centralized in `src/Fable.Transforms/Replacements.fs` and per-target files
  like `src/Fable.Transforms/Dart/Replacements.fs` and `src/Fable.Transforms/Rust/Replacements.fs`.

## Dart vs Rust backends (comparison)
Dart:
- Transform: `src/Fable.Transforms/Dart/Fable2Dart.fs`
- AST + printer: `src/Fable.Transforms/Dart/Dart.fs`, `src/Fable.Transforms/Dart/DartPrinter.fs`
- Runtime library: `src/fable-library-dart`
- Tests: `tests/Dart`

Rust:
- Transform: `src/Fable.Transforms/Rust/Fable2Rust.fs`
- AST + printer: `src/Fable.Transforms/Rust/AST/*`, `src/Fable.Transforms/Rust/RustPrinter.fs`
- Runtime library: `src/fable-library-rust`
- Tests: `tests/Rust`

Implication for Swift:
- Dart shows a lighter-weight path (custom AST + printer in one module).
- Rust shows a heavier, type-rich AST with more machinery. Swift probably does not need this initially.
- For a minimal subset, a Dart-like architecture is the pragmatic choice.

## Minimal subset (domain-programming focus)
Target a small core that enables typical domain logic without advanced compiler features:
- Types: primitives, tuples, records, discriminated unions, options, results, lists, arrays.
- Functions: lambdas, let bindings, modules, basic generic functions.
- Control flow: if/else, match (basic), while/for loops.
- Interop: very small surface (static imports, basic native calls).
- Exclusions for v0: computation expressions, SRTP, reflection, units of measure, dynamic operator overloading,
  complex object expressions, and most .NET BCL APIs.

This constraint keeps the Swift runtime small and limits transform complexity.

## Feasibility and rough effort
Feasible with staged delivery. The highest uncertainty is the runtime library and mapping of F# core semantics to Swift.
Rough sizing for a minimal backend (excluding exhaustive BCL coverage):
- Transform + printer skeleton: 2-4 weeks.
- Minimal runtime library + interop surface: 2-6 weeks.
- CLI/build/test plumbing + basic test suite: 1-2 weeks.
- Stabilization and gaps: 2-4 weeks.

Total: 7-16 weeks depending on scope and available Swift expertise.

## Proposed Swift backend design (incremental)
1) Swift AST + printer (Dart-style):
   - Keep a simple AST that maps 1:1 to Swift syntax.
   - Printer should be deterministic, stable, and simple to diff.
2) Fable2Swift transform:
   - Map Fable AST to Swift AST with a limited set of expression/statement forms.
   - Start with a "lowering" pass that normalizes Fable AST to a Swift-friendly subset.
3) Runtime library:
   - Implement a small `fable-library-swift` with Option, Result, List, Map, Set, String helpers, and a basic runtime.
   - Provide interop hooks similar to `src/Fable.Core/Fable.Core.Dart.fs` and `src/Fable.Core/Fable.Core.Rust.fs`.
4) Tooling (optional):
   - Add CLI language switch and output path rules.
   - Keep toolchain integration optional (emit-only by default; opt-in swiftc/SwiftPM).

## File-level pointers (upstream Fable repo)
Core language and compiler wiring:
- `src/Fable.AST/Plugins.fs` (Language union: add `Swift` here)
- `src/Fable.Transforms/Global/Compiler.fs` (CompilerOptions defaults and language/file extension)
- `src/Fable.Cli/Pipeline.fs` (language dispatch to backend)
- `src/Fable.Cli/Main.fs` (output paths and language-specific behavior)

Transforms and replacements:
- `src/Fable.Transforms/FSharp2Fable.fs` (F# AST -> Fable AST)
- `src/Fable.Transforms/Replacements.fs` and `src/Fable.Transforms/Replacements.Util.fs`
- `src/Fable.Transforms/Dart/Fable2Dart.fs` and `src/Fable.Transforms/Dart/DartPrinter.fs`
- `src/Fable.Transforms/Rust/Fable2Rust.fs` and `src/Fable.Transforms/Rust/RustPrinter.fs`

Runtime libraries and target-specific core APIs:
- `src/Fable.Core/Fable.Core.Dart.fs`
- `src/Fable.Core/Fable.Core.Rust.fs`
- `src/Fable.Core/Fable.Core.RustInterop.fs`
- `src/fable-library-dart`
- `src/fable-library-rust`

Build/test integration:
- `src/Fable.Build/FableLibrary/Dart.fs`
- `src/Fable.Build/FableLibrary/Rust.fs`
- `src/Fable.Build/Quicktest/Dart.fs`
- `src/Fable.Build/Quicktest/Rust.fs`
- `src/Fable.Build/Test/Dart.fs`
- `src/Fable.Build/Test/Rust.fs`
- `tests/Dart`
- `tests/Rust`

## Step-by-step implementation plan (concrete file layout)
1) Language plumbing (no backend logic yet)
   - `src/Fable.AST/Plugins.fs` (add `Swift` to `Language`).
   - `src/Fable.Transforms/Global/Compiler.fs` (default extension and language defaults).
   - `src/Fable.Cli/Pipeline.fs` (dispatch to Swift backend).
   - `src/Fable.Cli/Main.fs` (output path rules; no toolchain requirement).

2) Swift AST and printer scaffolding
   - `src/Fable.Transforms/Swift/Swift.fs` (Swift AST types).
   - `src/Fable.Transforms/Swift/SwiftPrinter.fs` (minimal printer for expressions/statements).
   - `src/Fable.Transforms/Fable.Transforms.fsproj` (include new Swift files).

3) Fable2Swift transform (minimal subset)
   - `src/Fable.Transforms/Swift/Fable2Swift.fs` (Fable AST -> Swift AST).
   - `src/Fable.Transforms/Swift/Replacements.fs` (target intrinsics and core mappings).
   - `src/Fable.Transforms/Replacements.fs` (wire Swift replacements where needed).

4) Swift interop surface
   - `src/Fable.Core/Fable.Core.Swift.fs` (Swift-specific attributes and helpers).

5) Runtime library layout
   - `src/fable-library-swift/Fable.Library.Swift.fsproj`
   - `src/fable-library-swift/Global.fs`
   - `src/fable-library-swift/FSharp.Core.fs`
   - `src/fable-library-swift/Option.fs`, `Result.fs`, `List.fs`, `Array.fs`, `Map.fs`, `Set.fs`
   - Mirror structure from `src/fable-library-dart` where possible.

6) Build, tests, and quicktest (toolchain optional)
   - `src/Fable.Build/FableLibrary/Swift.fs`
   - `src/Fable.Build/Quicktest/Swift.fs`
   - `src/Fable.Build/Test/Swift.fs`
   - `tests/Swift` (mirror `tests/Dart` and `tests/Rust`)
   - Swift toolchain integration is optional; default to emit-only compilation.

7) Documentation and constraints
   - `docs/swift-backend-feasibility.md` (keep current and update as scope grows).
   - Add a minimal README with supported subset and examples.

## TODO
- Keep the Swift toolchain optional (emit-only by default; opt-in swiftc/SwiftPM).
- Confirm desired Swift version if toolchain integration is enabled.
- Decide output layout (single file vs SwiftPM package structure).
- Define the initial subset and explicit unsupported features.
- Decide how F# async maps to Swift concurrency (v0 may skip async entirely).
- Determine interop story (FFI, native calls, module imports).
