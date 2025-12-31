# Swift backend TODO

- [x] Extend Swift AST/printer with indentation, blocks, functions, and imports so generated files stay valid as the surface grows.
- [x] Map a minimal set of Fable AST constructs to Swift AST (identifiers, literals, simple bindings/functions) instead of the current placeholder.
- [x] Add unit tests for `SwiftPrinter` to lock down formatting of comments and future constructs.
- [x] Flow import path resolution and optional source mappings through the Swift writer once the printer supports them (maps emit; currently empty until locations are wired).
- [x] Keep integration coverage for the Swift path as we add non-placeholder emission.
