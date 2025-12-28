# Swift backend TODO

- Extend Swift AST/printer with indentation, blocks, functions, and imports so generated files stay valid as the surface grows.
- Map a minimal set of Fable AST constructs to Swift AST (identifiers, literals, simple bindings/functions) instead of the current placeholder.
- Add unit tests for `SwiftPrinter` to lock down formatting of comments and future constructs.
- Flow import path resolution and optional source mappings through the Swift writer once the printer supports them.
- Keep integration coverage for the Swift path as we add non-placeholder emission.
