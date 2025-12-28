module rec Fable.AST.Swift

/// Minimal Swift AST scaffold to unblock backend wiring.
type SwiftExpression =
    | SwiftIdentifier of string
    | SwiftLiteral of string

type SwiftBlock = SwiftStatement list

type SwiftStatement =
    | SwiftExpr of SwiftExpression
    | SwiftBlock of SwiftBlock

type SwiftDeclaration =
    | SwiftComment of string
    | SwiftStatementDecl of SwiftStatement

type SwiftFile = { Declarations: SwiftDeclaration list }
