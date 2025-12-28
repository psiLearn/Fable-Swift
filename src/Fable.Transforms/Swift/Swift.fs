module rec Fable.AST.Swift

/// Minimal Swift AST scaffold to unblock backend wiring.
type SwiftExpression =
    | SwiftIdentifier of string
    | SwiftLiteral of string

type SwiftBlock = SwiftStatement list

type SwiftFunctionDecl =
    {
        Name: string
        Body: SwiftBlock
    }

type SwiftStatement =
    | SwiftExpr of SwiftExpression
    | SwiftBlock of SwiftBlock

type SwiftDeclaration =
    | SwiftComment of string
    | SwiftStatementDecl of SwiftStatement
    | SwiftFuncDecl of SwiftFunctionDecl

type SwiftFile = { Declarations: SwiftDeclaration list }
