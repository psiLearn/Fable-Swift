module rec Fable.AST.Swift

/// Minimal Swift AST scaffold to unblock backend wiring.
type SwiftExpression =
    | SwiftIdentifier of string
    | SwiftLiteral of string
    | SwiftStringLiteral of string
    | SwiftMemberAccess of SwiftExpression * string
    | SwiftCall of SwiftExpression * SwiftExpression list

type SwiftBlock = SwiftStatement list

type SwiftFunctionDecl =
    {
        Name: string
        Parameters: string list
        Body: SwiftBlock
    }

type SwiftImportDecl = { Module: string }

type SwiftBindingDecl =
    {
        Name: string
        Expr: SwiftExpression option
        IsMutable: bool
    }

type SwiftStatement =
    | SwiftExpr of SwiftExpression
    | SwiftBlock of SwiftBlock
    | SwiftReturn of SwiftExpression option
    | SwiftBindingStatement of SwiftBindingDecl

type SwiftDeclaration =
    | SwiftComment of string
    | SwiftImport of SwiftImportDecl
    | SwiftBinding of SwiftBindingDecl
    | SwiftStatementDecl of SwiftStatement
    | SwiftFuncDecl of SwiftFunctionDecl

type SwiftFile = { Declarations: SwiftDeclaration list }
