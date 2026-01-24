module rec Fable.AST.Swift

/// Minimal Swift AST scaffold to unblock backend wiring.
type SwiftBinaryOperator =
    | SwiftEqual
    | SwiftNotEqual
    | SwiftLess
    | SwiftLessOrEqual
    | SwiftGreater
    | SwiftGreaterOrEqual
    | SwiftLogicalOr
    | SwiftLogicalAnd
    | SwiftAssign

type SwiftExpression =
    | SwiftIdentifier of string
    | SwiftLiteral of string
    | SwiftStringLiteral of string
    | SwiftMemberAccess of SwiftExpression * string
    | SwiftSubscript of SwiftExpression * SwiftExpression
    | SwiftBinary of SwiftExpression * SwiftBinaryOperator * SwiftExpression
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
    | SwiftIf of SwiftExpression * SwiftBlock * SwiftBlock option

type SwiftDeclaration =
    | SwiftComment of string
    | SwiftImport of SwiftImportDecl
    | SwiftBinding of SwiftBindingDecl
    | SwiftStatementDecl of SwiftStatement
    | SwiftFuncDecl of SwiftFunctionDecl

type SwiftFile = { Declarations: SwiftDeclaration list }
