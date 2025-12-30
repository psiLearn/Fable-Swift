module Fable.Transforms.Swift.Fable2Swift

open System
open System.Globalization
open System.Numerics
open Fable
open Fable.AST.Fable
open Fable.AST.Swift
open Fable.Transforms

let placeholderMessage =
    "Swift backend is not implemented yet. See docs/swift-backend-feasibility.md."

let private warningPrefix = "Swift backend"
let private unsupportedExprPlaceholder = "/* unsupported expr */"
let private unsupportedBindingPlaceholder = "/* unsupported binding value */"

let private nilLiteral = SwiftLiteral "nil"
let private unsupportedExprCommentLiteral = SwiftLiteral unsupportedExprPlaceholder

let private unsupportedBindingCommentLiteral =
    SwiftLiteral unsupportedBindingPlaceholder

let private unsupportedExprFallback =
    SwiftLiteral $"fatalError(\"{warningPrefix}: unsupported expression\")"

let private unsupportedBindingFallback =
    SwiftLiteral $"fatalError(\"{warningPrefix}: unsupported binding value\")"

let placeholderFile sourceFile outPath : SwiftFile =
    let lines = [ placeholderMessage; $"Source: {sourceFile}"; $"Out: {outPath}" ]

    { Declarations = lines |> List.map SwiftComment }

let private isUnitType =
    function
    | Unit -> true
    | _ -> false

let private formatNumber (value: NumberValue) =
    let invariant = CultureInfo.InvariantCulture

    let bigIntFromParts (upper: uint64) (lower: uint64) =
        (BigInteger upper <<< 64) + BigInteger lower

    let int128FromParts (upper: uint64) (lower: uint64) =
        let unsigned = bigIntFromParts upper lower
        let signMask = 0x8000000000000000UL

        if (upper &&& signMask) <> 0UL then
            unsigned - (BigInteger.One <<< 128)
        else
            unsigned

    match value with
    | NumberValue.Int8 v -> v.ToString(invariant)
    | NumberValue.UInt8 v -> v.ToString(invariant)
    | NumberValue.Int16 v -> v.ToString(invariant)
    | NumberValue.UInt16 v -> v.ToString(invariant)
    | NumberValue.Int32 v -> v.ToString(invariant)
    | NumberValue.UInt32 v -> v.ToString(invariant)
    | NumberValue.Int64 v -> v.ToString(invariant)
    | NumberValue.UInt64 v -> v.ToString(invariant)
    | NumberValue.Int128(upper, lower) -> int128FromParts upper lower |> fun v -> v.ToString(invariant)
    | NumberValue.UInt128(upper, lower) -> bigIntFromParts upper lower |> fun v -> v.ToString(invariant)
    | NumberValue.BigInt v -> v.ToString(invariant)
    | NumberValue.NativeInt v -> string v
    | NumberValue.UNativeInt v -> string v
    | NumberValue.Float16 v -> v.ToString("G9", invariant)
    | NumberValue.Float32 v -> v.ToString("G9", invariant)
    | NumberValue.Float64 v -> v.ToString("G17", invariant)
    | NumberValue.Decimal v -> v.ToString(invariant)

let private tryTransformValue =
    function
    | BoolConstant value ->
        SwiftLiteral(
            if value then
                "true"
            else
                "false"
        )
        |> Some
    | StringConstant value -> SwiftStringLiteral value |> Some
    | CharConstant value -> SwiftStringLiteral(string value) |> Some
    | NumberConstant(value, _) -> SwiftLiteral(formatNumber value) |> Some
    | Null _ -> nilLiteral |> Some
    | UnitConstant -> SwiftLiteral("()") |> Some
    | _ -> None

let rec private tryTransformExpr =
    function
    | IdentExpr ident -> SwiftIdentifier ident.Name |> Some
    | Value(kind, _) -> tryTransformValue kind
    | TypeCast(expr, _) -> tryTransformExpr expr
    | Get(expr, kind, _, _) ->
        match tryTransformExpr expr with
        | None -> None
        | Some target ->
            match kind with
            | FieldGet info -> SwiftMemberAccess(target, info.Name) |> Some
            | TupleIndex index -> SwiftMemberAccess(target, string index) |> Some
            | _ -> None
    | Call(callee, info, _, _) ->
        let calleeExpr =
            match info.ThisArg, callee with
            | Some thisArg, IdentExpr ident ->
                match tryTransformExpr thisArg with
                | Some target -> SwiftMemberAccess(target, ident.Name) |> Some
                | None -> None
            | _ -> tryTransformExpr callee

        match calleeExpr, tryTransformArgs info.Args with
        | Some target, Some args -> SwiftCall(target, args) |> Some
        | _ -> None
    | CurriedApply(applied, args, _, _) ->
        match tryTransformExpr applied, tryTransformArgs args with
        | Some target, Some args -> SwiftCall(target, args) |> Some
        | _ -> None
    | _ -> None

and private tryTransformArgs args =
    let transformed = args |> List.map tryTransformExpr

    if transformed |> List.exists Option.isNone then
        None
    else
        transformed |> List.choose id |> Some

let private addSwiftWarning (com: Compiler) range message =
    addWarning com [] range $"{warningPrefix}: {message}"

let private hasUnsupportedArgs expr =
    match expr with
    | Call(_, info, _, _) -> tryTransformArgs info.Args |> Option.isNone
    | CurriedApply(_, args, _, _) -> tryTransformArgs args |> Option.isNone
    | _ -> false

let private warnUnsupportedExpr (com: Compiler) context expr =
    let message =
        if hasUnsupportedArgs expr then
            $"unsupported call arguments in {context}."
        else
            $"unsupported expression in {context}."

    addSwiftWarning com expr.Range message

let private unsupportedExprStatement = SwiftExpr unsupportedExprCommentLiteral

let private transformBinding (com: Compiler) (ident: Ident) value =
    match tryTransformExpr value with
    | Some expr ->
        let binding =
            {
                Name = ident.Name
                Expr = Some expr
                IsMutable = ident.IsMutable
            }

        [ SwiftBindingStatement binding ]
    | None ->
        warnUnsupportedExpr com $"binding '{ident.Name}'" value

        let binding =
            {
                Name = ident.Name
                Expr = Some unsupportedBindingFallback
                IsMutable = ident.IsMutable
            }

        [ SwiftExpr unsupportedBindingCommentLiteral; SwiftBindingStatement binding ]

let rec private transformExprAsStatements (com: Compiler) expr isTail =
    match expr with
    | Let(ident, value, body) -> transformBinding com ident value @ transformExprAsStatements com body isTail
    | LetRec(bindings, body) ->
        let bindingStatements =
            bindings
            |> List.collect (fun (ident, value) -> transformBinding com ident value)

        bindingStatements @ transformExprAsStatements com body isTail
    | Sequential exprs ->
        let lastIndex = List.length exprs - 1

        exprs
        |> List.mapi (fun index expr ->
            let isTail = isTail && index = lastIndex
            transformExprAsStatements com expr isTail
        )
        |> List.collect id
    | _ ->
        match tryTransformExpr expr with
        | Some swiftExpr ->
            if isTail && not (isUnitType expr.Type) then
                [ SwiftReturn(Some swiftExpr) ]
            else
                [ SwiftExpr swiftExpr ]
        | None ->
            warnUnsupportedExpr com "statement expression" expr

            if isTail && not (isUnitType expr.Type) then
                [ unsupportedExprStatement; SwiftReturn(Some unsupportedExprFallback) ]
            else
                [ unsupportedExprStatement ]

let private transformMemberDecl (com: Compiler) (decl: MemberDecl) =
    let parameters =
        decl.Args
        |> List.filter (fun arg -> not arg.IsThisArgument)
        |> List.map (fun arg -> arg.Name)

    if List.isEmpty parameters then
        match tryTransformExpr decl.Body with
        | Some expr ->
            [
                SwiftBinding
                    {
                        Name = decl.Name
                        Expr = Some expr
                        IsMutable = false
                    }
            ]
        | None ->
            warnUnsupportedExpr com $"member '{decl.Name}'" decl.Body

            [
                SwiftComment $"member {decl.Name} has unsupported value"
                SwiftBinding
                    {
                        Name = decl.Name
                        Expr = Some nilLiteral
                        IsMutable = false
                    }
            ]
    else
        let body = transformExprAsStatements com decl.Body true

        [
            SwiftFuncDecl
                {
                    Name = decl.Name
                    Parameters = parameters
                    Body = body
                }
        ]

let rec private transformDeclaration (com: Compiler) =
    function
    | ModuleDeclaration decl ->
        let header = SwiftComment $"module {decl.Name}"
        header :: (decl.Members |> List.collect (transformDeclaration com))
    | ActionDeclaration decl -> transformExprAsStatements com decl.Body false |> List.map SwiftStatementDecl
    | MemberDeclaration decl -> transformMemberDecl com decl
    | ClassDeclaration decl ->
        let range =
            match decl.Constructor with
            | Some ctor -> ctor.Body.Range
            | None -> decl.AttachedMembers |> List.tryPick (fun memberDecl -> memberDecl.Body.Range)

        addSwiftWarning com range $"class '{decl.Name}' declarations are not supported yet."
        [ SwiftComment $"class {decl.Name} is not supported yet" ]

module Compiler =

    let transformFile (com: Compiler) (file: File) : SwiftFile =
        let declarations = file.Declarations |> List.collect (transformDeclaration com)
        { Declarations = declarations }
