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

type private StderrAnalysis =
    {
        NeedsStderrPrint: bool
        HasStderrHelper: bool
        HasFoundationImport: bool
        LastImportIndex: int option
    }

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

let private isStringLibraryMember memberName (info: ImportInfo) =
    match info.Kind with
    | LibraryImport _ ->
        info.Selector = memberName
        && info.Path.EndsWith("/String.swift", StringComparison.Ordinal)
    | _ -> false

let private isSimplePrintfFormat (formatText: string) =
    match formatText with
    | "%s"
    | "%i"
    | "%d"
    | "%f"
    | "%O"
    | "%A" -> true
    | _ -> false

let private stdoutPrintIdentifier = SwiftIdentifier "print"
let private stderrPrintName = "stderrPrint"
let private stderrPrintIdentifier = SwiftIdentifier stderrPrintName

let private tryGetPrintTarget name =
    match name with
    | "printf"
    | "printfn" -> Some stdoutPrintIdentifier
    | "eprintf"
    | "eprintfn" -> Some stderrPrintIdentifier
    | _ -> None

let rec private tryGetCalleeName =
    function
    | IdentExpr ident -> Some ident.Name
    | Import(info, _, _) -> Some info.Selector
    | Get(_, FieldGet info, _, _) -> Some info.Name
    | Get(_, ExprGet(Value(StringConstant name, _)), _, _) -> Some name
    | TypeCast(expr, _) -> tryGetCalleeName expr
    | _ -> None

let rec private stripTypeCast =
    function
    | TypeCast(expr, _) -> stripTypeCast expr
    | expr -> expr

let private tryGetStringConstant expr =
    match stripTypeCast expr with
    | Value(StringConstant value, _) -> Some value
    | _ -> None

let private tryGetPrintfFormat expr =
    match stripTypeCast expr with
    | Call(callee, callInfo, _, _) ->
        match tryGetCalleeName callee, callInfo.Args with
        | Some "printf", [ arg ] -> tryGetStringConstant arg
        | _ -> None
    | CurriedApply(applied, args, _, _) ->
        match tryGetCalleeName applied, args with
        | Some "printf", [ arg ] -> tryGetStringConstant arg
        | _ -> None
    | _ -> None

let private tryGetConsoleTarget (info: ImportInfo) =
    if isStringLibraryMember "toConsole" info then
        Some stdoutPrintIdentifier
    elif isStringLibraryMember "toConsoleError" info then
        Some stderrPrintIdentifier
    else
        None

let rec private tryTransformExpr =
    function
    | Import(info, _, _) -> tryGetConsoleTarget info
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
    | Call(Import(info, _, _), callInfo, _, _) ->
        match tryGetConsoleTarget info with
        | Some target -> tryTransformConsoleCall target callInfo.Args
        | None -> None
    | CurriedApply(Call(Import(info, _, _), callInfo, _, _), args, _, _) ->
        match tryGetConsoleTarget info with
        | Some target -> tryTransformConsoleCall target (callInfo.Args @ args)
        | None -> None
    | Call(callee, callInfo, _, _) ->
        match tryGetCalleeName callee |> Option.bind tryGetPrintTarget with
        | Some target -> tryTransformConsoleCall target callInfo.Args
        | _ ->
            let calleeExpr =
                match callInfo.ThisArg, callee with
                | Some thisArg, IdentExpr ident ->
                    match tryTransformExpr thisArg with
                    | Some target -> SwiftMemberAccess(target, ident.Name) |> Some
                    | None -> None
                | _ -> tryTransformExpr callee

            match calleeExpr, tryTransformArgs callInfo.Args with
            | Some target, Some args -> SwiftCall(target, args) |> Some
            | _ -> None
    | CurriedApply(Call(callee, callInfo, callType, callRange), args, _, _) ->
        match tryGetCalleeName callee |> Option.bind tryGetPrintTarget with
        | Some target -> tryTransformConsoleCall target (callInfo.Args @ args)
        | _ ->
            match tryTransformExpr (Call(callee, callInfo, callType, callRange)), tryTransformArgs args with
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

and private tryTransformPrintArg expr =
    match tryGetPrintfFormat expr with
    | Some formatText -> SwiftStringLiteral formatText |> Some
    | None -> tryTransformExpr expr

and private tryTransformPrintArgs args =
    let args =
        match args with
        | first :: rest when rest.Length = 1 ->
            match stripTypeCast first with
            | Value(StringConstant formatText, _) when isSimplePrintfFormat formatText -> rest
            | _ ->
                match tryGetPrintfFormat first with
                | Some formatText when isSimplePrintfFormat formatText -> rest
                | _ -> args
        | _ -> args

    let transformed = args |> List.map tryTransformPrintArg

    if transformed |> List.exists Option.isNone then
        None
    else
        transformed |> List.choose id |> Some

and private tryTransformConsoleCall target args =
    match tryTransformPrintArgs args with
    | Some swiftArgs -> SwiftCall(target, swiftArgs) |> Some
    | None -> None

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

        let rec exprUsesStderrPrint =
            function
            | SwiftCall(callee, args) ->
                match callee with
                | SwiftIdentifier name when name = stderrPrintName -> true
                | _ -> exprUsesStderrPrint callee || (args |> List.exists exprUsesStderrPrint)
            | SwiftMemberAccess(expr, _) -> exprUsesStderrPrint expr
            | _ -> false

        let rec statementUsesStderrPrint =
            function
            | SwiftExpr expr -> exprUsesStderrPrint expr
            | SwiftReturn(Some expr) -> exprUsesStderrPrint expr
            | SwiftReturn None -> false
            | SwiftBindingStatement binding -> binding.Expr |> Option.exists exprUsesStderrPrint
            | SwiftBlock statements -> statements |> List.exists statementUsesStderrPrint

        let declarationUsesStderrPrint =
            function
            | SwiftBinding binding -> binding.Expr |> Option.exists exprUsesStderrPrint
            | SwiftStatementDecl stmt -> statementUsesStderrPrint stmt
            | SwiftFuncDecl funcDecl -> funcDecl.Body |> List.exists statementUsesStderrPrint
            | _ -> false

        let analyzeDeclaration index analysis declaration =
            let needsStderrPrint =
                analysis.NeedsStderrPrint || declarationUsesStderrPrint declaration

            let hasStderrHelper =
                analysis.HasStderrHelper
                || match declaration with
                   | SwiftFuncDecl funcDecl when funcDecl.Name = stderrPrintName -> true
                   | _ -> false

            let hasFoundationImport =
                analysis.HasFoundationImport
                || match declaration with
                   | SwiftImport importDecl when importDecl.Module = "Foundation" -> true
                   | _ -> false

            let lastImportIndex =
                match declaration with
                | SwiftImport _ -> Some index
                | _ -> analysis.LastImportIndex

            {
                NeedsStderrPrint = needsStderrPrint
                HasStderrHelper = hasStderrHelper
                HasFoundationImport = hasFoundationImport
                LastImportIndex = lastImportIndex
            }

        let analysis =
            declarations
            |> List.mapi (fun index declaration -> index, declaration)
            |> List.fold
                (fun state (index, declaration) -> analyzeDeclaration index state declaration)
                {
                    NeedsStderrPrint = false
                    HasStderrHelper = false
                    HasFoundationImport = false
                    LastImportIndex = None
                }

        let declarations =
            if analysis.NeedsStderrPrint && not analysis.HasStderrHelper then
                let helperBody =
                    [
                        SwiftExpr(
                            SwiftLiteral
                                "FileHandle.standardError.write(Data((String(describing: value) + \"\\n\").utf8))"
                        )
                    ]

                let helperDecl =
                    SwiftFuncDecl
                        {
                            Name = stderrPrintName
                            Parameters = [ "_ value: Any" ]
                            Body = helperBody
                        }

                let helperImports =
                    if analysis.HasFoundationImport then
                        []
                    else
                        [ SwiftImport { Module = "Foundation" } ]

                match analysis.LastImportIndex with
                | Some lastIndex ->
                    let prefix, suffix = declarations |> List.splitAt (lastIndex + 1)
                    prefix @ helperImports @ (helperDecl :: suffix)
                | None -> helperImports @ (helperDecl :: declarations)
            else
                declarations

        { Declarations = declarations }
