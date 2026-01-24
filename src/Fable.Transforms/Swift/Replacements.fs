module Fable.Transforms.Swift.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

let tryField (com: ICompiler) returnTyp ownerTyp fieldName =
    Fable.Transforms.JS.Replacements.tryField com returnTyp ownerTyp fieldName

let tryBaseConstructor (com: ICompiler) ctx (ent: EntityRef) (argTypes: Lazy<Type list>) genArgs args =
    Fable.Transforms.JS.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args

let makeMethodInfo (com: ICompiler) r (name: string) (parameters: (string * Type) list) (returnType: Type) =
    Fable.Transforms.JS.Replacements.makeMethodInfo com r name parameters returnType

let tryType typ =
    Fable.Transforms.JS.Replacements.tryType typ

let private implementedStringFunctions =
    set [| "IsNullOrEmpty"; "IsNullOrWhiteSpace" |]

let private optionValueCall (com: ICompiler) (r: SourceLocation option) (t: Type) (arg: Expr) =
    Helper.LibCall(com, "Option", "value", t, [ arg ], [ arg.Type ], ?loc = r)

let private options
    isStruct
    (com: ICompiler)
    (r: SourceLocation option)
    (t: Type)
    (info: ReplaceCallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "Some", _ -> NewOption(List.tryHead args, t.Generics.Head, isStruct) |> makeValue r |> Some
    | "get_None", _ -> NewOption(None, t.Generics.Head, isStruct) |> makeValue r |> Some
    | "get_Value", Some c -> optionValueCall com r t c |> Some
    | "get_IsSome", Some c -> Test(c, OptionTest true, r) |> Some
    | "get_IsNone", Some c -> Test(c, OptionTest false, r) |> Some
    | _ -> None

let private optionModule
    isStruct
    (com: ICompiler)
    (r: SourceLocation option)
    (t: Type)
    (info: ReplaceCallInfo)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "None", _ -> NewOption(None, t, isStruct) |> makeValue r |> Some
    | "GetValue", [ c ] -> optionValueCall com r t c |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | ("OfObj" | "OfNullable"), _ ->
        Helper.LibCall(com, "Option", "ofNullable", t, args, genArgs = info.GenericArgs, ?loc = r)
        |> Some
    | ("ToObj" | "ToNullable"), _ ->
        Helper.LibCall(com, "Option", "toNullable", t, args, genArgs = info.GenericArgs, ?loc = r)
        |> Some
    | "DefaultValue", _ -> Helper.LibCall(com, "Option", "defaultArg", t, List.rev args, ?loc = r) |> Some
    | "DefaultWith", _ ->
        Helper.LibCall(
            com,
            "Option",
            "defaultArgWith",
            t,
            List.rev args,
            List.rev info.SignatureArgTypes,
            genArgs = info.GenericArgs,
            ?loc = r
        )
        |> Some
    | "OrElse", _ -> Helper.LibCall(com, "Option", "orElse", t, List.rev args, ?loc = r) |> Some
    | "OrElseWith", _ ->
        Helper.LibCall(
            com,
            "Option",
            "orElseWith",
            t,
            List.rev args,
            List.rev info.SignatureArgTypes,
            genArgs = info.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("OfOption" | "ToOption" | "OfValueOption" | "ToValueOption"), [ arg ] -> arg |> Some
    | meth, _ ->
        Helper.LibCall(
            com,
            "Option",
            Naming.lowerFirst meth,
            t,
            args,
            info.SignatureArgTypes,
            genArgs = info.GenericArgs,
            ?loc = r
        )
        |> Some

let private log (com: ICompiler) (r: SourceLocation option) (t: Type) (info: ReplaceCallInfo) (args: Expr list) =
    Helper.LibCall(com, "String", "toConsole", t, args, info.SignatureArgTypes, genArgs = info.GenericArgs, ?loc = r)

let private tryOptionCall
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (info: ReplaceCallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.DeclaringEntityFullName with
    | Types.option -> options false com r t info thisArg args
    | Types.valueOption -> options true com r t info thisArg args
    | "Microsoft.FSharp.Core.OptionModule" -> optionModule false com r t info args
    | "Microsoft.FSharp.Core.ValueOption" -> optionModule true com r t info args
    | _ -> None

let private tryConsoleCall
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (info: ReplaceCallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | "get_Out" -> typedObjExpr t [] |> Some
    | "Write" ->
        addWarning com ctx.InlinePath r "Write will behave as WriteLine"
        log com r t info args |> Some
    | "WriteLine" -> log com r t info args |> Some
    | _ -> None

let private trySystemStringCall (com: ICompiler) ctx r t (info: ReplaceCallInfo) thisArg args =
    match info.CompiledName, thisArg with
    | Patterns.SetContains implementedStringFunctions, None ->
        Helper.LibCall(
            com,
            "String",
            Naming.lowerFirst info.CompiledName,
            t,
            args,
            info.SignatureArgTypes,
            genArgs = info.GenericArgs,
            ?loc = r
        )
        |> Some
    | _ -> None

let tryCall (com: ICompiler) ctx r t info thisArg args =
    match info.DeclaringEntityFullName with
    | "System.String" ->
        match trySystemStringCall com ctx r t info thisArg args with
        | Some expr -> Some expr
        | None -> Fable.Transforms.JS.Replacements.tryCall com ctx r t info thisArg args
    | "System.Console"
    | Types.option
    | Types.valueOption
    | "Microsoft.FSharp.Core.OptionModule"
    | "Microsoft.FSharp.Core.ValueOption" ->
        match tryOptionCall com ctx r t info thisArg args with
        | Some expr -> Some expr
        | None ->
            match tryConsoleCall com ctx r t info thisArg args with
            | Some expr -> Some expr
            | None -> Fable.Transforms.JS.Replacements.tryCall com ctx r t info thisArg args
    | _ -> Fable.Transforms.JS.Replacements.tryCall com ctx r t info thisArg args

let error (com: ICompiler) msg =
    Fable.Transforms.JS.Replacements.error com msg

let defaultof (com: ICompiler) ctx r typ =
    Fable.Transforms.JS.Replacements.defaultof com ctx r typ

let getRefCell (com: ICompiler) r typ (expr: Expr) =
    Fable.Transforms.JS.Replacements.getRefCell com r typ expr

let setRefCell (com: ICompiler) r (expr: Expr) (value: Expr) =
    Fable.Transforms.JS.Replacements.setRefCell com r expr value

let makeRefCellFromValue (com: ICompiler) r (value: Expr) =
    Fable.Transforms.JS.Replacements.makeRefCellFromValue com r value

let makeRefFromMutableFunc (com: ICompiler) ctx r t (value: Expr) =
    Fable.Transforms.JS.Replacements.makeRefFromMutableFunc com ctx r t value

let makeRefFromMutableValue (com: ICompiler) ctx r t (value: Expr) =
    Fable.Transforms.JS.Replacements.makeRefFromMutableValue com ctx r t value

let makeRefFromMutableField (com: ICompiler) ctx r t (value: Expr) =
    Fable.Transforms.JS.Replacements.makeRefFromMutableField com ctx r t value
