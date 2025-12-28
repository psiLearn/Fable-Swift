module Fable.Transforms.Swift.SwiftPrinter

open System
open System.Text
open Fable.AST.Swift
open Fable.Transforms.Printer

let isEmpty (file: SwiftFile) = List.isEmpty file.Declarations

let private indentStep = "    "

let private safe (text: string) =
    if isNull text then
        ""
    else
        text

let rec private renderExpression =
    function
    | SwiftIdentifier name -> safe name
    | SwiftLiteral literal -> safe literal
    | SwiftMemberAccess(expr, memberName) ->
        let target = renderExpression expr
        let memberText = safe memberName

        if String.IsNullOrWhiteSpace(target) || String.IsNullOrWhiteSpace(memberText) then
            ""
        else
            $"{target}.{memberText}"
    | SwiftCall(callee, args) ->
        let renderedCallee = renderExpression callee

        if String.IsNullOrWhiteSpace(renderedCallee) then
            ""
        else
            let renderedArgs =
                args
                |> List.map renderExpression
                |> List.filter (String.IsNullOrWhiteSpace >> not)
                |> String.concat ", "

            $"{renderedCallee}({renderedArgs})"

let rec private renderStatement (indent: string) =
    function
    | SwiftExpr expr ->
        let body = renderExpression expr

        if String.IsNullOrWhiteSpace(body) then
            ""
        else
            indent + body
    | SwiftBlock statements ->
        let innerIndent = indent + indentStep
        let nl = Environment.NewLine

        let inner =
            statements
            |> List.map (renderStatement innerIndent)
            |> List.filter (fun line -> not (String.IsNullOrWhiteSpace line))
            |> String.concat nl

        let openBrace = indent + "{"
        let closeBrace = indent + "}"

        if String.IsNullOrWhiteSpace(inner) then
            String.concat nl [ openBrace; closeBrace ]
        else
            String.concat nl [ openBrace; inner; closeBrace ]

let private renderDeclaration indent =
    function
    | SwiftComment text ->
        let body = safe text

        if String.IsNullOrWhiteSpace(body) then
            ""
        else
            $"{indent}// {body}"
    | SwiftImport importDecl ->
        let moduleName = safe importDecl.Module

        if String.IsNullOrWhiteSpace(moduleName) then
            ""
        else
            indent + $"import {moduleName}"
    | SwiftBinding bindingDecl ->
        let name = safe bindingDecl.Name

        if String.IsNullOrWhiteSpace(name) then
            ""
        else
            let keyword =
                if bindingDecl.IsMutable then
                    "var"
                else
                    "let"

            match bindingDecl.Expr with
            | None -> $"{indent}{keyword} {name}"
            | Some expr ->
                let rhs = renderExpression expr

                if String.IsNullOrWhiteSpace(rhs) then
                    $"{indent}{keyword} {name}"
                else
                    $"{indent}{keyword} {name} = {rhs}"
    | SwiftFuncDecl funcDecl ->
        let name = safe funcDecl.Name

        if String.IsNullOrWhiteSpace(name) then
            ""
        else
            let header = indent + $"func {name}()"
            let block = renderStatement indent (SwiftBlock funcDecl.Body)

            if String.IsNullOrWhiteSpace(block) then
                header
            else
                String.concat Environment.NewLine [ header; block ]
    | SwiftStatementDecl stmt -> renderStatement indent stmt

let private renderFile (file: SwiftFile) =
    let sb = StringBuilder()

    file.Declarations
    |> List.iter (fun decl ->
        let line = renderDeclaration "" decl

        if not (String.IsNullOrWhiteSpace line) then
            // TODO: extend with additional declaration kinds as Swift AST grows.
            sb.AppendLine(line) |> ignore
    )

    sb.ToString()

let run (writer: Writer) (file: SwiftFile) =
    async {
        if not (isEmpty file) then
            let output = renderFile file
            do! writer.Write(output)
    }
