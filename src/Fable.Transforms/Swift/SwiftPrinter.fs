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

let private renderExpression =
    function
    | SwiftIdentifier name -> safe name
    | SwiftLiteral literal -> safe literal

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
