module Fable.Transforms.Swift.SwiftPrinter

open System
open System.Text
open Fable.AST.Swift
open Fable.Transforms.Printer

let isEmpty (file: SwiftFile) = List.isEmpty file.Declarations

let private safe (text: string) =
    if isNull text then
        ""
    else
        text

let private renderExpression =
    function
    | SwiftIdentifier name -> safe name
    | SwiftLiteral literal -> safe literal

let private renderStatement =
    function
    | SwiftExpr expr -> renderExpression expr

let private renderDeclaration =
    function
    | SwiftStatementDecl stmt -> renderStatement stmt

let private renderFile (file: SwiftFile) =
    let sb = StringBuilder()

    file.Declarations
    |> List.iter (fun decl ->
        let line = renderDeclaration decl

        if not (String.IsNullOrWhiteSpace line) then
            // TODO: extend with indentation, statements/blocks, and other declaration kinds as Swift AST grows.
            sb.AppendLine(line) |> ignore
    )

    sb.ToString()

let run (writer: Writer) (file: SwiftFile) =
    async {
        if not (isEmpty file) then
            let output = renderFile file
            do! writer.Write(output)
    }
