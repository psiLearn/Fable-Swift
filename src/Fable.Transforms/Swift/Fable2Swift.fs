module Fable.Transforms.Swift.Fable2Swift

open System
open System.IO
open Fable
open Fable.AST.Fable
open Fable.AST.Swift

let placeholderMessage =
    "Swift backend is not implemented yet. See docs/swift-backend-feasibility.md."

let placeholderFile sourceFile outPath : SwiftFile =
    let lines = [ placeholderMessage; $"Source: {sourceFile}"; $"Out: {outPath}" ]

    { Declarations = lines |> List.map SwiftComment }
