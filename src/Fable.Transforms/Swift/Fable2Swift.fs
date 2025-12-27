module Fable.Transforms.Swift.Fable2Swift

open System
open System.IO
open Fable
open Fable.AST.Fable

let placeholderMessage =
    "Swift backend is not implemented yet. See docs/swift-backend-feasibility.md."

let placeholderContent sourceFile outPath =
    String.concat Environment.NewLine [ $"// {placeholderMessage}"; $"// Source: {sourceFile}"; $"// Out: {outPath}" ]
