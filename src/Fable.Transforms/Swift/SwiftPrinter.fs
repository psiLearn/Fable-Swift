module Fable.Transforms.Swift.SwiftPrinter

open Fable.AST.Swift
open System

let isEmpty (file: SwiftFile) = List.isEmpty file.Declarations

let run (_file: SwiftFile) =
    // Swift code emission is not implemented yet; fail explicitly to avoid silent no-op.
    async {
        return
            raise (
                NotImplementedException("Swift printer is not implemented yet. See docs/swift-backend-feasibility.md.")
            )
    }
