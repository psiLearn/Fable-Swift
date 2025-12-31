module Fable.Tests.Cli

open System
open Fable
open Fable.Cli.Entry
open Fable.Cli.Pipeline
open Fable.Compiler.ProjectCracker
open Fable.Compiler.Util
open Fable.Transforms
open Fable.AST.Swift
open Fable.Transforms.Printer
open Fable.Transforms.Swift
open Fable.Transforms.Swift.SwiftPrinter
open Expecto

type Result<'T1, 'T2> with
    member this.Value = match this with Ok v -> v | Error _ -> failwith "I'm Error!"

type InMemoryWriter(write: string -> unit) =
    interface Printer.Writer with
        member _.Write(str) =
            write str
            async.Return()

        member _.MakeImportPath(path) = path
        member _.AddSourceMapping(_, _, _, _, _, _) = ()
        member _.AddLog(_msg, _severity, ?range) = ()

    interface IDisposable with
        member _.Dispose() = ()

let private makeCompilerWithFile language libraryDir currentFile sourceFiles projectFile =
    let options = CompilerOptionsHelper.Make(language = language)

    { new Compiler with
        member _.LibraryDir = libraryDir
        member _.CurrentFile = currentFile
        member _.OutputDir = None
        member _.OutputType = OutputType.Library
        member _.ProjectFile = projectFile
        member _.ProjectOptions = invalidOp "Not implemented in test stub"
        member _.SourceFiles = sourceFiles
        member _.Options = options
        member _.Plugins = { MemberDeclarationPlugins = Map.empty }
        member _.IncrementCounter() = 0
        member _.IsPrecompilingInlineFunction = false
        member this.WillPrecompileInlineFunction _ = this
        member _.GetImplementationFile _ = invalidOp "Not implemented in test stub"
        member _.GetRootModule _ = "", None
        member _.TryGetEntity _ = None
        member _.GetInlineExpr _ = invalidOp "Not implemented in test stub"
        member _.AddWatchDependency _ = ()
        member _.AddLog(_msg, _severity, ?range, ?fileName, ?tag) = ()
    }

let private makeCompiler language libraryDir =
    makeCompilerWithFile language libraryDir "" [||] ""

let private makeSwiftCliArgs projectFile rootDir outDir sourceMaps =
    let usesOutDir =
        outDir
        |> Option.map String.IsNullOrWhiteSpace
        |> Option.defaultValue true
        |> not

    {
        ProjectFile = projectFile
        RootDir = rootDir
        OutDir = outDir
        IsWatch = false
        Precompile = false
        PrecompiledLib = None
        PrintAst = false
        FableLibraryPath = None
        Configuration = "Debug"
        NoRestore = true
        NoCache = true
        NoParallelTypeCheck = false
        SourceMaps = sourceMaps
        SourceMapsRoot = None
        Exclude = []
        Replace = Map.empty
        RunProcess = None
        CompilerOptions =
            CompilerOptionsHelper.Make(
                language = Swift,
                fileExtension = File.defaultFileExt usesOutDir Swift
            )
        Verbosity = Verbosity.Normal
    }

let private defaultPathResolver =
    { new PathResolver with
        member _.TryPrecompiledOutPath(_, _) = None
        member _.GetOrAddDeduplicateTargetDir(_, addTargetDir) = addTargetDir Set.empty }

let private withTempProject language (testFn: CrackerOptions -> unit) =
    let rootDir = IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    let projDir = IO.Path.Combine(rootDir, "proj")

    IO.Directory.CreateDirectory(projDir) |> ignore

    let projFile = IO.Path.Combine(projDir, "Test.fsproj")
    IO.File.WriteAllText(projFile, "<Project></Project>")

    let compilerOptions = CompilerOptionsHelper.Make(language = language)

    let cliArgs =
        {
            ProjectFile = projFile
            RootDir = projDir
            OutDir = None
            IsWatch = false
            Precompile = false
            PrecompiledLib = None
            PrintAst = false
            FableLibraryPath = None
            Configuration = "Debug"
            NoRestore = true
            NoCache = true
            NoParallelTypeCheck = false
            SourceMaps = false
            SourceMapsRoot = None
            Exclude = []
            Replace = Map.empty
            RunProcess = None
            CompilerOptions = compilerOptions
            Verbosity = Verbosity.Normal
        }

    let opts = CrackerOptions(cliArgs, evaluateOnly = true)

    try
        testFn opts
    finally
        if IO.Directory.Exists(rootDir) then
            try
                IO.Directory.Delete(rootDir, true)
            with _ ->
                // Best-effort cleanup; ignore IO errors from locked files.
                ()

let tests =
  testList "Cli" [

    testCase "Can use --outdir in lower case" <| fun () ->
        let res = parseCliArgs ["--outDir"; "foo"]
        Expect.isOk res "--outDir args"
        Expect.equal (res.Value.Value("--outDir")) (Some "foo") "--outDir value"

        let res = parseCliArgs ["--outdir"; "foo"]
        Expect.isOk res "--outdir args"
        Expect.equal (res.Value.Value("--outDir")) (Some "foo") "--outdir value"

    testCase "Cannot use --outir" <| fun () ->
        let res = parseCliArgs ["--outir"; "foo"]
        Expect.isError res "--outir args"

    testCase "Can parse swift language" <| fun () ->
        let res = parseCliArgs ["--lang"; "swift"]
        Expect.isOk res "--lang swift args"
        Expect.equal (argLanguage res.Value) (Ok Swift) "swift language"

    testCase "Can parse swift alias" <| fun () ->
        let res = parseCliArgs ["--lang"; "sw"]
        Expect.isOk res "--lang sw args"
        Expect.equal (argLanguage res.Value) (Ok Swift) "sw alias"

    testCase "Default file extension for swift" <| fun () ->
        Expect.equal (File.defaultFileExt false Swift) ".fs.swift" "swift extension without outDir"
        Expect.equal (File.defaultFileExt true Swift) ".swift" "swift extension with outDir"

    testCase "Unknown language preserves casing in error" <| fun () ->
        let res = parseCliArgs ["--lang"; "SwiftY"]
        Expect.isOk res "--lang invalid args"

        match argLanguage res.Value with
        | Ok _ -> failwith "Expected error for invalid language"
        | Error msg ->
            let expected = "'SwiftY' is not a valid language."
            Expect.isTrue (msg.Contains(expected, StringComparison.Ordinal)) "error message preserves casing"

    testCase "Swift library path uses .swift extension" <| fun () ->
        let libraryDir = Path.normalizePath (IO.Path.Combine("temp", "fable-library-swift"))
        let com = makeCompiler Swift libraryDir

        Expect.equal (getLibPath com "Fable.Core") (libraryDir + "/Fable.Core.swift") "swift lib path"

    testCase "Swift fable library path uses fable-library-swift" <| fun () ->
        withTempProject Swift (fun opts ->
            let expected =
                IO.Path.Combine(opts.FableModulesDir, "fable-library-swift")
                |> Path.normalizeFullPath

            Expect.equal (getFableLibraryPath opts false) expected "swift fable library path"
        )

    testCase "Swift compile path writes placeholder file" <| fun () ->
        let rootDir = IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        let outPath = IO.Path.Combine(rootDir, "Test.swift")

        let compiler = makeCompiler Swift "lib"
        let cliArgs = makeSwiftCliArgs "" rootDir (Some rootDir) false

        try
            Fable.Cli.Pipeline.Swift.compileFile compiler cliArgs defaultPathResolver false outPath
            |> Async.RunSynchronously

            Expect.isTrue (IO.File.Exists(outPath)) "placeholder file exists"

            let content = IO.File.ReadAllText(outPath)
            Expect.stringContains content "Swift backend is not implemented yet" "placeholder content"
            Expect.stringContains content compiler.CurrentFile "includes source path"
            Expect.stringContains content outPath "includes out path"
        finally
            if IO.Directory.Exists(rootDir) then
                IO.Directory.Delete(rootDir, true)

    testCase "Swift compile path emits source map when enabled" <| fun () ->
        let rootDir = IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        let projDir = IO.Path.Combine(rootDir, "proj")
        let projFile = IO.Path.Combine(projDir, "Test.fsproj")
        let outPath = IO.Path.Combine(rootDir, "Test.swift")

        IO.Directory.CreateDirectory(projDir) |> ignore
        IO.File.WriteAllText(projFile, "<Project></Project>")

        let compiler = makeCompiler Swift "lib"
        let cliArgs = makeSwiftCliArgs projFile projDir (Some rootDir) true

        try
            Fable.Cli.Pipeline.Swift.compileFile compiler cliArgs defaultPathResolver false outPath
            |> Async.RunSynchronously

            let mapFileName = IO.Path.GetFileName(outPath) + ".map"
            let encodedMapFileName = System.Uri.EscapeDataString(mapFileName)
            Expect.isTrue (IO.File.Exists(outPath + ".map")) "source map exists"

            let content = IO.File.ReadAllText(outPath)
            Expect.stringContains content $"//# sourceMappingURL={encodedMapFileName}" "source map link"
        finally
            if IO.Directory.Exists(rootDir) then
                IO.Directory.Delete(rootDir, true)

    testCase "Swift writer resolves import paths to outDir" <| fun () ->
        let rootDir = IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        let projDir = IO.Path.Combine(rootDir, "proj")
        let srcDir = IO.Path.Combine(projDir, "src")
        let outDir = IO.Path.Combine(rootDir, "out")
        let projFile = IO.Path.Combine(projDir, "Test.fsproj")
        let currentFile = IO.Path.Combine(srcDir, "Foo.fs")
        let outPath = IO.Path.Combine(outDir, "Foo.swift")

        IO.Directory.CreateDirectory(srcDir) |> ignore
        IO.Directory.CreateDirectory(outDir) |> ignore
        IO.File.WriteAllText(projFile, "<Project></Project>")
        IO.File.WriteAllText(currentFile, "module Foo")

        let compiler =
            makeCompilerWithFile Swift "lib" currentFile [| currentFile |] projFile

        let cliArgs = makeSwiftCliArgs projFile projDir (Some outDir) false

        try
            use writer =
                new Fable.Cli.Pipeline.Swift.SwiftWriter(
                    compiler,
                    cliArgs,
                    defaultPathResolver,
                    outPath,
                    System.Threading.CancellationToken.None
                )

            let resolved = (writer :> Printer.Writer).MakeImportPath("./Other.fs")

            Expect.equal resolved "./src/Other.swift" "resolves relative import path"
        finally
            if IO.Directory.Exists(rootDir) then
                IO.Directory.Delete(rootDir, true)

    testCase "Swift printer formats comments and blocks" <| fun () ->
        let mutable written = ""

        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftComment "headline"
                        SwiftStatementDecl(
                            SwiftBlock [ SwiftExpr(SwiftIdentifier "run()") ]
                        )
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "// headline"; "{"; "    run()"; "}"; "" ]

        Expect.equal written expected "renders comments and indented blocks"

    testCase "Swift printer formats simple function declarations" <| fun () ->
        let mutable written = ""
        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftFuncDecl
                            {
                                Name = "main"
                                Parameters = []
                                Body = [ SwiftExpr(SwiftIdentifier "run()") ]
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "func main()"; "{"; "    run()"; "}"; "" ]

        Expect.equal written expected "renders function with block"

    testCase "Swift printer formats return without expression" <| fun () ->
        let mutable written = ""
        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftFuncDecl
                            {
                                Name = "main"
                                Parameters = []
                                Body = [ SwiftReturn None ]
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "func main()"; "{"; "    return"; "}"; "" ]

        Expect.equal written expected "renders bare return"

    testCase "Swift printer formats imports and functions" <| fun () ->
        let mutable written = ""
        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftImport { Module = "Foundation" }
                        SwiftBinding
                            {
                                Name = "answer"
                                Expr = Some(SwiftLiteral "42")
                                IsMutable = false
                            }
                        SwiftBinding
                            {
                                Name = "counter"
                                Expr = None
                                IsMutable = true
                            }
                        SwiftBinding
                            {
                                Name = "nested"
                                Expr = Some(SwiftMemberAccess(SwiftIdentifier "foo", "bar"))
                                IsMutable = false
                            }
                        SwiftFuncDecl
                            {
                                Name = "main"
                                Parameters = [ "value"; "count" ]
                                Body =
                                    [
                                        SwiftExpr(SwiftIdentifier "run()")
                                        SwiftReturn(Some(SwiftIdentifier "value"))
                                    ]
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat
                Environment.NewLine
                [
                    "import Foundation"
                    "let answer = 42"
                    "var counter"
                    "let nested = foo.bar"
                    "func main(value, count)"
                    "{"
                    "    run()"
                    "    return value"
                    "}"
                    ""
                ]

        Expect.equal written expected "renders import, bindings, then function"

    testCase "Swift printer resolves path-like imports through writer" <| fun () ->
        let mutable written = ""

        let capture =
            { new Printer.Writer with
                member _.Write(str) =
                    written <- str
                    async.Return()

                member _.MakeImportPath(path) =
                    "resolved-" + path.Replace(".fs", ".swift")

                member _.AddSourceMapping(_, _, _, _, _, _) = ()
                member _.AddLog(_msg, _severity, ?range) = ()

              interface IDisposable with
                member _.Dispose() = () }

        let file =
            {
                Declarations =
                    [
                        SwiftImport { Module = "Foundation" }
                        SwiftImport { Module = "./Sources/App.fs" }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "import Foundation"; "import resolved-./Sources/App.swift"; "" ]

        Expect.equal written expected "resolves path imports while keeping module imports"

    testCase "Swift printer formats call expressions" <| fun () ->
        let mutable written = ""
        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftBinding
                            {
                                Name = "result"
                                Expr =
                                    Some(
                                        SwiftCall(
                                            SwiftIdentifier "foo",
                                            [
                                                SwiftIdentifier "bar"
                                                SwiftLiteral "1"
                                                SwiftStringLiteral "hi\nthere"
                                            ]
                                        )
                                    )
                                IsMutable = false
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat
                Environment.NewLine
                [ "let result = foo(bar, 1, \"hi\\nthere\")"; "" ]

        Expect.equal written expected "renders call expression"

    testCase "Swift printer escapes quotes and backslashes in string literals" <| fun () ->
        let mutable written = ""
        let capture = new InMemoryWriter(fun str -> written <- str) :> Printer.Writer

        let file =
            {
                Declarations =
                    [
                        SwiftBinding
                            {
                                Name = "text"
                                Expr = Some(SwiftStringLiteral "quote \" and \\ backslash")
                                IsMutable = false
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "let text = \"quote \\\" and \\\\ backslash\""; "" ]

        Expect.equal written expected "escapes quotes and backslashes"

    testCase "Swift transform uses bindings for zero-arg members" <| fun () ->
        let com = makeCompiler Swift "lib"

        let numberType =
            Fable.AST.Fable.Number(Fable.AST.NumberKind.Int32, Fable.AST.Fable.NumberInfo.Empty)

        let memberRef = Fable.AST.Fable.GeneratedMember.Value("answer", numberType, isInstance = false)

        let decl: Fable.AST.Fable.MemberDecl =
            {
                Name = "answer"
                Args = []
                Body =
                    Fable.AST.Fable.Value(
                        Fable.AST.Fable.NumberConstant(
                            Fable.AST.Fable.NumberValue.Int32 42,
                            Fable.AST.Fable.NumberInfo.Empty
                        ),
                        None
                    )
                MemberRef = memberRef
                IsMangled = false
                ImplementedSignatureRef = None
                UsedNames = Set.empty
                XmlDoc = None
                Tags = []
            }

        let file = Fable.AST.Fable.File([ Fable.AST.Fable.MemberDeclaration decl ])
        let swiftFile = Fable.Transforms.Swift.Fable2Swift.Compiler.transformFile com file

        match swiftFile.Declarations with
        | [ SwiftBinding binding ] ->
            Expect.equal binding.Name "answer" "binding name"
            Expect.equal binding.Expr (Some(SwiftLiteral "42")) "binding value"
        | declarations ->
            failtestf "Expected single SwiftBinding, got %A" declarations

    testCase "Swift transform maps toConsoleError to stderrPrint" <| fun () ->
        let com = makeCompiler Swift "lib"

        let expr =
            Fable.Transforms.Replacements.Util.Helper.LibCall(
                com,
                "String",
                "toConsoleError",
                Fable.AST.Fable.Unit,
                [ Fable.AST.Fable.Value(Fable.AST.Fable.StringConstant "oops", None) ]
            )

        let decl =
            Fable.AST.Fable.ActionDeclaration
                {
                    Body = expr
                    UsedNames = Set.empty
                }

        let file = Fable.AST.Fable.File([ decl ])
        let swiftFile = Fable.Transforms.Swift.Fable2Swift.Compiler.transformFile com file

        match swiftFile.Declarations with
        | SwiftImport importDecl
            :: SwiftFuncDecl funcDecl
            :: SwiftStatementDecl (SwiftExpr (SwiftCall(SwiftIdentifier name, _)))
            :: _ ->
            Expect.equal importDecl.Module "Foundation" "adds Foundation import"
            Expect.equal funcDecl.Name "stderrPrint" "adds stderr print helper"
            Expect.equal name "stderrPrint" "uses stderrPrint call"
        | declarations ->
            failtestf "Expected stderrPrint helper and call, got %A" declarations
  ]
