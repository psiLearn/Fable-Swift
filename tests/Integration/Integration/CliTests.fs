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

let private makeCompiler language libraryDir =
    let options = CompilerOptionsHelper.Make(language = language)

    { new Compiler with
        member _.LibraryDir = libraryDir
        member _.CurrentFile = ""
        member _.OutputDir = None
        member _.OutputType = OutputType.Library
        member _.ProjectFile = ""
        member _.ProjectOptions = invalidOp "Not implemented in test stub"
        member _.SourceFiles = [||]
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
        let cliArgs: CliArgs =
            {
                ProjectFile = ""
                RootDir = rootDir
                OutDir = Some rootDir
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
                CompilerOptions = CompilerOptionsHelper.Make(language = Swift)
                Verbosity = Verbosity.Normal
            }

        let pathResolver =
            { new PathResolver with
                member _.TryPrecompiledOutPath(_, _) = None
                member _.GetOrAddDeduplicateTargetDir(_, addTargetDir) = addTargetDir Set.empty }

        try
            Fable.Cli.Pipeline.Swift.compileFile compiler cliArgs pathResolver false outPath
            |> Async.RunSynchronously

            Expect.isTrue (IO.File.Exists(outPath)) "placeholder file exists"

            let content = IO.File.ReadAllText(outPath)
            Expect.stringContains content "Swift backend is not implemented yet" "placeholder content"
            Expect.stringContains content compiler.CurrentFile "includes source path"
            Expect.stringContains content outPath "includes out path"
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
                                Body = [ SwiftExpr(SwiftIdentifier "run()") ]
                            }
                    ]
            }

        SwiftPrinter.run capture file |> Async.RunSynchronously

        let expected =
            String.concat Environment.NewLine [ "func main()"; "{"; "    run()"; "}"; "" ]

        Expect.equal written expected "renders function with block"
  ]
