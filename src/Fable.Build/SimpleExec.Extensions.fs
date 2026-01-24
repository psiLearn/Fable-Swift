module SimpleExec

open SimpleExec
open BlackFox.CommandLine
open Build.Utils
open System
open System.IO

let private shouldUseBuiltCli () =
    match Environment.GetEnvironmentVariable("FABLE_USE_BUILT_CLI") with
    | null
    | "" -> false
    | value ->
        match value.Trim().ToLowerInvariant() with
        | "1"
        | "true" -> true
        | _ -> false

let private tryGetBuiltCliPath (localFableDir: string) =
    let dllPath = Path.Combine(localFableDir, "bin", "Release", "net10.0", "fable.dll")

    if File.Exists dllPath then
        Some dllPath
    else
        None

type Command with

    static member Fable(args: CmdLine, ?workingDirectory: string, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let args =
            if shouldUseBuiltCli () then
                match tryGetBuiltCliPath localFableDir with
                | Some dllPath ->
                    CmdLine.concat [ CmdLine.empty |> CmdLine.appendRaw "exec" |> CmdLine.appendRaw dllPath; args ]
                    |> CmdLine.toString
                | None ->
                    CmdLine.concat
                        [
                            CmdLine.empty
                            |> CmdLine.appendRaw "run"
                            |> CmdLine.appendPrefix "-c" "Release"
                            |> CmdLine.appendPrefix "--project" localFableDir
                            |> CmdLine.appendRaw "--"
                            args
                        ]
                    |> CmdLine.toString
            else
                CmdLine.concat
                    [
                        CmdLine.empty
                        |> CmdLine.appendRaw "run"
                        |> CmdLine.appendPrefix "-c" "Release"
                        |> CmdLine.appendPrefix "--project" localFableDir
                        |> CmdLine.appendRaw "--"
                        args
                    ]
                |> CmdLine.toString

        Command.Run("dotnet", args, ?workingDirectory = workingDirectory, ?noEcho = noEcho, ?echoPrefix = echoPrefix)

    static member Fable(?argsBuilder: CmdLine -> CmdLine, ?workingDirectory: string, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder = defaultArg argsBuilder id

        let args =
            if shouldUseBuiltCli () then
                match tryGetBuiltCliPath localFableDir with
                | Some dllPath ->
                    CmdLine.empty
                    |> CmdLine.appendRaw "exec"
                    |> CmdLine.appendRaw dllPath
                    |> argsBuilder
                    |> CmdLine.toString
                | None ->
                    CmdLine.empty
                    |> CmdLine.appendRaw "run"
                    |> CmdLine.appendPrefix "-c" "Release"
                    |> CmdLine.appendPrefix "--project" localFableDir
                    |> CmdLine.appendRaw "--"
                    |> argsBuilder
                    |> CmdLine.toString
            else
                CmdLine.empty
                |> CmdLine.appendRaw "run"
                |> CmdLine.appendPrefix "-c" "Release"
                |> CmdLine.appendPrefix "--project" localFableDir
                |> CmdLine.appendRaw "--"
                |> argsBuilder
                |> CmdLine.toString

        Command.Run("dotnet", args, ?workingDirectory = workingDirectory, ?noEcho = noEcho, ?echoPrefix = echoPrefix)

    static member FableAsync(argsBuilder: CmdLine -> CmdLine, ?workingDirectory, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder =
            CmdLine.empty
            |> CmdLine.appendRaw "run"
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "--"
            |> argsBuilder
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            argsBuilder,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member WatchFableAsync(argsBuilder: CmdLine -> CmdLine, ?workingDirectory, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder =
            CmdLine.empty
            |> CmdLine.appendRaw "watch"
            |> CmdLine.appendRaw "--no-hot-reload"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "run"
            // Without the release mode, Fable stack overflow when compiling the tests
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendRaw "--"
            |> argsBuilder
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            argsBuilder,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member WatchFableAsync(args: CmdLine, ?workingDirectory, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let args =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw "watch"
                    |> CmdLine.appendRaw "--no-hot-reload"
                    |> CmdLine.appendPrefix "--project" localFableDir
                    |> CmdLine.appendRaw "run"
                    // Without the release mode, Fable stack overflow when compiling the tests
                    |> CmdLine.appendPrefix "-c" "Release"
                    |> CmdLine.appendRaw "--"

                    args
                ]
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            args,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member WatchFable(args: CmdLine, ?workingDirectory, ?noEcho, ?echoPrefix) =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let args =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw "watch"
                    |> CmdLine.appendRaw "--no-hot-reload"
                    |> CmdLine.appendPrefix "--project" localFableDir
                    |> CmdLine.appendRaw "run"
                    // Without the release mode, Fable stack overflow when compiling the tests
                    |> CmdLine.appendPrefix "-c" "Release"
                    |> CmdLine.appendRaw "--"

                    args
                ]
            |> CmdLine.toString

        Command.Run("dotnet", args, ?workingDirectory = workingDirectory, ?noEcho = noEcho, ?echoPrefix = echoPrefix)
