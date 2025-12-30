# Local Fable packages

Use this repo to build local NuGet packages and install the Fable CLI tool in another project.

## Build packages (PowerShell)

From a PowerShell prompt opened at the repo root:

```powershell
dotnet run --project src\Fable.Build\Fable.Build.fsproj -- package
```

Packages are written to `temp\packages`. For subsequent runs you can skip rebuilding the libraries:

```powershell
dotnet run --project src\Fable.Build\Fable.Build.fsproj -- package --skip-fable-library
```

## Capture the package versions

```powershell
$repo = (Resolve-Path .).Path
$packagesDir = Join-Path $repo "temp\packages"

$cliVersion =
    (Get-ChildItem $packagesDir -Filter "Fable.[0-9]*.nupkg"
     | Sort-Object LastWriteTime -Descending
     | Select-Object -First 1).BaseName -replace '^Fable\.', ''

$compilerVersion =
    (Get-ChildItem $packagesDir -Filter "Fable.Compiler.*.nupkg"
     | Sort-Object LastWriteTime -Descending
     | Select-Object -First 1).BaseName -replace '^Fable\.Compiler\.', ''

$coreVersion =
    (Get-ChildItem $packagesDir -Filter "Fable.Core.*.nupkg"
     | Sort-Object LastWriteTime -Descending
     | Select-Object -First 1).BaseName -replace '^Fable\.Core\.', ''
```

Example versions from the current packages in this repo (use the variables above after you build locally):

```powershell
$cliVersion = "5.0.0-alpha.21"
$compilerVersion = "5.0.0-alpha.20"
$coreVersion = "5.0.0-beta.4"
```

Local builds may also produce versions like `5.999.0-local-build-YYYYMMDD-HHmmss`, so always use the detected versions above.

## Install in another project as a local tool

```powershell
Set-Location C:\path\to\example-project
dotnet new tool-manifest
dotnet tool install fable --version $cliVersion --add-source $packagesDir
```

If the tool is already installed:

```powershell
dotnet tool update fable --version $cliVersion --add-source $packagesDir
```

## Use it

```powershell
dotnet fable --help
dotnet fable . -o build
```

## Use the matching libraries in the example project

```powershell
dotnet add package Fable.Core --version $coreVersion --source $packagesDir
dotnet add package Fable.Compiler --version $compilerVersion --source $packagesDir
```

## Refresh after new local builds

Re-run the package command, capture the new versions, then update the tool and packages.
