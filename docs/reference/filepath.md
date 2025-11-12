# Retrieves the path to a SyncroSim object on disk

Retrieves the path to a SyncroSim `Session`, `SsimLibrary`, `Project`,
`Scenario`, of `Folder` on disk.

## Usage

``` r
filepath(ssimObject)

# S4 method for class 'character'
filepath(ssimObject)

# S4 method for class 'Session'
filepath(ssimObject)

# S4 method for class 'SsimObject'
filepath(ssimObject)

# S4 method for class 'Folder'
filepath(ssimObject)
```

## Arguments

- ssimObject:

  `Session`, `Project`, `SsimLibrary`, or `Folder` object

## Value

A character string: the path to a SyncroSim object on disk.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session and SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Get the file path
myFilePath <- filepath(myLibrary)
# }
```
