# Retrieves the temporary file path to a SyncroSim object on disk

Retrieves the temporary file path to a SyncroSim `Session`,
`SsimLibrary`, `Project` or `Scenario` on disk.

## Usage

``` r
tempfilepath(ssimObject)

# S4 method for class 'character'
tempfilepath(ssimObject)

# S4 method for class 'Session'
tempfilepath(ssimObject)

# S4 method for class 'SsimObject'
tempfilepath(ssimObject)
```

## Arguments

- ssimObject:

  `Session`, `Project`, or `SsimLibrary` object

## Value

A character string: the temporary file path to a SyncroSim object on
disk.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session and SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
 
# Get the temporary file path
myFilePath <- tempfilepath(myLibrary)
# }
```
