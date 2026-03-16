# Retrieves the temporary file path to a SyncroSim object on disk

Retrieves the temporary file path to a SyncroSim
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md),
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
or
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
on disk.

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

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  or
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

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
