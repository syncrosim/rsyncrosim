# Retrieves the path to a SyncroSim object on disk

Retrieves the path to a SyncroSim
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md),
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
of
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
on disk.

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

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  or
  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  object

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
