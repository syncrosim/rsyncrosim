# Backup a SsimLibrary

Backup a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md).
The backup folder can be defined in the SyncroSim User Interface, but is
by default at the same level as the SsimLibrary file, and is called
libraryName.backup.

## Usage

``` r
backup(ssimObject)

# S4 method for class 'character'
backup(ssimObject)

# S4 method for class 'SsimObject'
backup(ssimObject)
```

## Arguments

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

## Value

Invisibly returns `TRUE` upon success (i.e.successful backup) and
`FALSE` upon failure.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Back up data from the SsimLibrary
backup(myLibrary)
#> Backup complete.
# }
```
