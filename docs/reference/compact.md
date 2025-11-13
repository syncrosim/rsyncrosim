# Compacts a SsimLibrary

Compact a `SsimLibrary`. Removes extraneous data from the SyncroSim
library.

## Usage

``` r
compact(ssimLibrary)

# S4 method for class 'character'
compact(ssimLibrary)

# S4 method for class 'SsimLibrary'
compact(ssimLibrary)
```

## Arguments

- ssimLibrary:

  `SsimLibrary` object

## Value

Invisibly returns `TRUE` upon success (i.e.successful compacting) and
`FALSE` upon failure.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Compact data from the SsimLibrary
compact(myLibrary)
#> Compacting library...Library successfully compacted.
# }
```
