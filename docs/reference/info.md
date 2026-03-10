# Retrieves information about a library

Retrieves some basic metadata about a SsimLibrary: Name, Owner, Last
Modified, Size, Read Only, Data files, Publish files, Temporary files,
Backup files, and Use conda.

## Usage

``` r
info(ssimLibrary)

# S4 method for class 'SsimLibrary'
info(ssimLibrary)
```

## Arguments

- ssimLibrary:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

## Value

Returns a `data.frame` with information on the properties of the
SsimLibrary object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session and SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Get information about SsimLibrary  
info(myLibrary)
} # }
```
