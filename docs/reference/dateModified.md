# Last date a SsimLibrary, Project, Scenario, or Folder was modified

The most recent modification date of a `SsimLibrary`, `Project`,
`Scenario` or `Folder`.

## Usage

``` r
dateModified(ssimObject)

# S4 method for class 'character'
dateModified(ssimObject)

# S4 method for class 'SsimLibrary'
dateModified(ssimObject)

# S4 method for class 'Project'
dateModified(ssimObject)

# S4 method for class 'Scenario'
dateModified(ssimObject)

# S4 method for class 'Folder'
dateModified(ssimObject)
```

## Arguments

- ssimObject:

  `SsimLibrary`, `Project`, `Scenario`, or `Folder` object

## Value

A character string: date and time of the most recent modification to the
SsimObject provided as input.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session and SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Check the last date of modification of the SsimLibrary
dateModified(myLibrary)
} # }
```
