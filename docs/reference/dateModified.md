# Last date a SsimLibrary, Project, Scenario, or Folder was modified

The most recent modification date of a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
or
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md).

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

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
  or
  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  object

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
