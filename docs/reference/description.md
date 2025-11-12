# Description of SsimLibrary, Project or Scenario

Get or set the description of a `SsimLibrary`, `Project`, or `Scenario`.

## Usage

``` r
description(ssimObject)

description(ssimObject) <- value

# S4 method for class 'character'
description(ssimObject)

# S4 method for class 'SsimObject'
description(ssimObject)

# S4 method for class 'character'
description(ssimObject) <- value

# S4 method for class 'SsimObject'
description(ssimObject) <- value
```

## Arguments

- ssimObject:

  `SsimLibrary`, `Project`, `Scenario`, or `Folder` object

- value:

  character string specifying the new description

## Value

A character string: the description of the SsimObject

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")

# Retrieve the description of the SyncroSim Project
mydescription <- description(myProject)

# Set the description of the SyncroSim Project
description(myProject) <- "my description"
# }
```
